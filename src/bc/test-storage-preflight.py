"""Check native-preflight shell arguments without configuring or running bc."""
# SPDX-License-Identifier: Apache-2.0
import os
from pathlib import Path
import re
import subprocess
import textwrap
import unittest


@unittest.skipIf(os.name == "nt", "Host Bash fixture uses POSIX paths; native workflow covers Windows")
class PreflightOptions(unittest.TestCase):
    def invoke(self, sanitizer):
        workflow = Path(__file__).resolve().parents[2] / ".github/workflows/bc-compact-digit-correctness.yml"
        lines = workflow.read_text().splitlines()
        start = next(i for i, line in enumerate(lines) if line.strip().startswith("sanitizer_options="))
        end = next(i for i in range(start, len(lines)) if "tee evidence/configure.log" in lines[i])
        script = textwrap.dedent("\n".join(lines[start:end + 1]))
        script = script.rsplit(" 2>&1 | tee evidence/configure.log", 1)[0]
        modes = dict(sanitize="ON" if sanitizer else "OFF", compact="ON", prepared="ON",
                     typed="ON", profile="OFF", extensions="OFF")
        script = re.sub(r"\$\{\{ matrix\.mode\.(\w+) \}\}", lambda m: modes[m.group(1)], script)
        self.assertNotIn("${{", script)
        mock = r'cmake() { printf "%s\0" "CC=$CC" "CXX=$CXX" "$@"; }' + "\n"
        environment = dict(os.environ, CC="original-cc", CXX="original-cxx")
        result = subprocess.run(["/bin/bash", "-euo", "pipefail", "-c", mock + script],
                                env=environment, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                universal_newlines=True, timeout=5)
        self.assertEqual(result.returncode, 0, result.stderr)
        values = result.stdout.split("\0")
        self.assertEqual(values.pop(), "")
        self.assertEqual(values[:2], ["CC=clang", "CXX=clang++"] if sanitizer else
                         ["CC=original-cc", "CXX=original-cxx"])
        options = values[2:]
        self.assertEqual(options[:5], ["-S", ".", "-B", "build-native", "-G"])
        self.assertEqual(options.count("-DENABLE_BC_COMPACT_DIGITS=ON"), 1)
        self.assertIn("-DCMAKE_BUILD_TYPE=Release", options)
        self.assertIn("-DENABLE_BC_PREPARED_DIVISORS=ON", options)
        if sanitizer:
            self.assertIn("-DCMAKE_CXX_FLAGS=-fsanitize=address,undefined -fno-omit-frame-pointer", options)
            self.assertIn("-DCMAKE_EXE_LINKER_FLAGS=-fsanitize=address,undefined", options)
            self.assertIn("-DIPO_MODE=OFF", options)
        else:
            self.assertFalse(any("fsanitize" in value for value in options))

    def test_ordinary_keeps_flags_and_empty_array_is_safe_on_host_bash(self):
        self.invoke(False)

    def test_sanitizer_uses_explicit_clang_and_keeps_each_flag_together(self):
        self.invoke(True)


if __name__ == "__main__":
    unittest.main()
