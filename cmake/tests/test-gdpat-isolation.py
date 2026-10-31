#!/usr/bin/env python3
"""Prove shared imported sources cannot share writable outputs between builds."""

import argparse
from concurrent.futures import ThreadPoolExecutor
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


class ImportedIsolation(unittest.TestCase):
    def test_two_builds_keep_outputs_independent(self):
        with tempfile.TemporaryDirectory(prefix="gdpat-isolation-") as temporary:
            root = Path(temporary)
            dependency = root / "dependency"
            dependency.mkdir()
            (dependency / "CMakeLists.txt").write_text(
                "cmake_minimum_required(VERSION 3.15)\nproject(mock LANGUAGES NONE)\n")
            (dependency / "run-tests.py").write_text('''import os
from pathlib import Path
import sys
if "--list-tests" in sys.argv:
    print("dummy/shared-output")
else:
    output = Path(__file__).resolve().parent / "shared-output.txt"
    marker = os.environ["GDPAT_ISOLATION_MARKER"]
    output.write_text(marker)
    print("READY", flush=True)
    sys.stdin.readline()
    actual = output.read_text()
    if actual != marker:
        print("expected marker " + marker + ", found " + actual, file=sys.stderr)
        sys.exit(1)
''')
            project = root / "project"
            project.mkdir()
            module = str(Path(ARGS.module).resolve()).replace("\\", "/")
            (project / "CMakeLists.txt").write_text('''cmake_minimum_required(VERSION 3.15)
project(isolation LANGUAGES NONE)
include(CTest)
include(FetchContent)
set(ALLOW_THIRD_PARTY_DOWNLOADS ON)
include("''' + module + '''")
add_executable(dummy IMPORTED)
set_property(TARGET dummy PROPERTY IMPORTED_LOCATION "${CMAKE_COMMAND}")
add_gdpat_test(NAME dummy)
''')
            commands = []
            for name in ("first", "second"):
                build = root / name
                configure = subprocess.run(
                    [ARGS.cmake, "-S", str(project), "-B", str(build), "-G", ARGS.generator,
                     "-DFETCHCONTENT_SOURCE_DIR_GDPAT=" + str(dependency),
                     "-DPython_EXECUTABLE=" + sys.executable],
                    stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                    universal_newlines=True, timeout=30)
                self.assertEqual(configure.returncode, 0, configure.stdout)
                listing = subprocess.run(
                    [ARGS.ctest, "-C", "Debug", "--show-only=json-v1"], cwd=str(build),
                    stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                    universal_newlines=True, timeout=10)
                self.assertEqual(listing.returncode, 0, listing.stderr)
                selected = [test["command"] for test in json.loads(listing.stdout)["tests"]
                            if test["name"] == "dummy/shared-output"]
                self.assertEqual(len(selected), 1)
                commands.append(selected[0])

            # Readiness occurs after each write. Neither child may read until
            # both writes finish, making accidental shared storage fail reliably.
            children = []
            with ThreadPoolExecutor(max_workers=2) as readers:
                try:
                    for command, marker in zip(commands, ("A", "B")):
                        child = subprocess.Popen(
                            command, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, universal_newlines=True,
                            env=dict(os.environ, GDPAT_ISOLATION_MARKER=marker))
                        children.append(child)
                        ready = readers.submit(child.stdout.readline)
                        self.assertEqual(ready.result(timeout=5), "READY\n")
                    for child in children:
                        output, errors = child.communicate("go\n", timeout=5)
                        self.assertEqual(child.returncode, 0, errors)
                        self.assertEqual(output, "")
                        self.assertEqual(errors, "")
                finally:
                    for child in children:
                        if child.poll() is None:
                            child.kill()
                        child.communicate(timeout=5)
            self.assertFalse((dependency / "shared-output.txt").exists(),
                             "the imported source tree must remain read-only")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cmake", required=True)
    parser.add_argument("--ctest", required=True)
    parser.add_argument("--module", required=True)
    parser.add_argument("--generator", required=True)
    ARGS, remaining = parser.parse_known_args()
    unittest.main(argv=[__file__, *remaining])
