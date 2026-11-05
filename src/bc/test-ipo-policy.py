#!/usr/bin/env python3
"""Check explicit IPO opt-out against inherited native CMake configurations."""
import argparse
import json
from pathlib import Path
import re
import subprocess
import tempfile
import unittest


class IpoPolicy(unittest.TestCase):
    def test_off_overrides_configuration_defaults_and_preserves_flags(self):
        for configuration in ('Release', 'Bench'):
            with self.subTest(configuration=configuration), tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary)
                module = ARGS.module.resolve().as_posix().replace('"', '\\"')
                project = '''cmake_minimum_required(VERSION 3.15)
project(ipo_policy_probe LANGUAGES CXX)
set(CMAKE_CONFIGURATION_TYPES "Debug;Release;Bench")
set(CMAKE_INTERPROCEDURAL_OPTIMIZATION ON)
set(CMAKE_INTERPROCEDURAL_OPTIMIZATION_RELEASE ON)
set(CMAKE_INTERPROCEDURAL_OPTIMIZATION_BENCH ON)
set(IPO_MODE OFF CACHE STRING "Policy under test" FORCE)
include("MODULE")
gd_check_ipo()
if(MSVC)
  string(APPEND CMAKE_CXX_FLAGS " /DGD_IPO_EXPLICIT_FLAG=1")
else()
  string(APPEND CMAKE_CXX_FLAGS " -DGD_IPO_EXPLICIT_FLAG=1")
endif()
add_library(probe STATIC probe.cc)
gd_apply_ipo(probe)
foreach(suffix "" _DEBUG _RELEASE _RELWITHDEBINFO _MINSIZEREL _BENCH)
  get_target_property(enabled probe "INTERPROCEDURAL_OPTIMIZATION${suffix}")
  if(enabled)
    message(FATAL_ERROR "IPO_MODE=OFF left ${suffix} IPO enabled")
  endif()
endforeach()
'''.replace('MODULE', module)
                (root / 'CMakeLists.txt').write_text(project, encoding='utf-8')
                (root / 'probe.cc').write_text(
                    '#ifndef GD_IPO_EXPLICIT_FLAG\n#error Caller flags were lost\n#endif\n'
                    'int probe() { return GD_IPO_EXPLICIT_FLAG; }\n', encoding='utf-8')
                build = root / 'build'
                command = [ARGS.cmake, '-S', str(root), '-B', str(build),
                           '-DCMAKE_BUILD_TYPE=' + configuration,
                           '-DCMAKE_EXPORT_COMPILE_COMMANDS=ON']
                if ARGS.generator:
                    command += ['-G', ARGS.generator]
                if ARGS.compiler:
                    command += ['-DCMAKE_CXX_COMPILER=' + ARGS.compiler]
                configured = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                            universal_newlines=True, timeout=90)
                self.assertEqual(configured.returncode, 0, configured.stdout)
                policy = json.loads((build / 'gd-ipo-policy.json').read_text(encoding='utf-8'))
                self.assertEqual(policy, dict(schema=1, mode='OFF', automatic_ipo=False))
                compiled = subprocess.run([ARGS.cmake, '--build', str(build), '--config', configuration,
                                           '--target', 'probe'], stdout=subprocess.PIPE,
                                          stderr=subprocess.STDOUT, universal_newlines=True, timeout=30)
                self.assertEqual(compiled.returncode, 0, compiled.stdout)
                inventory = build / 'compile_commands.json'
                if inventory.is_file():
                    rows = json.loads(inventory.read_text(encoding='utf-8'))
                    self.assertTrue(rows)
                    for row in rows:
                        command = ' '.join(row['arguments']) if 'arguments' in row else row['command']
                        self.assertIn('GD_IPO_EXPLICIT_FLAG=1', command)
                        self.assertIsNone(re.search(
                            r'''(?:^|[\s"'])(?:-flto(?:=[^\s"']+)?|/GL)(?=$|[\s"'])''',
                            command, re.IGNORECASE), command)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--module', type=Path, required=True)
    parser.add_argument('--cmake', default='cmake')
    parser.add_argument('--compiler')
    parser.add_argument('--generator')
    ARGS, remaining = parser.parse_known_args()
    unittest.main(argv=[__file__, *remaining])
