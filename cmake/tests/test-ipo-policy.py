#!/usr/bin/env python3
"""Check explicit IPO policy without depending on host LTO availability.
SPDX-License-Identifier: Apache-2.0
"""
import argparse
import json
from pathlib import Path
import subprocess
import tempfile
import unittest


class IpoPolicy(unittest.TestCase):
    def configure(self, mode=None, build_type='Release', supported=True, inherited=None):
        with tempfile.TemporaryDirectory() as directory:
            source = Path(directory)
            source.joinpath('CMakeLists.txt').write_text('''
cmake_minimum_required(VERSION 3.15)
project(IpoPolicyFixture LANGUAGES NONE)
include("''' + MODULE.as_posix() + '''")
# Replace only capability detection. The production policy/application run
# unchanged, including cache defaults and target property inheritance.
set_property(GLOBAL PROPERTY probe_calls 0)
function(check_ipo_supported)
  cmake_parse_arguments(probe "" "RESULT;OUTPUT" "LANGUAGES" ${ARGN})
  if(NOT "${probe_LANGUAGES}" STREQUAL "C;CXX")
    message(FATAL_ERROR "Expected a joint C/CXX capability probe")
  endif()
  get_property(calls GLOBAL PROPERTY probe_calls)
  math(EXPR calls "${calls}+1")
  set_property(GLOBAL PROPERTY probe_calls "${calls}")
  set("${probe_RESULT}" "${MOCK_SUPPORTED}" PARENT_SCOPE)
  set("${probe_OUTPUT}" "deliberately unsupported probe" PARENT_SCOPE)
endfunction()
gd_check_ipo()
add_library(subject INTERFACE)
if(DEFINED MOCK_INITIAL_IPO)
  # INTERFACE fixtures have no compiler and do not inherit this compile
  # property. Seed the setting a compiled target would inherit at creation.
  set_property(TARGET subject PROPERTY INTERPROCEDURAL_OPTIMIZATION "${MOCK_INITIAL_IPO}")
endif()
gd_apply_ipo(subject)
get_property(target_ipo TARGET subject PROPERTY INTERPROCEDURAL_OPTIMIZATION)
get_property(calls GLOBAL PROPERTY probe_calls)
file(WRITE "${CMAKE_BINARY_DIR}/policy.json"
  "{\\"enabled\\":\\"${ENABLE_IPO}\\",\\"target_ipo\\":\\"${target_ipo}\\",\\"calls\\":${calls}}")
''', encoding='utf-8')
            command = [CMAKE, '-S', str(source), '-B', str(source / 'build'),
                       '-DCMAKE_BUILD_TYPE=' + build_type,
                       '-DMOCK_SUPPORTED=' + ('ON' if supported else 'OFF')]
            if mode is not None:
                command.append('-DIPO_MODE=' + mode)
            if inherited is not None:
                command.append('-DCMAKE_INTERPROCEDURAL_OPTIMIZATION=' + inherited)
                command.append('-DMOCK_INITIAL_IPO=' + inherited)
            result = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                    universal_newlines=True, timeout=30)
            report = source / 'build/policy.json'
            return result, json.loads(report.read_text()) if report.exists() else None

    def check(self, expected, **kwargs):
        result, report = self.configure(**kwargs)
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertEqual(report, expected)

    def test_default_supported_release_enables_ipo(self):
        self.check(dict(enabled='ON', target_ipo='TRUE', calls=1))

    def test_default_unsupported_release_keeps_generic_build(self):
        self.check(dict(enabled='OFF', target_ipo='', calls=1), supported=False)

    def test_default_debug_skips_capability_probe(self):
        self.check(dict(enabled='OFF', target_ipo='', calls=0), build_type='Debug')

    def test_default_without_single_build_type_preserves_probe(self):
        self.check(dict(enabled='ON', target_ipo='TRUE', calls=1), build_type='')

    def test_off_skips_probe_for_supported_and_unsupported_hosts(self):
        for supported in (True, False):
            for build_type in ('Release', 'Debug'):
                with self.subTest(supported=supported, build_type=build_type):
                    self.check(dict(enabled='OFF', target_ipo='FALSE', calls=0),
                               mode='OFF', supported=supported, build_type=build_type)

    def test_explicit_off_overrides_inherited_target_default(self):
        self.check(dict(enabled='OFF', target_ipo='FALSE', calls=0), mode='OFF', inherited='ON')

    def test_auto_retains_existing_inherited_property_behavior(self):
        self.check(dict(enabled='OFF', target_ipo='ON', calls=0),
                   mode='AUTO', build_type='Debug', inherited='ON')

    def test_required_supported_release_and_debug_enable_ipo(self):
        for build_type in ('Release', 'Debug'):
            with self.subTest(build_type=build_type):
                self.check(dict(enabled='ON', target_ipo='TRUE', calls=1),
                           mode='REQUIRED', build_type=build_type)

    def test_required_unsupported_fails_without_fallback(self):
        result, report = self.configure(mode='REQUIRED', supported=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIsNone(report)
        self.assertIn('IPO_MODE=REQUIRED but IPO is unsupported', result.stdout)
        self.assertIn('deliberately unsupported probe', result.stdout)

    def test_unknown_or_misspelled_mode_is_rejected(self):
        for mode in ('off', 'ON', 'THIN', ''):
            with self.subTest(mode=mode):
                result, report = self.configure(mode=mode)
                self.assertNotEqual(result.returncode, 0)
                self.assertIsNone(report)
                self.assertIn('Unsupported IPO_MODE', result.stdout)

    def test_failed_reconfiguration_removes_stale_policy_report(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            script = root / 'reconfigure.cmake'
            script.write_text('''
cmake_minimum_required(VERSION 3.15)
set(CMAKE_BINARY_DIR "''' + root.as_posix() + '''")
include("''' + MODULE.as_posix() + '''")
function(check_ipo_supported)
  cmake_parse_arguments(probe "" "RESULT;OUTPUT" "LANGUAGES" ${ARGN})
  set("${probe_RESULT}" "${MOCK_SUPPORTED}" PARENT_SCOPE)
  set("${probe_OUTPUT}" "deliberately unsupported probe" PARENT_SCOPE)
endfunction()
set(IPO_MODE AUTO)
set(MOCK_SUPPORTED ON)
gd_check_ipo()
if(NOT EXISTS "${CMAKE_BINARY_DIR}/gd-ipo-policy.json")
  message(FATAL_ERROR "Initial successful policy report missing")
endif()
set(IPO_MODE REQUIRED)
set(MOCK_SUPPORTED OFF)
gd_check_ipo()
''', encoding='utf-8')
            result = subprocess.run([CMAKE, '-P', str(script)], stdout=subprocess.PIPE,
                                    stderr=subprocess.STDOUT, universal_newlines=True, timeout=30)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn('IPO_MODE=REQUIRED but IPO is unsupported', result.stdout)
            self.assertFalse((root / 'gd-ipo-policy.json').exists())


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--cmake', required=True)
    parser.add_argument('--module', type=Path, required=True)
    args, remaining = parser.parse_known_args()
    CMAKE, MODULE = args.cmake, args.module.resolve()
    unittest.main(argv=[__file__, *remaining])
