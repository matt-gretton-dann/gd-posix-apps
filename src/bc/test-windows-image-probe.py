#!/usr/bin/env python3
"""Check module-API bounds, capability reporting and owned-child cleanup."""
import argparse
import ctypes
import importlib.util
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest import mock

SPEC = importlib.util.spec_from_file_location(
    'image_probe', Path(__file__).parent / 'scripts/windows-image-probe.py')
PROBE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(PROBE)
BC = None


class Function:
    def __init__(self, call):
        self.call = call

    def __call__(self, *args):
        return self.call(*args)


class FakeModules:
    """Mock call-contract tests; native integration below verifies Windows ABI."""
    def __init__(self):
        self.paths = {0x10000000: r'D:\system\other.dll', 0x20000000: r'D:\bin\bc-copy.exe'}
        self.required = None
        self.fail_enum = False
        self.truncated = False
        self.fail_name = False
        self.wrong_base = False
        self.zero_info = False
        self.fail_info = False
        self.K32EnumProcessModulesEx = Function(self.enumerate)
        self.K32GetModuleFileNameExW = Function(self.filename)
        self.K32GetModuleInformation = Function(self.information)
        self.EnumProcessModulesEx = self.K32EnumProcessModulesEx
        self.GetModuleFileNameExW = self.K32GetModuleFileNameExW
        self.GetModuleInformation = self.K32GetModuleInformation

    def enumerate(self, handle, modules, capacity, needed, flags):
        assert flags == 3 and capacity == ctypes.sizeof(modules)
        for index, address in enumerate(self.paths):
            modules[index] = address
        ctypes.cast(needed, ctypes.POINTER(ctypes.c_uint32)).contents.value = (
            self.required if self.required is not None else
            len(self.paths) * ctypes.sizeof(ctypes.c_void_p))
        return 0 if self.fail_enum else 1

    def filename(self, handle, module, output, capacity):
        if self.fail_name:
            return 0
        if self.truncated:
            return capacity - 1
        output.value = self.paths[module]
        return len(output.value)

    def information(self, handle, module, output, size):
        info = ctypes.cast(output, ctypes.POINTER(self.api.info_type)).contents
        info.base = 0 if self.zero_info else module + (4096 if self.wrong_base else 0)
        info.image_bytes = 0x23000
        return 0 if self.fail_info else 1

    def api_instance(self):
        with mock.patch.object(ctypes, 'WinDLL', return_value=self, create=True):
            self.api = PROBE.WindowsModules()
        return self.api


class ModuleTests(unittest.TestCase):
    def test_canonical_windows_paths(self):
        for path in (r'D:\bin\bc-copy.exe', 'd:/BIN/bc-copy.exe',
                     r'\\?\D:\bin\bc-copy.exe'):
            self.assertEqual(PROBE.canonical_windows_path(path), r'd:\bin\bc-copy.exe')
        self.assertEqual(PROBE.canonical_windows_path(r'\\?\UNC\Server\share\bc.exe'),
                         r'\\server\share\bc.exe')

    def test_selects_matching_main_module_instead_of_first_dll(self):
        fake = FakeModules()
        result = fake.api_instance().snapshot(123, 'd:/BIN/bc-copy.exe')
        self.assertEqual(result['status'], 'collected')
        self.assertEqual(result['module_base'], '0x0000000020000000')
        self.assertEqual(result['image_bytes'], 0x23000)
        self.assertTrue(result['main_module_path_matches'])
        self.assertEqual(result['modules_observed'], 2)

    def test_null_module_handle_cannot_identify_the_executable(self):
        fake = FakeModules()
        fake.paths = {0: r'D:\bin\bc-copy.exe', **fake.paths}
        result = fake.api_instance().snapshot(123, r'D:\bin\bc-copy.exe')
        self.assertEqual(result['module_base'], '0x0000000020000000')

    def test_psapi_fallback(self):
        fake = FakeModules()
        with mock.patch.object(ctypes, 'WinDLL', side_effect=[OSError('missing'), fake], create=True):
            fake.api = PROBE.WindowsModules()
        self.assertEqual(fake.api.snapshot(123, r'D:\bin\bc-copy.exe')['status'], 'collected')

    def test_missing_native_apis_are_unavailable(self):
        with mock.patch.object(ctypes, 'WinDLL', side_effect=OSError('missing'), create=True):
            with self.assertRaises(PROBE.ModuleUnavailable):
                PROBE.WindowsModules()

    def test_module_list_bounds_and_alignment(self):
        for required in (8193, 65536):
            fake = FakeModules()
            fake.required = required
            with self.assertRaises(PROBE.ModuleUnavailable) as error:
                fake.api_instance().snapshot(123, r'D:\bin\bc-copy.exe')
            self.assertFalse(error.exception.retryable)

    def test_empty_or_different_module_list_is_retryable(self):
        for required in (None, 0):
            fake = FakeModules()
            fake.required = required
            with self.assertRaises(PROBE.ModuleUnavailable) as error:
                fake.api_instance().snapshot(123, r'D:\not-the-main.exe')
            self.assertTrue(error.exception.retryable)

    def test_initialization_race_and_access_denial(self):
        for code, retryable in ((299, True), (87, True), (5, False)):
            fake = FakeModules()
            fake.fail_enum = True
            with mock.patch.object(PROBE, 'last_error', return_value=code):
                with self.assertRaises(PROBE.ModuleUnavailable) as error:
                    fake.api_instance().snapshot(123, r'D:\bin\bc-copy.exe')
            self.assertEqual(error.exception.code, code)
            self.assertEqual(error.exception.retryable, retryable)

    def test_truncated_paths_do_not_establish_identity(self):
        fake = FakeModules()
        fake.truncated = True
        with self.assertRaises(PROBE.ModuleUnavailable):
            fake.api_instance().snapshot(123, r'D:\bin\bc-copy.exe')

    def test_path_access_denial_is_a_capability_result(self):
        fake = FakeModules()
        fake.fail_name = True
        with mock.patch.object(PROBE, 'last_error', return_value=5):
            with self.assertRaises(PROBE.ModuleUnavailable) as error:
                fake.api_instance().snapshot(123, r'D:\bin\bc-copy.exe')
        self.assertFalse(error.exception.retryable)
        self.assertEqual(error.exception.code, 5)

    def test_inconsistent_base_is_not_reported_as_a_measurement(self):
        fake = FakeModules()
        fake.wrong_base = True
        with self.assertRaises(PROBE.ModuleUnavailable) as error:
            fake.api_instance().snapshot(123, r'D:\bin\bc-copy.exe')
        self.assertTrue(error.exception.retryable)

    def test_invalid_main_module_information(self):
        for attribute in ('zero_info', 'fail_info'):
            fake = FakeModules()
            setattr(fake, attribute, True)
            with self.assertRaises(PROBE.ModuleUnavailable):
                fake.api_instance().snapshot(123, r'D:\bin\bc-copy.exe')


class LaunchTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.exe = Path(self.temp.name) / 'bc.exe'
        self.exe.write_bytes(b'immutable fixture')
        self.child = mock.Mock()
        self.child._handle = 123
        self.child.returncode = 0
        self.child.poll.return_value = 0
        self.child.communicate.return_value = (b'2\r\n', b'')
        self.api = mock.Mock()
        self.api.snapshot.return_value = dict(status='collected', module_base='0x20000000')

    def run_probe(self, **kwargs):
        with mock.patch.object(PROBE, 'native_windows_64', return_value=True), \
                mock.patch.object(PROBE, 'WindowsModules', return_value=self.api), \
                mock.patch.object(PROBE.subprocess, 'Popen', return_value=self.child):
            return PROBE.probe(self.exe, **kwargs)

    def test_generic_capability_fallback_does_not_spawn(self):
        with mock.patch.object(PROBE, 'native_windows_64', return_value=False), \
                mock.patch.object(PROBE.subprocess, 'Popen') as spawn:
            result = PROBE.probe(self.exe)
        spawn.assert_not_called()
        self.assertEqual(result['status'], 'unavailable')
        self.assertFalse(result['checked'])

    def test_checked_launch_and_unchanged_binary(self):
        result = self.run_probe()
        self.assertTrue(result['checked'])
        self.assertEqual(result['binary_sha256'], PROBE.digest(self.exe))
        self.child.communicate.assert_called_once()
        self.assertEqual(self.child.communicate.call_args.args, (PROBE.PROGRAM,))
        self.assertGreater(self.child.communicate.call_args.kwargs['timeout'], 0)
        self.assertLessEqual(self.child.communicate.call_args.kwargs['timeout'], 120)
        self.child.kill.assert_not_called()

    def test_missing_process_handle_retains_correctness_check(self):
        del self.child._handle
        result = self.run_probe()
        self.assertEqual(result['status'], 'unavailable')
        self.assertTrue(result['checked'])

    def test_retry_then_match(self):
        self.child.poll.return_value = None
        self.api.snapshot.side_effect = [PROBE.ModuleUnavailable('initializing', 299, True),
                                         dict(status='collected')]
        self.child.communicate.side_effect = lambda *a, **kw: (
            setattr(self.child, 'poll', mock.Mock(return_value=0)) or (b'2\n', b''))
        with mock.patch.object(PROBE.time, 'sleep'):
            result = self.run_probe()
        self.assertEqual(result['attempts'], 2)
        self.assertTrue(result['checked'])

    def test_probe_retry_deadline_is_bounded(self):
        self.api.snapshot.side_effect = PROBE.ModuleUnavailable('initializing', 299, True)
        with mock.patch.object(PROBE.time, 'monotonic', side_effect=[0, 0, 3, 3]):
            result = self.run_probe(probe_timeout=2)
        self.assertEqual(result['status'], 'unavailable')
        self.assertEqual(result['attempts'], 1)
        self.assertTrue(result['checked'])

    def test_incorrect_output_or_exit_is_a_hard_failure(self):
        for stdout, stderr, code in ((b'3\n', b'', 0), (b'2\n', b'error', 0), (b'2\n', b'', 1)):
            self.child.communicate.return_value = stdout, stderr
            self.child.returncode = code
            with self.assertRaises(PROBE.ProbeError):
                self.run_probe()

    def test_timeout_kills_and_reaps_owned_child(self):
        self.child.poll.return_value = None
        self.child.communicate.side_effect = [subprocess.TimeoutExpired('bc', 1), (b'', b'')]
        with self.assertRaises(PROBE.ProbeError):
            self.run_probe(timeout=1)
        self.child.kill.assert_called_once()
        self.assertEqual(self.child.communicate.call_count, 2)

    def test_unexpected_api_failure_still_cleans_up(self):
        self.child.poll.return_value = None
        self.api.snapshot.side_effect = RuntimeError('unexpected')
        with self.assertRaises(RuntimeError):
            self.run_probe()
        self.child.kill.assert_called_once()
        self.child.communicate.assert_called_once()

    def test_changed_executable_is_a_hard_failure(self):
        with mock.patch.object(PROBE, 'digest', side_effect=['before', 'after']):
            with self.assertRaises(PROBE.ProbeError):
                self.run_probe()

    def test_environment_drops_runtime_override_and_profile_variables(self):
        with mock.patch.dict(os.environ, {'BC_ENV_ARGS': 'unexpected', 'DC_ENV_ARGS': 'unexpected',
                                         'GD_BC_PROFILE_OUTPUT': 'unexpected'}, clear=True):
            result = PROBE.clean_environment()
        self.assertEqual(result, {'LC_ALL': 'C', 'LANG': 'C'})

    def test_deadlines_must_be_positive(self):
        for value in (0, -1, float('nan'), float('inf'), True):
            for key in ('timeout', 'probe_timeout'):
                with self.assertRaises(ValueError):
                    PROBE.probe(self.exe, **{key: value})


@unittest.skipUnless(os.name == 'nt' and ctypes.sizeof(ctypes.c_void_p) == 8,
                     'native 64-bit Windows module API integration')
class NativeTests(unittest.TestCase):
    def test_actual_bc_copy_with_unicode_path(self):
        if BC is None:
            self.skipTest('--bc is required for native integration')
        with tempfile.TemporaryDirectory() as directory:
            target = Path(directory) / 'bc-copy-\u03bb.exe'
            shutil.copyfile(BC, target)
            for _ in range(3):
                result = PROBE.probe(target)
                self.assertEqual(result['status'], 'collected', result)
                self.assertTrue(result['checked'])
                self.assertTrue(result['main_module_path_matches'])
                self.assertEqual(result['binary_sha256'], PROBE.digest(BC))
                self.assertRegex(result['module_base'], r'^0x[0-9a-f]{16}$')
                self.assertGreater(result['image_bytes'], 0)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--bc', type=Path)
    args, remaining = parser.parse_known_args()
    BC = args.bc
    unittest.main(argv=[__file__, *remaining])
