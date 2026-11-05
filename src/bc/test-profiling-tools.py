#!/usr/bin/env python3
"""Check profiling-output gates, native units and bounded diagnostic children."""
import argparse
import importlib.util
import os
import io
import contextlib
import ctypes
import hashlib
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest import mock


class ProfilingTools(unittest.TestCase):
    def setUp(self):
        spec = importlib.util.spec_from_file_location('bc_profile_tools', COLLECTOR)
        self.module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.module)
        self.bench = self.module.load_benchmark()

    def test_windows_accounting_preserves_pointer_and_counter_widths(self):
        api = mock.Mock()
        wide = ctypes.sizeof(ctypes.c_size_t) == 8
        handle_value = 0x123456789ABC if wide else 0x12345678
        peak = 8589934592 if wide else 1073741824
        def memory(handle, pointer, size):
            counters = pointer._obj
            self.assertEqual(handle, handle_value)
            self.assertEqual(counters.cb, size)
            self.assertEqual(size, 72 if wide else 40)
            self.assertEqual(type(counters).PeakWorkingSetSize.offset, 8)
            counters.PeakWorkingSetSize = peak
            counters.PeakPagefileUsage = peak + 1
            return 1
        def times(handle, created, exited, kernel, user):
            kernel._obj.low = 10000000
            user._obj.high, user._obj.low = 1, 5000000
            return 1
        api.K32GetProcessMemoryInfo.side_effect = memory
        api.GetProcessTimes.side_effect = times
        result = self.module.windows_process_fields(handle_value, api)
        self.assertEqual(result['peak_rss_native'], peak)
        self.assertEqual(result['peak_commit_charge_bytes'], peak + 1)
        self.assertIn('bytes', result['native_unit'])
        self.assertEqual(result['cpu_time']['system_seconds'], 1)
        self.assertEqual(result['cpu_time']['user_ticks_100ns'], (1 << 32) + 5000000)
        self.assertEqual(result['cpu_time']['user_seconds'], ((1 << 32) + 5000000) / 10000000)
        self.assertEqual(api.K32GetProcessMemoryInfo.argtypes[0], ctypes.c_void_p)

    def test_windows_api_failures_remain_explicit_and_independent(self):
        api = mock.Mock()
        api.K32GetProcessMemoryInfo.return_value = 0
        api.GetProcessTimes.return_value = 0
        with mock.patch.object(ctypes, 'get_last_error', return_value=5, create=True):
            result = self.module.windows_process_fields(123, api)
        self.assertEqual(result['status'], 'unavailable')
        self.assertEqual(result['winerror'], 5)
        self.assertEqual(result['cpu_time']['status'], 'unavailable')
        self.assertEqual(result['cpu_time']['winerror'], 5)
        self.assertNotIn('peak_rss_native', result)
        self.assertEqual(self.module.windows_process_fields(None)['status'], 'unavailable')
        self.assertEqual(self.module.windows_process_fields(1, object())['status'], 'unavailable')

    def test_windows_cpu_and_memory_availability_are_independent(self):
        for memory_ok, cpu_ok in ((True, False), (False, True)):
            api = mock.Mock()
            def memory(handle, pointer, size):
                pointer._obj.PeakWorkingSetSize = 123
                return int(memory_ok)
            api.K32GetProcessMemoryInfo.side_effect = memory
            api.GetProcessTimes.return_value = int(cpu_ok)
            with mock.patch.object(ctypes, 'get_last_error', return_value=5, create=True):
                result = self.module.windows_process_fields(1, api)
            self.assertEqual(result['status'], 'collected' if memory_ok else 'unavailable')
            self.assertEqual(result['cpu_time']['status'], 'collected' if cpu_ok else 'unavailable')
            if cpu_ok:
                self.assertEqual(result['cpu_time']['user_seconds'], 0)
                self.assertEqual(result['cpu_time']['system_seconds'], 0)

    def test_windows_reads_accounting_before_releasing_the_child_handle(self):
        child = mock.MagicMock()
        child._handle, child.returncode = 123, 0
        child.communicate.return_value = (b'2\n', b'')
        with mock.patch.object(self.module.subprocess, 'Popen') as popen, \
                mock.patch.object(self.module, 'windows_process_fields') as fields:
            popen.return_value.__enter__.return_value = child
            def read(handle):
                self.assertEqual(handle, 123)
                child.communicate.assert_called_once_with(b'input', timeout=3)
                popen.return_value.__exit__.assert_not_called()
                return {'status': 'collected'}
            fields.side_effect = read
            result, usage = self.module.run_windows_process(['fake'], b'input', {}, 3)
        self.assertEqual(result.stdout, b'2\n')
        self.assertEqual(usage['status'], 'collected')

    def test_windows_memory_checks_file_transport_and_child_output(self):
        case = {'name': 'file-fixture', 'program': '2\n',
                'expected': ['2'], 'input_mode': 'file'}
        for output in (b'2\n', b'3\n'):
            def run(argv, payload, environment, timeout):
                self.assertEqual(payload, b'')
                self.assertEqual(Path(argv[-1]).read_bytes(), b'2\n')
                return (subprocess.CompletedProcess(argv, 0, output, b''),
                        {'status': 'collected', 'peak_rss_native': 123,
                         'native_unit': 'bytes (Windows peak working set)'})
            with tempfile.TemporaryDirectory() as directory, \
                    mock.patch.object(self.module.platform, 'system', return_value='Windows'), \
                    mock.patch.object(self.module, 'run_windows_process', side_effect=run):
                if output == b'2\n':
                    result = self.module.rss_profile(self.bench, {'argv': ['fake']},
                                                     case, Path(directory), 3)
                    self.assertTrue(result['checked'])
                    self.assertEqual(result['peak_rss_native'], 123)
                    self.assertEqual(result['input_mode'], 'file')
                    self.assertTrue((Path(directory) / 'file-fixture.rss.json').is_file())
                else:
                    with self.assertRaises(self.bench.BenchmarkError):
                        self.module.rss_profile(self.bench, {'argv': ['fake']},
                                                case, Path(directory), 3)

    def test_windows_deadline_kills_and_reaps_before_propagating(self):
        child = mock.MagicMock()
        child.communicate.side_effect = [subprocess.TimeoutExpired(['fake'], 1), (b'', b'')]
        with mock.patch.object(self.module.subprocess, 'Popen') as popen, \
                mock.patch.object(self.module, 'windows_process_fields') as fields:
            popen.return_value.__enter__.return_value = child
            with self.assertRaises(subprocess.TimeoutExpired):
                self.module.run_windows_process(['fake'], b'', {}, 1)
        child.kill.assert_called_once_with()
        self.assertEqual(child.communicate.call_count, 2)
        fields.assert_not_called()

    @unittest.skipUnless(sys.platform == 'win32', 'native Windows process accounting')
    def test_windows_peak_survives_child_exit_and_memory_release(self):
        # Touch then release memory before exit: the kernel peak must survive,
        # rather than reflecting a sampled current working set or the parent.
        code = ('import sys; b=bytearray(64*1024*1024); '
                'b[::4096]=bytes([1])*len(b[::4096]); del b; print(2)')
        case = {'name': 'native-windows', 'program': '', 'expected': ['2']}
        with tempfile.TemporaryDirectory() as directory:
            result = self.module.rss_profile(self.bench, {'argv': [sys.executable, '-c', code]},
                                             case, Path(directory), 20)
        self.assertTrue(result['checked'])
        self.assertEqual(result['status'], 'collected')
        self.assertGreaterEqual(result['peak_rss_native'], 64 * 1024 * 1024)
        self.assertEqual(result['cpu_time']['status'], 'collected')
        self.assertGreaterEqual(result['cpu_time']['user_seconds'], 0)

    def test_native_memory_units_and_optional_counters(self):
        darwin = self.module.rss_fields('Darwin',
            ' 8589934592 maximum resident set size\n'
            ' 9999999999 instructions retired\n 22222222222 cycles elapsed\n'
            ' 1234 peak memory footprint\n')
        self.assertEqual(darwin['peak_rss_native'], 8589934592)
        self.assertIn('bytes', darwin['native_unit'])
        self.assertEqual(darwin['native_counters']['instructions_retired'], 9999999999)
        self.assertEqual(darwin['native_counters']['cycles_elapsed'], 22222222222)
        self.assertEqual(darwin['peak_memory_footprint_bytes'], 1234)
        linux = self.module.rss_fields('Linux', 'MAX_RSS_KIB=65536\n')
        self.assertEqual(linux['peak_rss_native'], 65536)
        self.assertIn('KiB', linux['native_unit'])
        self.assertNotIn('native_counters', linux)
        self.assertEqual(self.module.rss_fields('Darwin', 'missing\n')['status'], 'unavailable')
        self.assertEqual(self.module.rss_fields('Windows', '')['status'], 'unavailable')

    def test_native_cpu_time_is_optional_and_distinct_from_elapsed_time(self):
        darwin = self.module.cpu_time_fields('Darwin', ' 47.73 real 46.91 user 0.05 sys\n')
        self.assertEqual(darwin['status'], 'collected')
        self.assertEqual((darwin['user_seconds'], darwin['system_seconds']), (46.91, .05))
        linux = self.module.cpu_time_fields('Linux',
                    'MAX_RSS_KIB=4096\nUSER_SECONDS=0.00\nSYSTEM_SECONDS=0.01\n')
        self.assertEqual((linux['user_seconds'], linux['system_seconds']), (0, .01))
        for system, content in (('Windows', ''), ('Linux', 'USER_SECONDS=0.2\n'),
                                ('Darwin', 'unrecognized'), ('Linux', 'USER_SECONDS=-1\nSYSTEM_SECONDS=0\n')):
            self.assertEqual(self.module.cpu_time_fields(system, content)['status'], 'unavailable')

    def test_each_repeated_output_is_checked(self):
        case = {'name': 'fixture', 'expected': ['2']}
        self.module.check(self.bench, case, 2, 0, b'2\n2\n', b'')
        for status, output, errors in ((23, b'', b''), (0, b'2\n', b''),
                                       (0, b'2\n3\n', b''), (0, b'2\n2\n', b'warning')):
            with self.assertRaises(self.bench.BenchmarkError):
                self.module.check(self.bench, case, 2, status, output, errors)

    def test_each_mathematical_result_and_state_is_checked(self):
        case = {'name': 'math-fixture', 'expected': None,
                'oracle': {'kind': 'decimal-ulp', 'lower': '1', 'upper': '1',
                           'scale': 2, 'expected_state': ['2', '10', '2']}}
        chunk = b'1.00\n2\n10\n2\n'
        self.module.check(self.bench, case, 2, 0, chunk * 2, b'')
        for output in (chunk, chunk * 2 + b'0\n',
                       chunk + b'1.03\n2\n10\n2\n',
                       chunk + b'1.00\n3\n10\n2\n'):
            with self.assertRaises(self.bench.BenchmarkError):
                self.module.check(self.bench, case, 2, 0, output, b'')

    def test_cpu_preserves_file_transport(self):
        case = {'name': 'file-fixture', 'program': '2\n',
                'expected': ['2'], 'input_mode': 'file'}
        code = ('import pathlib,sys; assert not sys.stdin.read(); '
                'print(pathlib.Path(sys.argv[1]).read_text(),end="")')
        tool = {'argv': [sys.executable, '-c', code]}
        with tempfile.TemporaryDirectory() as directory, \
                mock.patch.object(self.module.platform, 'system', return_value='Unsupported'):
            result = self.module.cpu_profile(self.bench, tool, case, 2, Path(directory), 3)
        self.assertEqual(result['status'], 'unavailable')

    def test_linux_sampling_checks_output_before_decoding(self):
        case = {'name': 'fixture', 'program': '2\n', 'expected': ['2'], 'input_mode': 'file'}
        tool = {'argv': ['fake-bc']}
        for mode in ('success', 'wrong-output', 'child-exit', 'empty-trace', 'decode-exit'):
            with self.subTest(mode=mode), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)

                def run(argv, payload, environment, timeout):
                    self.assertEqual(payload, b'')
                    self.assertEqual(environment['PERF_CONFIG'], '/dev/null')
                    if argv[1] == 'record':
                        self.assertIn('cpu-clock:u', argv)
                        self.assertEqual(Path(argv[-1]).read_bytes(), b'2\n2\n')
                        (root / 'fixture.perf.data').write_bytes(b'profile')
                        output = b'3\n3\n' if mode == 'wrong-output' else b'2\n2\n'
                        return subprocess.CompletedProcess(argv, int(mode == 'child-exit'), output, b'')
                    self.assertEqual(argv[1:], ['script', '-i', str(root / 'fixture.perf.data')])
                    trace = b'' if mode == 'empty-trace' else b'cpu-clock:u sample\n'
                    return subprocess.CompletedProcess(argv, int(mode == 'decode-exit'), trace, b'')

                with mock.patch.object(self.module, 'run_group', side_effect=run) as child:
                    args = (self.bench, tool, case, 2, root, 3, Path('/fake/perf'))
                    if mode == 'success':
                        report = self.module.cpu_profile(*args)
                        self.assertTrue(report['checked'])
                        self.assertEqual(report['input_mode'], 'file')
                        self.assertEqual(report['trace_sha256'],
                                         hashlib.sha256(b'cpu-clock:u sample\n').hexdigest())
                    else:
                        with self.assertRaises(self.bench.BenchmarkError):
                            self.module.cpu_profile(*args)
                    self.assertEqual(child.call_count, 1 if mode in ('wrong-output', 'child-exit') else 2)

    def test_memory_preserves_file_transport(self):
        case = {'name': 'file-fixture', 'program': '2\n',
                'expected': ['2'], 'input_mode': 'file'}
        def child(argv, payload, environment, timeout):
            self.assertEqual(payload, b'')
            self.assertEqual(Path(argv[-1]).read_bytes(), b'2\n')
            Path(argv[argv.index('-o') + 1]).write_text('MAX_RSS_KIB=1\n')
            return subprocess.CompletedProcess(argv, 0, b'2\n', b'')
        with tempfile.TemporaryDirectory() as directory, \
                mock.patch.object(self.module.platform, 'system', return_value='Linux'), \
                mock.patch.object(Path, 'is_file', return_value=True), \
                mock.patch.object(self.module, 'run_group', side_effect=child):
            result = self.module.rss_profile(self.bench, {'argv': ['fake']},
                                             case, Path(directory), 3)
        self.assertEqual(result['peak_rss_native'], 1)

    def test_program_archive_matches_declared_bytes(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            config = root / 'config.json'
            config.write_text(json.dumps({'implementations': [{
                'name': 'fake', 'argv': [sys.executable, '-c', 'import sys; sys.stdin.read(); print(2)'],
                'metadata': {'source_revision': 'fixture', 'compiler': 'fixture', 'flags': 'fixture',
                             'version': 'fixture', 'instrumented': False,
                             'binary_sha256': hashlib.sha256(Path(sys.executable).read_bytes()).hexdigest()}}]}))
            summary = root / 'summary.json'
            summary.write_text(json.dumps({'workloads': [{'name': 'startup',
                'implementations': {'fake': {'median_seconds': 10}}}]}))
            output = root / 'artifacts'
            argv = [COLLECTOR, '--config', str(config), '--implementation', 'fake',
                    '--summary', str(summary), '--output', str(output), '--workload', 'startup']
            original = Path.write_text
            def translated(path, data, *args, **kwargs):
                if path.suffix == '.bc':
                    return path.write_bytes(data.replace('\n', '\r\n').encode('utf-8'))
                return original(path, data, *args, **kwargs)
            with mock.patch.object(Path, 'write_text', translated), \
                    mock.patch.object(sys, 'argv', argv), \
                    mock.patch.object(self.module.platform, 'system', return_value='Unsupported'), \
                    contextlib.redirect_stdout(io.StringIO()):
                self.assertEqual(self.module.main(), 0)
            manifest = json.loads((output / 'manifest.json').read_text())
            self.assertEqual(hashlib.sha256((output / 'startup.bc').read_bytes()).hexdigest(),
                             manifest['workloads'][0]['program_sha256'])

    def test_frozen_math_cli_archives_oracle_and_declared_input(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            frozen = root / 'frozen'
            (frozen / 'workloads').mkdir(parents=True)
            program = b'fixture\n'
            case = {'oracle': {'kind': 'decimal-ulp', 'lower': '1', 'upper': '1',
                               'scale': 2, 'expected_state': ['2', '10', '2']}}
            oracle = self.bench.oracle_payload(case)
            (frozen / 'workloads/math.bc').write_bytes(program)
            (frozen / 'workloads/math.expected').write_bytes(oracle)
            (frozen / 'manifest.json').write_text(json.dumps({'suite': 'math', 'corpus': [{
                'name': 'math', 'group': 'fixture', 'parameters': {}, 'input_mode': 'file',
                'program_sha256': hashlib.sha256(program).hexdigest(),
                'oracle_sha256': hashlib.sha256(oracle).hexdigest()}]}))
            declared = root / 'library.bc'
            declared.write_bytes(b'ordinary library fixture\n')
            code = ('import pathlib,sys; assert not sys.stdin.read(); '
                    'assert pathlib.Path(sys.argv[2]).read_bytes()==b"fixture\\n"; '
                    'print("1.00\\n2\\n10\\n2")')
            config = root / 'config.json'
            config.write_text(json.dumps({'implementations': [{
                'name': 'fake', 'argv': [sys.executable, '-c', code, str(declared)],
                'input_artifacts': [{'path': str(declared),
                    'sha256': hashlib.sha256(declared.read_bytes()).hexdigest()}],
                'metadata': {'source_revision': 'fixture', 'compiler': 'fixture', 'flags': 'fixture',
                             'version': 'fixture', 'instrumented': False,
                             'binary_sha256': hashlib.sha256(Path(sys.executable).read_bytes()).hexdigest()}}]}))
            summary = root / 'summary.json'
            summary.write_text(json.dumps({'workloads': [{'name': 'math',
                'implementations': {'fake': {'median_seconds': 10}}}]}))
            output = root / 'artifacts'
            argv = [COLLECTOR, '--config', str(config), '--implementation', 'fake',
                    '--summary', str(summary), '--output', str(output), '--workload', 'math',
                    '--suite', 'math', '--replay-corpus', str(frozen)]
            with mock.patch.object(sys, 'argv', argv), \
                    mock.patch.object(self.module.platform, 'system', return_value='Unsupported'), \
                    contextlib.redirect_stdout(io.StringIO()):
                self.assertEqual(self.module.main(), 0)
            manifest = json.loads((output / 'manifest.json').read_text())
            self.assertEqual((output / 'math.expected').read_bytes(), oracle)
            self.assertEqual(manifest['suite'], 'math')
            self.assertEqual(manifest['replay']['source_manifest_sha256'],
                             self.bench.digest_file(frozen / 'manifest.json'))
            row = manifest['workloads'][0]
            self.assertEqual(row['oracle_sha256'], hashlib.sha256(oracle).hexdigest())
            self.assertEqual(row['cpu']['input_mode'], 'file')
            artifact = manifest['implementation']['input_artifacts'][0]
            self.assertEqual((output / artifact['archived_as']).read_bytes(), declared.read_bytes())
            # A library changed after initial configuration cannot silently be profiled.
            declared.write_bytes(b'changed\n')
            with self.assertRaises(self.bench.BenchmarkError):
                with self.module.diagnostic_input(self.bench, manifest['implementation'],
                                                  {'program': '2\n'}, 1):
                    self.fail('changed declared input reached launch')

    def test_sampler_fallback_checks_output(self):
        case = {'name': 'fixture', 'program': 'ignored\n', 'expected': ['2']}
        tool = {'argv': [sys.executable, '-c', 'import sys; sys.stdin.read(); print(2)']}
        with tempfile.TemporaryDirectory() as directory, \
                mock.patch.object(self.module.platform, 'system', return_value='Unsupported'):
            result = self.module.cpu_profile(self.bench, tool, case, 1, Path(directory), 3)
        self.assertEqual(result['status'], 'unavailable')

    @unittest.skipUnless(os.name == 'posix', 'native time wrapper is POSIX-only')
    def test_wrapper_timeout_closes_descendant_pipes(self):
        # If only the wrapper dies, its child's inherited output pipe stays open
        # for two seconds. The outer deadline detects that cleanup failure.
        with tempfile.TemporaryDirectory() as directory:
            wrapper = Path(directory) / 'wrapper.py'
            wrapper.write_text("import subprocess, sys, time\n"
                               "subprocess.Popen([sys.executable, '-c', 'import time; time.sleep(2)'])\n"
                               "time.sleep(2)\n")
            probe = Path(directory) / 'probe.py'
            probe.write_text("import importlib.util, subprocess, sys\n"
                             "spec=importlib.util.spec_from_file_location('collector',sys.argv[1])\n"
                             "module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module)\n"
                             "try: module.run_group([sys.executable,sys.argv[2]],b'',None,0.2)\n"
                             "except subprocess.TimeoutExpired: pass\n"
                             "else: raise SystemExit(1)\n")
            result = subprocess.run([sys.executable, str(probe), COLLECTOR, str(wrapper)],
                                    stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=1.5)
            self.assertEqual(result.returncode, 0, result.stderr)

    def test_timeout_kills_and_reaps_diagnostic_child(self):
        case = {'name': 'fixture', 'program': 'ignored\n', 'expected': ['2']}
        tool = {'argv': [sys.executable, '-c', 'import time; time.sleep(5)']}
        with tempfile.TemporaryDirectory() as directory, \
                mock.patch.object(self.module.platform, 'system', return_value='Unsupported'):
            with self.assertRaises(subprocess.TimeoutExpired):
                self.module.cpu_profile(self.bench, tool, case, 1, Path(directory), 0.05)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('collector')
    args, remaining = parser.parse_known_args()
    COLLECTOR = str(Path(args.collector).resolve())
    unittest.main(argv=[__file__, *remaining])
