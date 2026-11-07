#!/usr/bin/env python3
"""Check process-control semantics with synthetic observations, never bc timings."""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import random
import subprocess
import tempfile
import types
import unittest
from unittest import mock

PATH = Path(__file__).parent / 'scripts/benchmark-process-controls.py'
SPEC = importlib.util.spec_from_file_location('process_controls', str(PATH))
C = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(C)
B = C.load('bench-bc')
P = C.load('profile-bc')


def native(ticks=10000000):
    return dict(cpu_time=dict(status='collected', user_ticks_100ns=ticks,
                              system_ticks_100ns=0))


class ProcessControls(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.first, self.second = [self.root / name for name in ('a', 'b')]
        self.first.write_bytes(b'ordinary fixture bytes')
        self.second.write_bytes(b'independently built fixture bytes')
        self.a, self.b = [dict(name=name, argv=[str(p)], metadata=dict(
            source_revision=C.BASE, instrumented=False,
            binary_sha256=B.digest_file(p)))
            for name, p in (('build0', self.first), ('build1', self.second))]
        self.tool = dict(name='fixture', argv=[str(self.first), '-l'],
                         binary_sha256=B.digest_file(self.first))
        self.case = dict(name='fixture', program='1+1\n', expected=['2'],
                         group='fixture', parameters={})

    def test_protocol_is_fixed_and_corpus_has_independent_heldout_arguments(self):
        self.assertEqual((C.SAMPLES, C.WARMUPS, C.ITERATIONS), (19, 3, 4096))
        self.assertEqual((C.TOTAL_SECONDS, C.CHILD_SECONDS), (6600, 120))
        full, held = [C.cases(B, preset) for preset in ('full', 'holdout')]
        self.assertEqual(len(full), 8)
        self.assertEqual(len(held), 8)
        self.assertEqual({c['parameters']['function'] for c in full}, set('scalej'))
        for collection in (full, held):
            self.assertEqual(sum(c['parameters']['scale'] == 20 for c in collection), 7)
            self.assertEqual(sum(c['parameters']['scale'] == 100 for c in collection), 1)
            for case in collection:
                self.assertEqual(case['parameters']['iterations'], 4096)
                self.assertIn('i<4096;', case['program'])
                self.assertEqual(case['oracle']['kind'], 'decimal-ulp')
        self.assertTrue({c['oracle']['reference_id'] for c in full}.isdisjoint(
                        c['oracle']['reference_id'] for c in held))

    def test_deployments_preserve_alias_and_distinct_copy_identities(self):
        tools = C.deployments(self.a, self.b, self.root / 'copy', B)
        self.assertEqual([t['name'] for t in tools], ['reference', 'alias', 'copy', 'independent'])
        self.assertEqual(tools[0]['argv'], tools[1]['argv'])
        self.assertFalse(os.path.samefile(tools[0]['argv'][0], tools[2]['argv'][0]))
        self.assertEqual(B.digest_file(tools[0]['argv'][0]), B.digest_file(tools[2]['argv'][0]))
        self.assertNotEqual(B.digest_file(tools[0]['argv'][0]), B.digest_file(tools[3]['argv'][0]))
        self.assertTrue(all(t['argv'][1:] == ['-l'] for t in tools))
        tools[1]['metadata']['instrumented'] = True
        self.assertIs(self.a['metadata']['instrumented'], False)
        self.assertIs(tools[0]['metadata']['instrumented'], False)

    def test_independent_build_rejects_same_file(self):
        with self.assertRaisesRegex(ValueError, 'own file identity'):
            C.deployments(self.a, self.a, self.root / 'copy', B)

    def test_deployment_rejects_changed_instrumented_or_wrong_source_image(self):
        for key, value in (('instrumented', True), ('source_revision', 'f' * 40),
                           ('binary_sha256', '0' * 64)):
            with self.subTest(key=key), mock.patch.dict(self.a['metadata'], {key: value}):
                with self.assertRaisesRegex(ValueError, 'accepted-source image'):
                    C.deployments(self.a, self.b, self.root / 'copy', B)

    def invoke(self, status=0, output=b'2\n', errors=b'', deadline=105):
        adapter = mock.Mock(return_value=(subprocess.CompletedProcess(
            self.tool['argv'], status, output, errors), native()))
        profile = types.SimpleNamespace(run_windows_process=adapter, check=P.check)
        with mock.patch.object(C.time, 'monotonic', return_value=100), \
                mock.patch.object(C.time, 'perf_counter', side_effect=[10, 10.75]):
            result = C.checked(B, profile, self.tool, self.case, deadline)
        return result, adapter

    def test_checked_validates_output_and_clips_timeout_to_remaining_budget(self):
        result, adapter = self.invoke()
        self.assertEqual(result['seconds'], .75)
        self.assertTrue(result['checked'])
        self.assertEqual(result['output_sha256'], hashlib.sha256(b'2\n').hexdigest())
        self.assertEqual(adapter.call_args.args[1], b'1+1\n')
        self.assertEqual(adapter.call_args.args[3], 5)
        self.assertNotIn('GD_BC_PROFILE_OUTPUT', adapter.call_args.args[2])
        _, adapter = self.invoke(deadline=1000)
        self.assertEqual(adapter.call_args.args[3], 120)

    def test_checked_rejects_bad_status_stderr_and_wrong_output(self):
        for kwargs in (dict(status=1), dict(errors=b'failure'), dict(output=b'3\n')):
            with self.subTest(kwargs=kwargs), self.assertRaises(B.BenchmarkError):
                self.invoke(**kwargs)

    def test_expired_deadline_never_launches_child(self):
        with mock.patch.object(C.time, 'monotonic', return_value=100):
            adapter = mock.Mock()
            with self.assertRaisesRegex(ValueError, 'deadline'):
                C.checked(B, types.SimpleNamespace(run_windows_process=adapter),
                          self.tool, self.case, 99)
            adapter.assert_not_called()

    def test_changed_image_never_launches_child(self):
        self.first.write_bytes(b'changed')
        adapter = mock.Mock()
        with self.assertRaisesRegex(ValueError, 'changed'):
            C.checked(B, types.SimpleNamespace(run_windows_process=adapter),
                      self.tool, self.case, 1000)
        adapter.assert_not_called()

    def test_native_cpu_ticks_keep_zeros_unavailable_and_invalid_distinct(self):
        self.assertEqual(C.cpu_seconds(dict(native=native(0))), 0)
        self.assertEqual(C.cpu_seconds(dict(native=native(12345678))), 1.2345678)
        self.assertIsNone(C.cpu_seconds(dict(native=dict(cpu_time=dict(status='unavailable')))))
        for bad in (-1, True, .1, None):
            with self.subTest(bad=bad), self.assertRaisesRegex(ValueError, 'CPU ticks'):
                C.cpu_seconds(dict(native=native(bad)))

    def sample(self, **options):
        tools = [dict(name=n) for n in ('reference', 'alias', 'copy', 'independent')]
        observations = []
        for number in range(3):
            for tool in tools:
                observations.append(dict(phase='measurement', round=number,
                    workload='fixture', implementation=tool['name'], seconds=1,
                    native=options.get('native', native())))
        with mock.patch.object(C, 'SAMPLES', 3):
            result, = C.summarize(B, tools, [self.case], observations, 197)
        return result, tools, observations

    def test_summary_replays_paired_cpu_and_wall_without_acceptance(self):
        result, _, _ = self.sample()
        self.assertTrue(result['cpu_duration_adequate'])
        self.assertEqual(len(result['comparisons']), 3)
        for row in result['comparisons']:
            self.assertEqual(row['wrapper_wall']['ci95'], [1, 1])
            self.assertEqual(row['cpu']['ci95'], [1, 1])
        self.assertNotIn('acceptance', result)

    def test_zero_or_missing_cpu_keeps_wall_and_reports_unqualified(self):
        for fields in (native(0), dict(cpu_time=dict(status='unavailable'))):
            with self.subTest(fields=fields):
                result, _, _ = self.sample(native=fields)
                self.assertFalse(result['cpu_duration_adequate'])
                self.assertIn('unqualified', result['cpu_status'])
                self.assertTrue(all(r['cpu'] is None for r in result['comparisons']))
                self.assertTrue(all(r['wrapper_wall']['ci95'] == [1, 1]
                                    for r in result['comparisons']))

    def test_positive_short_cpu_does_not_satisfy_duration_requirement(self):
        result, _, _ = self.sample(native=native(156250))
        self.assertFalse(result['cpu_duration_adequate'])
        self.assertEqual(result['cpu_role_medians_seconds']['reference'], .015625)

    def test_incomplete_sample_count_is_rejected(self):
        _, tools, rows = self.sample()
        with mock.patch.object(C, 'SAMPLES', 3), self.assertRaisesRegex(ValueError, 'incomplete'):
            C.summarize(B, tools, [self.case], rows[:-1], 197)

    def test_streamed_samples_keep_seed_order_programs_and_checked_identity(self):
        _, tools, _ = self.sample()
        target = self.root / 'stage'
        fake = dict(seconds=1, native=native(), checked=True, output_bytes=2,
                    output_sha256=hashlib.sha256(b'2\n').hexdigest())
        with mock.patch.object(C, 'checked', return_value=fake) as run, \
                mock.patch.object(C, 'SAMPLES', 3), mock.patch.object(C, 'WARMUPS', 1):
            C.measure(B, P, tools, [self.case], target, 100, 197)
        rows = [json.loads(line) for line in (target / 'samples.jsonl').read_text().splitlines()]
        self.assertEqual(len(rows), 16)
        rng, index = random.Random(197), 0
        for phase, count in (('warmup', 1), ('measurement', 3)):
            for number in range(count):
                workloads = [self.case]
                rng.shuffle(workloads)
                ordered = list(tools)
                rng.shuffle(ordered)
                for order, tool in enumerate(ordered):
                    self.assertEqual((rows[index]['phase'], rows[index]['round'],
                                      rows[index]['implementation'], rows[index]['order']),
                                     (phase, number, tool['name'], order))
                    index += 1
        self.assertEqual(run.call_count, 16)
        self.assertEqual((target / 'workloads/fixture.bc').read_bytes(), b'1+1\n')
        self.assertEqual((target / 'workloads/fixture.expected').read_bytes(), b'2\n')

    def test_failure_preserves_prior_streamed_rows(self):
        _, tools, _ = self.sample()
        fake = dict(seconds=1, native=native(), checked=True)
        target = self.root / 'failed'
        with mock.patch.object(C, 'checked', side_effect=[fake, B.BenchmarkError('incorrect')]), \
                self.assertRaisesRegex(B.BenchmarkError, 'incorrect'):
            C.measure(B, P, tools, [self.case], target, 100, 197)
        rows = (target / 'samples.jsonl').read_text().splitlines()
        self.assertEqual(len(rows), 1)
        self.assertTrue(json.loads(rows[0])['checked'])

    def controller(self, available=True, build_failure=False, measurement_failure=None,
                   preservation_failure=False):
        output = self.root / 'controller'
        events, collector = [], mock.Mock()
        original_load = C.load

        class Commands:
            def __init__(self, root, seconds):
                self.commands, self.deadline = [], 1234
                self.root = root
                self.total_seconds = seconds

            def run(self, argv, timeout):
                name = argv[argv.index('--name') + 1]
                events.append(name)
                self.commands.append(dict(argv=argv, timeout=timeout))
                if build_failure and name == 'build1':
                    raise ValueError('second build failed')
                directory = Path(argv[argv.index('--output') + 1])
                directory.mkdir()
                binary = directory / 'bc.fixture'
                binary.write_bytes(name.encode())
                tool = dict(name=name, argv=[str(binary)], metadata=dict(
                    source_revision=C.BASE, binary_sha256=B.digest_file(binary), instrumented=False))
                B.write_json(directory / 'implementations.json', dict(implementations=[tool]))

        def implementations(path, require_provenance):
            self.assertTrue(require_provenance)
            tools = json.loads(path.read_text())['implementations']
            for tool in tools:
                tool['binary_sha256'] = tool['metadata']['binary_sha256']
            return tools

        def collect(tools, directory, timeout, deadline):
            events.append('collect')
            collector(tools, directory, timeout, deadline)
            if preservation_failure:
                raise ValueError('asset changed')

        def measure(*args):
            events.append('measure')
            if measurement_failure:
                raise measurement_failure
            return []

        def modules(name):
            if name == 'benchmark-math-ci':
                return types.SimpleNamespace(Commands=Commands)
            if name == 'windows-image-probe':
                return types.SimpleNamespace(native_windows_64=lambda: available)
            if name == 'benchmark-ci':
                return types.SimpleNamespace(host_info=lambda: {})
            if name == 'collect-bc-codegen':
                return types.SimpleNamespace(collect=collect)
            return original_load(name)

        with mock.patch.object(C, 'load', modules), mock.patch.object(C, 'measure', measure), \
                mock.patch.object(B, 'implementations', implementations), \
                mock.patch.object(C, 'cases', return_value=[]):
            # Return this benchmark module so its normalization fixture is used.
            real_modules = modules
            with mock.patch.object(C, 'load', side_effect=lambda name:
                                   B if name == 'bench-bc' else real_modules(name)):
                result = C.main(['--source', str(self.root), '--output', str(output),
                                 '--replica', '2'])
        return result, json.loads((output / 'evidence/report.json').read_text()), events, collector

    def test_controller_collects_all_images_after_both_stages(self):
        result, report, events, collector = self.controller()
        self.assertEqual(result, 0)
        self.assertEqual(events, ['build0', 'build1', 'measure', 'measure', 'collect'])
        self.assertEqual([t['name'] for t in collector.call_args.args[0]],
                         ['reference', 'alias', 'copy', 'independent'])
        self.assertEqual(collector.call_args.args[2:], (15, 1234))
        self.assertIs(report['performance_acceptance'], False)
        self.assertEqual(report['replica'], 2)
        self.assertEqual(report['source_revision'], C.BASE)
        self.assertEqual([r['same_file_as_reference'] for r in report['deployments']],
                         [True, True, False, False])

    def test_unsupported_platform_reports_unavailable_without_build_or_execution(self):
        result, report, events, collector = self.controller(available=False)
        self.assertEqual(result, 0)
        self.assertEqual(report['status'], 'unavailable')
        self.assertEqual(events, [])
        collector.assert_not_called()

    def test_failed_second_build_preserves_completed_first_image(self):
        result, report, events, collector = self.controller(build_failure=True)
        self.assertEqual(result, 1)
        self.assertIn('second build failed', report['failure'])
        self.assertEqual(events, ['build0', 'build1', 'collect'])
        self.assertEqual([t['name'] for t in collector.call_args.args[0]], ['build0'])

    def test_failed_measurement_preserves_all_images(self):
        result, report, events, collector = self.controller(
            measurement_failure=B.BenchmarkError('wrong output'))
        self.assertEqual(result, 1)
        self.assertIn('wrong output', report['failure'])
        self.assertEqual(events, ['build0', 'build1', 'measure', 'collect'])
        self.assertEqual(len(collector.call_args.args[0]), 4)

    def test_failed_preservation_cannot_report_success(self):
        result, report, _, _ = self.controller(preservation_failure=True)
        self.assertEqual(result, 1)
        self.assertEqual(report['status'], 'failed')
        self.assertIn('asset changed', report['preservation_failure'])

    def test_keyboard_interrupt_is_not_swallowed(self):
        with self.assertRaises(KeyboardInterrupt):
            self.controller(measurement_failure=KeyboardInterrupt())
        report = json.loads((self.root / 'controller/evidence/report.json').read_text())
        self.assertEqual(report['status'], 'incomplete')


if __name__ == '__main__':
    unittest.main()
