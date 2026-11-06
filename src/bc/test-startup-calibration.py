#!/usr/bin/env python3
"""Check fixed startup calibration decisions using synthetic fixtures only."""
import copy
import hashlib
import importlib.util
import io
import json
from pathlib import Path
import tempfile
import time
import unittest
from unittest import mock


class StartupCalibration(unittest.TestCase):
    def setUp(self):
        path = Path(__file__).resolve().parent / 'scripts/startup-calibration.py'
        spec = importlib.util.spec_from_file_location('startup_calibration', path)
        self.module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.module)
        self.clock = dict(monotonic=True, adjustable=False, resolution=1e-9)
        self.stat = dict(ci95=[.99, 1.01], pairs=151)
        self.blocks = [dict(base_median_seconds=.001, initial=dict(self=copy.deepcopy(self.stat), aa=copy.deepcopy(self.stat)),
                            confirmation=dict(self=copy.deepcopy(self.stat), aa=copy.deepcopy(self.stat))) for _ in range(60)]

    def test_fixed_null_study_qualifies_without_granting_merge_eligibility(self):
        report = self.module.assess_calibration(self.blocks, self.clock)
        self.assertEqual(report['status'], 'qualified empirical study')
        self.assertEqual(report['precise_blocks'], 60)
        self.assertEqual(report['scaled_5pct_detected_blocks'], 60)
        self.assertEqual(report['confirmed_null_blocks'], 0)
        self.assertFalse(report['merge_eligible'])
        self.assertEqual(self.blocks[0]['initial']['aa'], self.stat)

    def test_both_directions_of_persistent_null_difference_disqualify(self):
        for interval in ([1.001, 1.01], [.98, .999]):
            blocks = copy.deepcopy(self.blocks)
            for stage in ('initial', 'confirmation'):
                blocks[0][stage]['aa']['ci95'] = interval
            report = self.module.assess_calibration(blocks, self.clock)
            self.assertEqual(report['status'], 'inconclusive')
            self.assertEqual(report['confirmed_null_blocks'], 1)

    def test_precision_sensitivity_and_clock_fail_closed(self):
        for change in ('precision', 'sensitivity', 'clock', 'resolution'):
            blocks, clock = copy.deepcopy(self.blocks), dict(self.clock)
            if change in ('precision', 'sensitivity'):
                interval = [.8, 1.2] if change == 'precision' else [.95, 1.05]
                for block in blocks:
                    block['initial']['self']['ci95'] = interval
            elif change == 'clock':
                clock['monotonic'] = False
            else:
                clock['resolution'] = .01
            with self.subTest(change=change):
                self.assertEqual(self.module.assess_calibration(blocks, clock)['status'], 'inconclusive')

    def test_missing_nonfinite_counts_and_incomplete_studies_cannot_qualify(self):
        variants = [self.blocks[:-1]]
        for change in ('missing', 'nan', 'count', 'bool'):
            blocks = copy.deepcopy(self.blocks)
            stat = blocks[0]['initial']['aa']
            if change == 'missing':
                blocks[0].pop('confirmation')
            elif change == 'nan':
                stat['ci95'][0] = float('nan')
            elif change == 'count':
                stat['pairs'] = 150
            else:
                stat['pairs'] = True
            variants.append(blocks)
        for blocks in variants:
            self.assertEqual(self.module.assess_calibration(blocks, self.clock)['status'], 'inconclusive')

    def test_candidate_rule_keeps_startup_separate_from_howard_margin(self):
        calibration = self.module.assess_calibration(self.blocks, self.clock)
        for interval, expected in (([.99, 1.01], 'no statistically significant startup regression observed'),
                                   ([.8, .9], 'no statistically significant startup regression observed'),
                                   ([1.001, 1.01], 'confirmed startup regression')):
            stages = {name: dict(aa=copy.deepcopy(self.stat), candidate=dict(ci95=interval, pairs=151))
                      for name in ('initial', 'confirmation')}
            with self.subTest(interval=interval):
                report = self.module.assess_candidate(calibration, stages)
                self.assertEqual(report['status'], expected)
                self.assertFalse(report['merge_eligible'])
        stages['initial']['candidate'] = dict(ci95=[.99, 1.01], pairs=151)
        self.assertEqual(self.module.assess_candidate(calibration, stages)['status'], 'inconclusive')
        self.assertEqual(self.module.assess_candidate(dict(status='inconclusive'), stages)['status'], 'inconclusive')

    def test_independent_build_and_uninstrumented_identity_are_required(self):
        with tempfile.TemporaryDirectory() as temporary:
            tools = []
            for name in ('base', 'aa', 'candidate'):
                path = Path(temporary) / name
                path.write_bytes(name.encode())
                tools.append(dict(name=name, argv=[str(path)], metadata=dict(
                    source_revision='a' * 40, compiler='fixture', flags=['fixture'], version='fixture',
                    instrumented=False, binary_sha256=hashlib.sha256(path.read_bytes()).hexdigest(),
                    configuration=dict(diagnostic_profile=False))))
            self.module.validate_tools(tools)
            for change in ('alias', 'source', 'instrumented', 'hash'):
                invalid = copy.deepcopy(tools)
                if change == 'alias':
                    invalid[1] = dict(copy.deepcopy(invalid[0]), name='aa')
                elif change == 'source':
                    invalid[1]['metadata']['source_revision'] = 'b' * 40
                elif change == 'instrumented':
                    invalid[2]['metadata']['instrumented'] = True
                else:
                    invalid[0]['metadata']['binary_sha256'] = '0' * 64
                with self.subTest(change=change), self.assertRaises(ValueError):
                    self.module.validate_tools(invalid)

    def test_phase_retains_all_checked_samples_and_fixed_counts_without_bc(self):
        spec = importlib.util.spec_from_file_location('startup_fixture_bench',
            Path(__file__).resolve().parent / 'scripts/bench-bc.py')
        bench = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(bench)
        tools = [dict(name=name) for name in ('base', 'self', 'aa')]
        case = dict(name='startup', group='startup', program='1+1\n', expected=['2'], parameters={})
        raw = io.StringIO()
        calls = []

        def fixture(tool, case, timeout):
            calls.append(tool['name'])
            return dict(seconds=.001, checked=True, output_bytes=2, output_sha256='f' * 64)

        with mock.patch.object(bench, 'run_checked', fixture):
            report = self.module.collect_phase(bench, tools, case, raw, 0, 'initial', 200659,
                                                time.monotonic() + 30)
        rows = [json.loads(line) for line in raw.getvalue().splitlines()]
        self.assertEqual(len(calls), (151 + 5) * 3)
        self.assertEqual(len(rows), len(calls))
        self.assertTrue(all(row['checked'] for row in rows))
        self.assertEqual(report['ratios']['aa']['pairs'], 151)
        self.assertEqual(report['ratios']['self']['ci95'], [1, 1])
        self.assertEqual(report['base_median_seconds'], .001)
        with mock.patch.object(bench, 'run_checked', fixture), self.assertRaises(TimeoutError):
            self.module.collect_phase(bench, tools, case, io.StringIO(), 1, 'initial', 200659,
                                      time.monotonic() - 1)


if __name__ == '__main__':
    unittest.main()
