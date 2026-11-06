#!/usr/bin/env python3
"""Check fixed startup calibration decisions using synthetic fixtures only."""
import copy
import importlib.util
from pathlib import Path
import unittest


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


if __name__ == '__main__':
    unittest.main()
