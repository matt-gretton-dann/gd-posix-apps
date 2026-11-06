#!/usr/bin/env python3
"""Competitive-margin acceptance keeps observations and controls independent."""
import importlib.util
import copy
import math
from pathlib import Path
import unittest
from unittest import mock

PATH = Path(__file__).resolve().parent / 'scripts/benchmark-policy.py'
SPEC = importlib.util.spec_from_file_location('acceptance_policy', str(PATH))
POLICY = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(POLICY)


def ratio(low, high, pairs=9):
    return dict(ci95=[low, high], pairs=pairs)


class Acceptance(unittest.TestCase):
    def assess(self, first=None, competitive=None, **kwargs):
        options = dict(control='pass', system='Linux', required_pairs=9)
        options.update(kwargs)
        return POLICY.regression_acceptance(
            first if first is not None else ratio(1.06, 1.09), competitive, **options)

    def test_confirmed_regression_can_keep_a_proven_competitive_margin(self):
        self.assertEqual(self.assess(competitive=ratio(.80, .90), second=ratio(1.07, 1.10)),
                         'allowed with Howard margin')

    def test_existing_limits_remain_when_margin_is_not_proven(self):
        for competitor in (None, ratio(.8, .900001), ratio(.91, .99)):
            self.assertEqual(self.assess(competitive=competitor, second=ratio(1.06, 1.08)),
                             'exceeds regression limit')
        self.assertEqual(self.assess(first=ratio(.99, 1.05)), 'within regression limit')
        self.assertEqual(self.assess(first=ratio(1.01, 1.04), system='Darwin'), 'inconclusive')
        self.assertEqual(self.assess(first=ratio(.98, 1), system='Darwin'),
                         'within regression limit')
        self.assertEqual(self.assess(system='Darwin', competitive=ratio(.8, .89)),
                         'allowed with Howard margin')

    def test_control_failure_is_never_waived(self):
        for control in ('inconclusive', 'inconclusive-noisy', 'confirmed A/A bias', None):
            self.assertEqual(self.assess(competitive=ratio(.1, .2), control=control),
                             'inconclusive')

    def test_startup_short_duration_and_unknown_hosts_remain_unqualified(self):
        self.assertEqual(self.assess(competitive=ratio(.1, .2), substantive=False),
                         'excluded observation')
        self.assertEqual(self.assess(competitive=ratio(.1, .2), measured=False), 'inconclusive')
        self.assertEqual(self.assess(competitive=ratio(.1, .2), system='unknown'), 'inconclusive')

    def test_invalid_or_unmatched_statistics_cannot_supply_a_margin(self):
        for bad in (ratio(.1, math.nan), ratio(.1, math.inf), ratio(-1, .2),
                    ratio(.3, .2), ratio(True, .2), ratio(.1, .2, 8),
                    dict(ci95=[.1, .2]), dict(ci95=[.1], pairs=9)):
            self.assertNotEqual(self.assess(competitive=bad), 'allowed with Howard margin')
            self.assertEqual(self.assess(first=bad, competitive=ratio(.1, .2)), 'inconclusive')
        self.assertEqual(self.assess(competitive=ratio(.1, .2), second=ratio(.1, math.nan)),
                         'inconclusive')

    def test_rows_remain_independent_and_do_not_imply_merge_eligibility(self):
        statuses = [self.assess(competitive=ratio(.1, .2)),
                    self.assess(competitive=ratio(.95, 1.1), second=ratio(1.06, 1.08))]
        self.assertEqual(statuses, ['allowed with Howard margin', 'exceeds regression limit'])

    def test_collectors_emit_a_separate_review_without_rewriting_observations(self):
        arithmetic = dict(name='work', preset='full', comparison='paired',
                          ratio=ratio(1.06, 1.1), confirmation=ratio(1.06, 1.09),
                          candidate_over_howard=ratio(.1, .2),
                          aa_control_status='pass', status='confirmed regression')
        math_row = dict(name='work', preset='full', parameters=dict(mode='repeated'),
                        candidate_over_base=ratio(1.06, 1.1),
                        regression_confirmation=ratio(1.06, 1.09),
                        candidate_over_standard=ratio(.1, .2),
                        candidate_over_portable=ratio(.95, 1.1), aa_status='pass',
                        duration_target_met={'gd-portable': True, 'gd-base': True,
                                             'howard-standard': True, 'howard-portable': False},
                        regression_status='confirmed regression')
        for name, row in [('benchmark-ci', arithmetic), ('benchmark-math-ci', math_row)]:
            path = PATH.parent / (name + '.py')
            spec = importlib.util.spec_from_file_location(name.replace('-', '_'), str(path))
            collector = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(collector)
            original = copy.deepcopy(row)
            with mock.patch.object(collector.platform, 'system', return_value='Linux'):
                result = collector.acceptance_report([row], 9)
            self.assertEqual(result['results'][0]['acceptance'], 'allowed with Howard margin')
            self.assertEqual(result['results'][0]['raw_status'], 'confirmed regression')
            self.assertFalse(result['merge_eligible'])
            self.assertEqual(row, original)
        math_row['duration_target_met']['howard-standard'] = False
        result = POLICY.acceptance_review([math_row], 'Linux', 'math', 9)
        self.assertEqual(result['results'][0]['acceptance'], 'exceeds regression limit')
        math_row['duration_target_met']['howard-standard'] = True
        math_row['candidate_over_standard'] = ratio(.95, 1.1)
        math_row['candidate_over_portable'] = ratio(.1, .2)
        result = POLICY.acceptance_review([math_row], 'Linux', 'math', 9)
        self.assertEqual(result['results'][0]['acceptance'], 'exceeds regression limit')

    def test_control_and_fixed_duration_rows_keep_their_original_obligations(self):
        row = dict(name='work', comparison='paired', ratio=ratio(.99, 1.01, 31),
                   candidate_over_howard=None, fixed_samples=31, aa_control_status='pass',
                   status='pass')
        self.assertEqual(POLICY.acceptance_review([row], 'Linux', 'arithmetic', 9)
                         ['results'][0]['acceptance'], 'within regression limit')
        row['status'] = 'inconclusive-duration-shortfall'
        self.assertEqual(POLICY.acceptance_review([row], 'Linux', 'arithmetic', 9)
                         ['results'][0]['acceptance'], 'inconclusive')
        row['status'] = 'pass'
        row['comparison'] = 'aa'
        self.assertEqual(POLICY.acceptance_review([row], 'Linux', 'arithmetic', 9)
                         ['results'][0]['acceptance'], 'excluded observation')

    def test_math_keeps_baseline_pass_when_only_howard_is_too_short(self):
        row = dict(name='work', parameters=dict(mode='repeated'), aa_status='pass',
                   candidate_over_base=ratio(.99, 1.04),
                   candidate_over_standard=ratio(.1, .2),
                   duration_target_met={'gd-portable': True, 'gd-base': True,
                                        'howard-standard': False}, regression_status='pass')
        result = POLICY.acceptance_review([row], 'Linux', 'math', 9)
        self.assertEqual(result['results'][0]['acceptance'], 'within regression limit')


if __name__ == '__main__':
    unittest.main()
