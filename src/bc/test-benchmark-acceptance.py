#!/usr/bin/env python3
"""Competitive-margin acceptance keeps observations and controls independent."""
import importlib.util
import math
from pathlib import Path
import unittest

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


if __name__ == '__main__':
    unittest.main()
