#!/usr/bin/env python3
"""Check VM admission evidence without interpreting instrumented elapsed times."""
import argparse
from fractions import Fraction
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

SPEC = importlib.util.spec_from_file_location(
    'divisor_oracles', Path(__file__).with_name('test-divisor-reuse-cli.py'))
ORACLE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(ORACLE)
EVENTS = ('divisor_cache_lookups', 'divisor_cache_fingerprint_limbs',
          'divisor_cache_preparation_attempts', 'divisor_cache_promotions', 'divisor_cache_hits')


class Profile(unittest.TestCase):
    def profile(self, program, expected):
        with tempfile.TemporaryDirectory() as temp:
            output = Path(temp) / 'profile.json'
            environment = {key: value for key, value in os.environ.items()
                           if not key.startswith(('BC_', 'DC_', 'GD_BC_PROFILE_'))}
            environment.update(LC_ALL='C', GD_BC_PROFILE_OUTPUT=str(output))
            child = subprocess.run([BC], input=program, universal_newlines=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                   timeout=5, env=environment)
            self.assertEqual(child.returncode, 0, child.stderr)
            self.assertEqual(child.stderr, '')
            self.assertEqual([Fraction(token) for token in child.stdout.replace('\\\n', '').splitlines()],
                             expected)
            report = json.loads(output.read_text())
        self.assertIs(report['instrumented'], True)
        self.assertEqual(report['total']['live_bytes'], 0)
        self.assertEqual(report['total']['allocation_calls'], report['total']['deallocation_calls'])
        for values in report['allocations'].values():
            self.assertEqual(values['live_bytes'], 0)
            self.assertEqual(values['allocation_calls'], values['deallocation_calls'])
        for name in EVENTS:
            self.assertGreaterEqual(report['events'][name], 0)
        return report

    def check_disabled(self, report):
        for name in EVENTS:
            self.assertEqual(report['events'][name], 0)
        self.assertEqual(report['allocations']['divisors']['allocation_calls'], 0)

    def test_stable_owner_reaches_the_prepared_hit_path(self):
        count = 12
        a, b = ORACLE.A, ORACLE.B
        report = self.profile(f'a={a}\nb={b}\nscale=7\n'
                              f'for(i=0;i<{count};i++) {{\nq=a/b\nr=a%b\n}}\nq\nr\n',
                              ORACLE.pair(Fraction(a), Fraction(b), 7)[::2])
        self.assertEqual(report['events']['division_requests'], 2 * count)
        if not ENABLED:
            self.check_disabled(report)
            return
        events = report['events']
        self.assertEqual(events['divisor_cache_lookups'], 2 * count)
        self.assertEqual(events['divisor_cache_preparation_attempts'], 1)
        self.assertEqual(events['divisor_cache_promotions'], 1)
        self.assertEqual(events['divisor_cache_hits'], 2 * count - 3)
        digits_per_limb = len(str(report['limb_base'])) - 1
        limbs = (len(str(b)) + digits_per_limb - 1) // digits_per_limb
        self.assertEqual(events['divisor_cache_fingerprint_limbs'], 2 * limbs)
        metadata = report['allocations']['divisors']
        self.assertEqual(metadata['allocation_calls'], 1)
        self.assertGreater(metadata['allocated_bytes'], 0)
        self.assertEqual(metadata['allocated_bytes'], metadata['peak_live_bytes'])

    def test_fresh_temporaries_do_not_prepare_or_scan(self):
        count = 12
        a, b = ORACLE.A, ORACLE.B
        report = self.profile(f'a={a}\nb={b}\nscale=7\n'
                              f'for(i=1;i<={count};i++) {{\nq=a/(b+i)\nr=a%(b+i)\n}}\nq\nr\n',
                              ORACLE.pair(Fraction(a), Fraction(b + count), 7)[::2])
        self.assertEqual(report['events']['division_requests'], 2 * count)
        if not ENABLED:
            self.check_disabled(report)
            return
        # A unique temporary never reaches admission or allocates cache metadata.
        self.check_disabled(report)

    def test_unique_request_clears_old_entry_before_stable_readmission(self):
        a, b = ORACLE.A, ORACLE.B
        report = self.profile(f'a={a}\nb={b}\nscale=7\n'
                              'for(i=0;i<4;i++) q=a/b\nq\n'
                              'q=a/(b+1)\nq\n'
                              'for(i=0;i<4;i++) q=a/b\nq\n',
                              [ORACLE.pair(Fraction(a), Fraction(b), 7)[0],
                               ORACLE.pair(Fraction(a), Fraction(b + 1), 7)[0],
                               ORACLE.pair(Fraction(a), Fraction(b), 7)[0]])
        self.assertEqual(report['events']['division_requests'], 9)
        if not ENABLED:
            self.check_disabled(report)
            return
        events = report['events']
        self.assertEqual(events['divisor_cache_lookups'], 8)
        self.assertEqual(events['divisor_cache_preparation_attempts'], 2)
        self.assertEqual(events['divisor_cache_promotions'], 2)
        self.assertEqual(events['divisor_cache_hits'], 2)
        self.assertEqual(report['allocations']['divisors']['allocation_calls'], 1)

    def test_scalar_requests_do_not_allocate_cache_metadata(self):
        report = self.profile('scale=0\nfor(i=0;i<6;i++) q=9/2\nr=9%2\nq\nr\n',
                              [Fraction(4), Fraction(1)])
        self.assertEqual(report['events']['division_requests'], 7)
        self.check_disabled(report)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('bc')
    parser.add_argument('--enabled', choices=('ON', 'OFF'), required=True)
    args, remainder = parser.parse_known_args()
    BC = args.bc
    ENABLED = args.enabled == 'ON'
    unittest.main(argv=[__file__, *remainder])
