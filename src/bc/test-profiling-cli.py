#!/usr/bin/env python3
"""Check diagnostic counters independently of noisy process timings."""
import argparse
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


class ProfilingCli(unittest.TestCase):
    def invoke(self, program, output=None):
        environment = {key: value for key, value in os.environ.items()
                       if not key.startswith(('BC_', 'DC_', 'GD_BC_PROFILE_'))}
        environment['LC_ALL'] = 'C'
        if output is not None:
            environment['GD_BC_PROFILE_OUTPUT'] = str(output)
        return subprocess.run([BC], input=program, stdout=subprocess.PIPE,
                              stderr=subprocess.PIPE, universal_newlines=True,
                              env=environment, timeout=5)

    def profile(self, program, expected):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / 'profile \u03bb.json'
            result = self.invoke(program, output)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout, expected)
            self.assertEqual(result.stderr, '')
            counters = json.loads(output.read_text(encoding='utf-8'))
        self.assertEqual(counters['schema'], 1)
        self.assertTrue(counters['instrumented'])
        self.assertIn('not process malloc or RSS', counters['scope'])
        total = counters['total']
        self.assertEqual(total['live_bytes'], 0)
        self.assertEqual(total['allocation_calls'], total['deallocation_calls'])
        self.assertGreater(total['peak_live_bytes'], 0)
        self.assertGreater(counters['multiply_split_point'], 0)
        if EXPECTED_SPLIT is not None:
            self.assertEqual(counters['multiply_split_point'], EXPECTED_SPLIT)
        for values in counters['allocations'].values():
            self.assertEqual(values['live_bytes'], 0)
            self.assertLessEqual(values['peak_live_bytes'], total['peak_live_bytes'])
        self.assertEqual(sum(v['allocated_bytes'] for v in counters['allocations'].values()),
                         total['allocated_bytes'])
        return counters

    def test_disabled_output_preserves_cli(self):
        result = self.invoke('1+1\n')
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, '2\n', ''))

    def test_arithmetic_and_ownership(self):
        # Exceed the inline capacity so this fixture continues to exercise COW.
        value = 123456789012345678901234567890
        counters = self.profile('a={}\nb=a\na+=1\na*b\nb\n'.format(value),
                                '{}\n{}\n'.format((value + 1) * value, value))
        for event in ('digit_copies', 'owned_digit_copies', 'cow_clones', 'cow_limb_bytes',
                      'literal_conversions', 'basic_multiply_calls',
                      'instructions_executed'):
            self.assertGreater(counters['events'][event], 0, event)
        # Mutable iterator checks borrow their caller-owned vector.
        self.assertEqual(counters['events']['iterator_owner_copies'], 0)

    def test_power_has_no_unused_terminal_square(self):
        # Small exact powers isolate binary exponentiation from recursive
        # multiplication; count both result products and necessary squares.
        for exponent, expected_calls in ((0, 0), (1, 0), (2, 2), (3, 3), (16, 5), (31, 9)):
            with self.subTest(exponent=exponent):
                counters = self.profile('2^{}\n'.format(exponent),
                                        '{}\n'.format(2 ** exponent))
                self.assertEqual(counters['events']['basic_multiply_calls'], expected_calls)
                self.assertEqual(counters['events']['karatsuba_multiply_calls'], 0)
        # Exact value and decimal length checks for a large base. Exponent one
        # already returns early in BasicNumber, regardless of the threshold.
        digits = '7' * 1000
        counters = self.profile('a={}\nb=a^1\nb-a\nlength(b)\n'.format(digits), '0\n1000\n')
        self.assertEqual(counters['events']['basic_multiply_calls'], 0)
        self.assertEqual(counters['events']['karatsuba_multiply_calls'], 0)

    def test_array_clone_and_frames(self):
        counters = self.profile('define f(a[]) {\na[1]=9\nreturn(a[1])\n}\na[1]=3\n'
                                'a[2]=123456789012345678901234567890\nf(a[])\na[1]\n',
                                '9\n3\n')
        self.assertGreater(counters['events']['function_calls'], 0)
        self.assertGreater(counters['events']['array_clones'], 0)
        self.assertGreater(counters['events']['array_entries_copied'], 0)
        for category in ('arrays', 'frames', 'instructions', 'results', 'digits'):
            self.assertGreater(counters['allocations'][category]['allocation_calls'], 0, category)

    def test_unwritable_output_reports_failure(self):
        with tempfile.TemporaryDirectory() as directory:
            result = self.invoke('2\n', Path(directory) / 'missing' / 'profile.json')
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('cannot open profiling output', result.stderr)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('bc')
    parser.add_argument('--split-point', type=int)
    args, remaining = parser.parse_known_args()
    BC = str(Path(args.bc).resolve())
    EXPECTED_SPLIT = args.split_point
    unittest.main(argv=[__file__, *remaining])
