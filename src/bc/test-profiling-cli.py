#!/usr/bin/env python3
"""Check diagnostic counters independently of noisy process timings."""
import argparse
import json
import os
from pathlib import Path
import subprocess
import sys
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
        self.assertEqual(counters['events']['result_validation_calls'], 0)

    def test_power_has_no_unused_terminal_square(self):
        # Small exact powers isolate binary exponentiation from recursive
        # multiplication; count both result products and necessary squares.
        for exponent, expected_calls in ((0, 0), (1, 0), (2, 2), (3, 3), (16, 5), (31, 9)):
            with self.subTest(exponent=exponent):
                counters = self.profile('2^{}\n'.format(exponent),
                                        '{}\n'.format(2 ** exponent))
                self.assertEqual(counters['events']['basic_multiply_calls'] +
                                 counters['events']['square_multiply_calls'], expected_calls)
                self.assertEqual(counters['events']['karatsuba_multiply_calls'], 0)
        # Exact value and decimal length checks for a large base. Exponent one
        # already returns early in BasicNumber, regardless of the threshold.
        digits = '7' * 1000
        counters = self.profile('a={}\nb=a^1\nb-a\nlength(b)\n'.format(digits), '0\n1000\n')
        self.assertEqual(counters['events']['basic_multiply_calls'], 0)
        self.assertEqual(counters['events']['karatsuba_multiply_calls'], 0)
        self.assertEqual(counters['events']['square_multiply_calls'], 0)

    def test_blocked_products_count_as_schoolbook_leaves(self):
        for digits in (100, 1000):
            with self.subTest(digits=digits):
                a, b = '7' * digits, '8' * digits
                expected = str(int(a) * int(b))
                counters = self.profile('a={};b={};r=a*b;r-{}\n'.format(a, b, expected),
                                        '0\n')
                blocked = counters['events']['blocked_multiply_calls']
                self.assertGreater(blocked, 0)
                self.assertLessEqual(blocked, counters['events']['basic_multiply_calls'])

    def test_shared_coefficient_square_dispatch(self):
        # Multi-limb operands avoid the single-limb multiply-accumulate path.
        for digits in (40, 2000):
            with self.subTest(digits=digits):
                value = '9' * digits
                counters = self.profile('a={}\nb=a*a\nb-a*a\nlength(b)\n'.format(value),
                                        '0\n{}\n'.format(2 * digits))
                self.assertGreater(counters['events']['square_multiply_calls'], 0)

    def test_literal_cache_survives_function_activations_and_base_changes(self):
        counters = self.profile('define f() {\nreturn (10)\n}\nf()\nf()\n'
                                'ibase=2\nf()\nf()\n', '10\n10\n2\n2\n')
        self.assertEqual(counters['events']['literal_cache_misses'], 2)
        self.assertEqual(counters['events']['literal_cache_hits'], 2)
        self.assertEqual(counters['events']['literal_conversions'], 2)
        # Changing the key reuses one bounded memo, not another cache entry.
        self.assertEqual(counters['allocations']['literals']['allocation_calls'], 1)
        self.assertEqual(counters['allocations']['literals']['deallocation_calls'], 1)

    def test_large_integer_radix_input_uses_bounded_dc_dispatch(self):
        if hasattr(sys, 'set_int_max_str_digits'):
            sys.set_int_max_str_digits(100000)
        for width, expected_dispatches in ((65535, 0), (65536, 1), (65537, 1)):
            with self.subTest(width=width):
                digits = '1' * width
                expected = str(int(digits, 2))
                counters = self.profile('ibase=2\na={}\nibase=A\na-{}\nlength(a)\n'.format(
                    digits, expected), '0\n{}\n'.format(len(expected)))
                self.assertEqual(counters['events'].get('radix_dc_input_calls', 0),
                                 expected_dispatches)
                leaves = counters['events'].get('radix_dc_input_leaf_conversions', 0)
                self.assertEqual(leaves > 0, expected_dispatches > 0)
        # A point retains the established grouped fractional conversion path.
        digits = '1' * 65536
        expected = str(int(digits, 2)) + '.5'
        counters = self.profile('ibase=2\na={}.1\nibase=A\na-{}\n'.format(digits, expected),
                                '0\n')
        self.assertEqual(counters['events'].get('radix_dc_input_calls', 0), 0)

    def test_verified_arithmetic_consumes_proven_last_use_temporaries(self):
        expected = '4' + '0' * 100
        wrapped = expected[:68] + '\\\n' + expected[68:] + '\n'
        counters = self.profile('define f(x) {\nreturn((x+x)+(x+x));\n}\nf(10^100)\n',
                                wrapped)
        self.assertGreater(counters['events'].get('temporary_consumptions', 0), 0)

    def test_inline_arithmetic_keeps_the_copy_fallback(self):
        counters = self.profile('define f(x) {\nreturn((x+x)+(x+x));\n}\nf(7)\n', '28\n')
        self.assertEqual(counters['events']['cow_clones'], 0)
        self.assertEqual(counters['events']['owned_digit_copies'], 0)
        self.assertEqual(counters['events'].get('temporary_consumptions', 0), 0)

    def test_single_digits_need_no_decoded_cache_allocation(self):
        counters = self.profile('0\n1\nA\nF\n', '0\n1\n10\n15\n')
        self.assertEqual(counters['events']['single_digit_literals'], 4)
        self.assertEqual(counters['events']['literal_conversions'], 0)
        self.assertEqual(counters['allocations']['literals']['allocation_calls'], 0)

    def test_comparison_positions_distinguish_early_and_full_scans(self):
        value = '7' * 1000
        for kind, right in (('high', '8' + value[1:]), ('low', value[:-1] + '8'),
                            ('equal', value)):
            with self.subTest(kind=kind):
                counters = self.profile('a={}\nb={}\nr=0\nif(a<b) r=1\nr\n'.format(value, right),
                                        '0\n' if kind == 'equal' else '1\n')
                width = len(str(counters['limb_base'])) - 1
                positions = 1 if kind == 'high' else (len(value) + width - 1) // width
                self.assertEqual(counters['events']['comparison_limb_positions'], positions)
                self.assertGreater(counters['events']['zero_scan_limbs'], 0)

    def test_sequential_calls_reuse_frame_capacity(self):
        definition = ('define f(a,b,c,d) {\nauto e,h\n'
                      'e=a+b;h=c+d;return(e+h)\n}\n')
        def run(count):
            return self.profile(definition +
                                'r=0;for(i=0;i<{};i++) r+=f(1,2,3,4)\nr\n'.format(count),
                                '{}\n'.format(10 * count))
        short, long = run(1), run(200)
        self.assertEqual(long['events']['function_calls'], 200)
        # Sequential activations of the same shape reuse frame storage after
        # warm-up. This checks allocation behavior, without a wall-time gate.
        self.assertEqual(long['allocations']['frames']['allocation_calls'],
                         short['allocations']['frames']['allocation_calls'])
        self.assertEqual(long['allocations']['results']['allocation_calls'],
                         short['allocations']['results']['allocation_calls'])

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
