#!/usr/bin/env python3
"""Check the portable arctangent's written contraction certificate exactly.

These are rational proof checks, not a second implementation of arctangent.
The numerical CLI/oracle tests separately exercise the interpreter and library.
SPDX-License-Identifier: Apache-2.0
"""
from fractions import Fraction as F
from pathlib import Path
import re
import unittest


UTILITIES = Path(__file__).resolve().parents[1]
ROOT = UTILITIES.parents[1]


class AtanProof(unittest.TestCase):
    def test_documented_ratio_bound_covers_the_left_endpoint(self):
        document = (UTILITIES / 'bc-portable-math.md').read_text(encoding='utf-8')
        match = re.search(r'stored squared\s+ratio\s+(?:is below|has magnitude at most)'
                          r'\s+([0-9./]+)', document)
        self.assertIsNotNone(match, 'the contraction proof must state its ratio bound')
        declared = F(match.group(1).rstrip('.'))
        # The near-one map is increasing on [4/5, 5/4]. Its left endpoint
        # is negative and does not enter the positive half-angle loop.
        endpoint = (F(4, 5)-1)/(F(4, 5)+1)
        self.assertEqual(endpoint, -F(1, 9))
        self.assertGreater(endpoint*endpoint, F(11, 1000))
        self.assertGreaterEqual(declared, endpoint*endpoint,
                                'the documented ratio misses the negative correction at .8')

    def test_source_still_has_the_certificates_reduction_premises(self):
        source = (ROOT / 'src/bc/math-library.bc').read_text(encoding='utf-8')
        body = source.split('define a(x) {', 1)[1].split('\ndefine e(x)', 1)[0]
        for premise in ('if (x>=.8)', 'if (x<=1.25)', 'x=(x-1)/(x+1)',
                        'while (x>.1)', 'q= -x*x',
                        'w=s+2*length(s+scale(x)+length(x)+1)+24'):
            self.assertIn(premise, body, 'changed reduction needs a new proof review')

    def test_power_term_and_tail_error_bounds(self):
        z, ratio = F(1, 9), F(1, 81)
        # E_(j+1) <= ratio*E_j + (1+|z|)*u, E_0=0.
        power = (1+z)/(1-ratio)
        divided_term = power+1
        tail_after_terminal_power = power*ratio/(1-ratio)
        self.assertEqual(power, F(9, 8))
        self.assertEqual(divided_term, F(17, 8))
        self.assertEqual(tail_after_terminal_power, F(9, 640))
        self.assertLess(tail_after_terminal_power, 2)
        # Stored multiplication truncates toward zero. At j=W+1,
        # |T_j|/u <= (1/729)*(10/81)^W <1 for every integer W>=0.
        self.assertLess(F(1, 729), 1)
        self.assertLess(F(10, 81), 1)

    def test_three_half_angles_and_reconstruction_budget(self):
        # W>=26; each stored half-angle differs from exact H by <2u.
        # Rational lower sqrt bounds give upper H bounds without floats.
        u = F(1, 10**26)
        for x, root_lower, target in (
                (F(1), F(141, 100), F(415, 1000)),
                (F(415, 1000), F(27, 25), F(1, 5)),
                (F(1, 5), F(101, 100), F(1, 10))):
            self.assertLess(root_lower*root_lower, 1+x*x)
            self.assertLess(x/(1+root_lower)+2*u, target)
        # H is 1/2-Lipschitz on nonnegative inputs; initial error <=u.
        argument_error = F(1)
        for _ in range(3):
            argument_error = argument_error/2+2
        self.assertLess(argument_error, 4)
        self.assertEqual(8*F(17, 8), 17)
        self.assertEqual(8*2+32+2, 50)
        # (17N+50)u <=301(N+1)u for all N>=0.
        self.assertLess(17, 301)
        self.assertLess(50, 301)

    def test_existing_guard_precision_has_rounding_headroom(self):
        # N<=W+1, T>=S+1 and d(T)<=T imply W+2<=29T.
        # T<10^d and d>=1 give the uniform pre-rounding bound below.
        self.assertLess(F(301*29, 10**25), F(1, 2))


if __name__ == '__main__':
    unittest.main()
