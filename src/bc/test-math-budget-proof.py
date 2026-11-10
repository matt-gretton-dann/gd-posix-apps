#!/usr/bin/env python3
"""Check residual guard inequalities against the written math error estimates.

These rational checks bind the revised source to the estimates documented in
bc-portable-math.md. They supplement independent numerical enclosures and the
operation-by-operation Bessel model; they are not a whole-program proof.
SPDX-License-Identifier: Apache-2.0
"""
from fractions import Fraction as F
from math import factorial
from pathlib import Path
import unittest


ROOT = Path(__file__).resolve().parent


class MathBudgetProof(unittest.TestCase):
    def test_source_uses_the_reviewed_budgets(self):
        source = (ROOT / 'math-library.bc').read_text(encoding='utf-8')
        self.assertEqual(source.count('+12'), 6)
        self.assertIn('w=s+2*length(t)+12\n  if (m<0) w-=m', source)
        self.assertIn('w=s+g+h+2*length(s+g+h+1)+10\n  if (s==0) w+=22', source)
        self.assertIn('w=s+scale(x)+length(x)+2*length(s+scale(x)+length(x)+length(n)+1)+16', source)
        self.assertIn('if (g<w+16) g=w+16', source)
        logarithm = source.split('define l(x) {', 1)[1].split('\ndefine s(x)', 1)[0]
        self.assertLess(logarithm.index('if (h==1)'),
                        logarithm.index('if (scale(x)>w) x=x/1'))

    def test_residual_pre_rounding_bounds_are_below_half_an_output_unit(self):
        # Bounds already include conditioning/growth guards and T<10^d(T).
        # W+1<=29T, W+10<=37T and Bessel W+1<=19T remain conservative
        # when only the nonnegative residual precision is decreased.
        bounds = {
            'atan': F(32809, 10**13),
            'atan near one': F(8729, 10**13),
            'exp including initial clipping': F(6000, 10**12),
            'ordinary log including initial clipping': F(74*10**7, 10**12),
            'normalized log': F(37*10**6+1, 10**13),
            'sine and cosine': F(16, 10**14)+F(37*10**6, 10**14)+F(1, 10**15),
            'positive ascending Bessel': F(2*10**6*19**2, 10**11),
            'Hankel Bessel': F(10**6*19**2, 10**16),
        }
        for path, bound in bounds.items():
            with self.subTest(path=path):
                self.assertGreater(bound, 0)
                self.assertLess(bound, F(1, 2))
        self.assertEqual(max(bounds.values()), F(361, 50000))
        # The existing exponential estimate plus one initial coercion fits
        # the stated 6000*T^2*u residual coefficient.
        self.assertLess(16*(174+8)+1, 6000)

    def test_log_root_count_survives_the_smaller_precision(self):
        # W>=K+14. The geometric sum of logarithmic root perturbations
        # is <4*10^(K/2)*10^(-K-14), hence below 1/1000.
        self.assertLess(F(4, 10**14), F(1, 1000))
        self.assertLess(-F(1, 20)+F(1, 1000), -F(1, 25))
        # exp(7/3)>10 certifies log(10)<7/3 without floating point.
        self.assertGreater(sum((F(7, 3)**i/factorial(i) for i in range(11)), F()), 10)
        self.assertLess(50*F(7, 3), 128)
        # log(1+t)>=t/(1+t), so log(1.05)>=1/21.
        self.assertEqual(F(1, 20)/(1+F(1, 20)), F(1, 21))
        self.assertLess(42*F(7, 3), 128)

    def test_hankel_selector_remains_exactly_the_old_selector(self):
        # At scale zero these are unbounded integer additions. Evaluation
        # loses sixteen residual places, but selection retains all 32.
        for base in (0, 1, 20, 1000, 10**20):
            for growth in (0, base+31, base+32, base+33, 10**40):
                with self.subTest(base=base, growth=growth):
                    self.assertEqual(max(growth, base+16+16), max(growth, base+32))

    def test_default_unit_literals_preserve_the_callers_base(self):
        source = (ROOT / 'math-library.bc').read_text(encoding='utf-8')
        body = source.split('if (scale==20) {', 1)[1].split('  n=0', 1)[0]
        # Each decimal literal must be evaluated before restoring ibase.
        for branch, literal in (('if (x==1)', 'r=.78539816339744830962'),
                                ('if (x== -1)', 'r= -.78539816339744830962')):
            arm = body.split(branch, 1)[1].split('}', 1)[0]
            self.assertLess(arm.index(literal), arm.index('ibase=b'))
            self.assertIn('return(r)', arm)
        # Its numerical enclosure is independently checked by the CLI test's
        # exact Machin certificate, including both signs and caller bases.


if __name__ == '__main__':
    unittest.main()
