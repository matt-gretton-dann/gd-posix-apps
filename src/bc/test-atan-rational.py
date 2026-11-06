#!/usr/bin/env python3
"""Check the low-scale arctangent candidate with exact rational arithmetic.

This is test-side algebra and a fixed-decimal arithmetic model, not a runtime
oracle dependency. The alternating series supplies independent enclosures.
"""
from fractions import Fraction as F
from pathlib import Path
import re
import unittest


# Ascending coefficients. Their common scale cancels in P(z)/Q(z).
P = (305540235, 698377680, 552473922, 175855680, 19225635, 327680)
Q = (305540235, 800224425, 758107350, 312161850, 52026975, 2401245)
DEGREE = len(P) - 1
ORDER = 2 * DEGREE + 1
H = F(1, 81)
RECONSTRUCTION = 16


def polynomial(coefficients, value):
    result = F(0)
    for coefficient in reversed(coefficients):
        result = result * value + coefficient
    return result


def trunc(value, places):
    # int(Fraction) truncates toward zero, including negative products.
    factor = 10 ** places
    return F(int(value * factor), factor)


def decimal_kernel(value, places):
    squared = trunc(value * value, places)

    def horner(coefficients):
        result = F(coefficients[-1])
        for coefficient in reversed(coefficients[:-1]):
            result = trunc(result * squared, places) + coefficient
        return result

    return trunc(trunc(value * horner(P), places) / horner(Q), places)


def approximation_bound():
    # Q(z)*sum((-z)^k/(2*k+1))-P(z) vanishes through degree 2*DEGREE.
    # Bound every remaining coefficient convolution by a geometric tail.
    normalized = [F(q, Q[0]) for q in Q]
    return (F(RECONSTRUCTION, 9) * H ** ORDER / (1 - H) *
            sum(normalized[j] / (2 * (ORDER - j) + 1) for j in range(DEGREE + 1)))


def arithmetic_coefficient():
    # Q(z)>=Q[0]. Both finite Horner results also retain their positive
    # constant, so neither exact nor truncated division approaches a pole.
    pn = [F(p, Q[0]) for p in P]
    qn = [F(q, Q[0]) for q in Q]
    pmax = sum(pn)
    pd = sum(i * pn[i] for i in range(1, DEGREE + 1))
    qd = sum(i * qn[i] for i in range(1, DEGREE + 1))
    # Squaring changes z by <u; bound the derivative of P/Q on [0,H]
    # using the larger positive coefficient sums at z=1.
    argument = (pd + pmax * qd) / 9
    horner_error = 1 / (1 - H)
    # Two Horner errors, the numerator multiplication, then final division.
    evaluation = ((horner_error / 9 + 1) / Q[0] +
                  pmax * horner_error / (9 * Q[0]) + 1)
    return argument + evaluation


class RationalAtan(unittest.TestCase):
    def test_maintained_source_coefficients_are_bound_to_the_proof(self):
        source = (Path(__file__).resolve().parent / 'math-library.bc').read_text(encoding='utf-8')
        atan = source.split('define a(x) {', 1)[1].split('define e(x)', 1)[0]
        branch = re.search(r'if \(s<=20\) \{\n(.*?)\n  \}', atan, re.DOTALL)
        self.assertIsNotNone(branch, 'the proof admits only entry scales through 20')
        for variable, coefficients in (('t', P), ('u', Q)):
            initial = re.findall(r'(?m)^\s*' + variable + r'=(\d+)$', branch.group(1))
            updates = re.findall(r'(?m)^\s*' + variable + '=' + variable +
                                 r'\*q\+(\d+)$', branch.group(1))
            self.assertEqual(len(initial), 1)
            self.assertEqual(tuple(reversed(tuple(map(int, initial + updates)))), coefficients)

    def test_coefficients_match_the_independent_series_through_twice_the_degree(self):
        self.assertEqual(P[0], Q[0])
        self.assertTrue(all(coefficient > 0 for coefficient in P + Q))
        for power in range(ORDER):
            residual = sum(F(Q[j] * (-1) ** (power - j), 2 * (power - j) + 1)
                           for j in range(min(power, DEGREE) + 1))
            self.assertEqual(residual, P[power] if power <= DEGREE else 0)

    def test_whole_interval_approximation_and_decimal_budget(self):
        approximation = F(6779, 10 ** 25)
        self.assertLess(approximation_bound(), approximation)
        self.assertLess(approximation, F(68, 1000 * 10 ** 20))
        self.assertLess(arithmetic_coefficient(), 1024)
        for scale in range(21):
            # W>=S+26. (W+3)*10^-W decreases on this domain, so the
            # smallest permitted W maximizes the existing reduction bound.
            places = scale + 26
            unit = F(1, 10 ** places)
            reduction = 301 * (places + 3) * unit
            arithmetic = RECONSTRUCTION * 1024 * unit
            with self.subTest(scale=scale):
                self.assertLess(reduction + arithmetic + approximation,
                                F(1, 2 * 10 ** scale))

    def test_fixed_decimal_model_against_independent_alternating_enclosures(self):
        for numerator in range(-16, 17):
            value = F(numerator, 144)
            terms = [(-1) ** k * value ** (2 * k + 1) / (2 * k + 1)
                     for k in range(41)]
            partial = sum(terms[:-1])
            lower, upper = sorted((partial, partial + terms[-1]))
            for scale in (0, 1, 2, 19, 20):
                places = scale + 26
                stored = trunc(value, places)
                result = decimal_kernel(stored, places)
                error = max(abs(result - lower), abs(result - upper))
                # Include the initial truncation of the test input; atan is
                # 1-Lipschitz on the real line. This model does not substitute
                # for the full library's reciprocal/half-angle validation.
                bound = (approximation_bound() / RECONSTRUCTION +
                         (arithmetic_coefficient() + 1) / 10 ** places)
                with self.subTest(value=value, scale=scale):
                    self.assertLessEqual(error, bound)
                exact = stored * polynomial(P, stored * stored) / polynomial(Q, stored * stored)
                self.assertLessEqual(abs(result - exact),
                                     arithmetic_coefficient() / 10 ** places)


if __name__ == '__main__':
    unittest.main()
