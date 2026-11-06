#!/usr/bin/env python3
"""Check the low-scale arctangent candidate with exact rational arithmetic.

This is test-side algebra and a fixed-decimal arithmetic model, not a runtime
oracle dependency. The alternating series supplies independent enclosures.
"""
from fractions import Fraction as F
import unittest


# Ascending coefficients. Their common scale cancels in P(z)/Q(z).
P = (58561878375, 163192434405, 169269290190, 80191217106,
     16979477515, 1296036105, 15728640)
Q = (58561878375, 182713060530, 218461268025, 124835010300,
     34493884425, 4058104050, 135270135)
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
    # Q(z)*sum((-z)^k/(2*k+1))-P(z) vanishes through degree 12.
    # Bound every remaining coefficient convolution by a geometric tail.
    normalized = [F(q, Q[0]) for q in Q]
    return (F(RECONSTRUCTION, 9) * H ** 13 / (1 - H) *
            sum(normalized[j] / (2 * (13 - j) + 1) for j in range(7)))


def arithmetic_coefficient():
    # Q(z)>=Q[0]. Both finite Horner results also retain their positive
    # constant, so neither exact nor truncated division approaches a pole.
    pn = [F(p, Q[0]) for p in P]
    qn = [F(q, Q[0]) for q in Q]
    pmax = sum(pn)
    pd = sum(i * pn[i] for i in range(1, 7))
    qd = sum(i * qn[i] for i in range(1, 7))
    # Squaring changes z by <u; bound the derivative of P/Q on [0,H]
    # using the larger positive coefficient sums at z=1.
    argument = (pd + pmax * qd) / 9
    horner_error = 1 / (1 - H)
    # Two Horner errors, the numerator multiplication, then final division.
    evaluation = ((horner_error / 9 + 1) / Q[0] +
                  pmax * horner_error / (9 * Q[0]) + 1)
    return argument + evaluation


class RationalAtan(unittest.TestCase):
    def test_coefficients_match_the_independent_series_through_degree_twelve(self):
        self.assertEqual(P[0], Q[0])
        self.assertTrue(all(coefficient > 0 for coefficient in P + Q))
        for power in range(13):
            residual = sum(F(Q[j] * (-1) ** (power - j), 2 * (power - j) + 1)
                           for j in range(min(power, 6) + 1))
            self.assertEqual(residual, P[power] if power < 7 else 0)

    def test_whole_interval_approximation_and_decimal_budget(self):
        approximation = approximation_bound()
        self.assertLess(approximation, F(1, 65536 * 10 ** 20))
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
