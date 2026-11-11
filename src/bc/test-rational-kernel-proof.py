#!/usr/bin/env python3
"""Exact residual/roundoff budgets for bounded ordinary math kernels.

Independent rational models supplement certified CLI enclosures; they are not
whole-program proofs or performance evidence. See bc-bounded-rational-kernels.md.
SPDX-License-Identifier: Apache-2.0
"""
from fractions import Fraction as F
from math import factorial
from pathlib import Path
import re
import random
import unittest


KERNELS = {
    'atan': ([3828825, 6831825, 3738735, 638055, 16384],
             [3828825, 8108100, 5675670, 1455300, 99225], F(1, 9)),
    'atan-small': ([945, 735, 64], [945, 1050, 225], F(1, 100)),
    'sin': ([363275871831577908403200, -54839355237791393068800,
             2120649063015013090560, -31712777908498486800,
             203385425766914820, -481959816488503],
            [363275871831577908403200, 5706623400804924998400,
             44454031219351353600, 219578286347980560,
             707177798947620, 1230626892363], F(16, 25)),
    'cos': ([280669698675663897600, -135355106332109664000,
             9249069176002656000, -204260164593685200,
             1767200224481340, -5342316580775],
            [280669698675663897600, 4979743005722284800,
             44369900711136000, 254519795209680,
             969954677340, 2045322787], F(2, 5)),
    'log': ([305540235, -698377680, 552473922, -175855680, 19225635, -327680],
            [305540235, -800224425, 758107350, -312161850, 52026975, -2401245], F(1, 9)),
}
EXP = [518918400, 259459200, 60540480, 8648640, 831600, 55440, 2520, 72, 1]
HORNER = [(F(1, 2), 5, 30, 14), (F(1), 7, 53, 17), (F(2), 8, 64, 15),
          (F(4), 8, 92, 13), (F(8), 9, 166, 13), (F(16), 14, 393, 17),
          (F(32), 16, 748, 17), (F(64), 19, 1563, 18), (F(128), 15, 2430, 14)]


def polynomial(c, x):
    return sum((v*x**i for i, v in enumerate(c)), F())


def derivative(c, x):
    return sum((i*v*x**(i-1) for i, v in enumerate(c) if i), F())


def coefficient(kind, k):
    if kind.startswith('atan'):
        return F((-1)**k, 2*k+1)
    if kind == 'log':
        return F(1, 2*k+1)
    if kind == 'exp':
        return F(1, factorial(k))
    return F((-1)**k, factorial(2*k+(kind == 'sin')))


def residual_bound(kind, p, q, x):
    """Exact cancelled prefix plus a geometric bound for |Taylor a_k|<=1."""
    residual = []
    for k in range(65):
        residual.append(sum((v*coefficient(kind, k-j) for j, v in enumerate(q) if j <= k), F())
                        - (p[k] if k < len(p) else 0))
    if any(residual[:2*(len(q)-1)+1]):
        raise AssertionError('Pade residual does not cancel its declared prefix')
    return (sum((abs(v)*x**i for i, v in enumerate(residual)), F()) +
            sum(map(abs, q))*x**65/(1-x))


def certificate(kind):
    pi, qi, a = KERNELS[kind]
    p, q = [F(v, qi[0]) for v in pi], [F(v, qi[0]) for v in qi]
    z = a*a
    lower = 1-sum((-v)*z**i for i, v in enumerate(q) if v < 0)
    if lower <= 0:
        raise AssertionError('denominator lower bound is not positive')
    pa, qa = list(map(abs, p)), list(map(abs, q))
    d = 1/(1-z)
    if d*F(1, 10**15) >= qi[0]*lower/2:
        raise AssertionError('rounded denominator could cross the bound')
    error = (2*d/(qi[0]*lower) + 2*polynomial(pa, z)*d/(qi[0]*lower**2) + 1 +
             (derivative(pa, z)*polynomial(qa, z) +
              polynomial(pa, z)*derivative(qa, z))/lower**2)
    leading = kind != 'cos'
    if leading:
        error = a*error+1
    tail = residual_bound(kind, p, q, z)/lower*(a if leading else 1)
    return tail, error


def exp_certificate():
    p = [F(v, EXP[0]) for v in EXP]
    q = [v*(-1)**i for i, v in enumerate(p)]
    a, z = F(1, 8), F(1, 64)
    even, odd = EXP[::2], EXP[1::2]
    e, o = polynomial(even, z), a*polynomial(odd, z)
    lower = EXP[0]-o
    if lower <= F(9, 10)*EXP[0]:
        raise AssertionError('exponential denominator is too small')
    error = ((2*o/(1-z)+2*e*(a/(1-z)+1))/lower**2+1 +
             2*(e*a*derivative(odd, z)+o*derivative(even, z))/lower**2)
    tail = residual_bound('exp', p, q, a)/(lower/EXP[0])
    return tail, error


def trunc(a, b):
    return (1 if a*b >= 0 else -1)*(abs(a)//abs(b))


def fixed_polynomial(c, q, unit):
    value = c[-1]*unit
    for v in reversed(c[:-1]):
        value = trunc(value*q, unit)+v*unit
    return value


class RationalKernelProof(unittest.TestCase):
    def test_log_normalization_bounds_the_low_scale_root_multiplier(self):
        # Without normalization, |decimal exponent|<64+floor(S/2)<=74.
        # Normalization gives a mantissa in [1,10). Use a common log bound
        # 75*log(10)<175 and the unchanged <.001 root-perturbation bound.
        self.assertGreater(sum((F(7, 3)**i/factorial(i) for i in range(11)), F()), 10)
        # Positive atanh coefficients give log(1.2)>2/11 and
        # -log(.8)=log(1.25)>2/9. Ten roots therefore enter [.8,1.2].
        self.assertLess(F(175, 1024)+F(1, 1000), F(2, 11))
        self.assertLess(F(175, 1024)+F(1, 1000), F(2, 9))

    def test_exact_residuals_and_all_elementary_scale_budgets(self):
        for kind in KERNELS:
            tail, error = certificate(kind)
            self.assertLess(error, 4, kind)
            for scale in range(1, 21):
                if kind.startswith('atan'):
                    amplification, residual = 8, F(114, 10**14)
                elif kind == 'log':
                    amplification = 2048
                    residual = F(74, 100000)+2048*(4+F(81, 80))*F(1, 10**14)
                else:
                    amplification = 160 if kind == 'sin' else 125
                    residual = F(169, 10**15)+F(amplification*4+72, 10**14)
                self.assertLess(F(1, 2)+residual+amplification*tail*10**scale, F(505, 1000),
                                (kind, scale))
        tail, error = exp_certificate()
        self.assertLess(error, 4)
        for scale in range(1, 21):
            for k in range(8):
                for negative in (False, True):
                    for g in ([0] if negative else range(1, 10)):
                        unit = F(1, 10**(scale+g+14))
                        gain = 2**k*(1 if negative else 10**g)
                        total = F(1, 2)+gain*((8 if negative else 12)*unit +
                                             (1 if negative else 2)*tail)*10**scale
                        self.assertLess(total, F(505, 1000))
        self.assertLess(128*(F(4, 10**15)+tail), F(1, 1000))

    def test_signed_fixed_point_kernels_against_exact_rational_values(self):
        rng = random.Random(21719782)
        for kind, (p, q, a) in KERNELS.items():
            for scale in (15, 16, 34):
                unit = 10**scale
                samples = [F(), a, -a] + [a*F(rng.randrange(-10**12, 10**12), 10**12)
                                           for _ in range(32)]
                for x in samples:
                    xi = trunc(x.numerator*unit, x.denominator)
                    x = F(xi, unit)
                    square = xi*xi//unit
                    numerator = fixed_polynomial(p, square, unit)
                    denominator = fixed_polynomial(q, square, unit)
                    self.assertGreater(denominator, 0)
                    value = trunc(numerator*unit, denominator)
                    exact = polynomial(p, x*x)/polynomial(q, x*x)
                    if kind != 'cos':
                        value = trunc(xi*value, unit)
                        exact *= x
                    self.assertLess(abs(F(value, unit)-exact), F(4, unit))

    def test_bessel_compact_rules_cover_all_remaining_orders_and_scales(self):
        # Positive Taylor terms with a geometric remainder certify exp(16/7)<10.
        x = F(16, 7)
        upper = (sum((x**i/factorial(i) for i in range(49)), F()) +
                 x**49/factorial(49)/(1-x/50))
        self.assertLess(upper, 10)
        for a, slope, intercept, divisor in HORNER:
            q = a*a/4
            for scale in range(1, 21):
                degree = (slope*scale+intercept)//divisor
                nmax = max(-(-3*a.numerator//a.denominator), 4*(scale+1))-1
                growth = -(-(7*a/16).numerator//(7*a/16).denominator)
                digits = 0
                while 10**digits < 2*nmax+3:
                    digits += 1
                working = scale+growth+digits+4
                ratio = q/(degree+2)**2
                self.assertLess(ratio, 1)
                tail = q**(degree+1)/factorial(degree+1)**2/(1-ratio)
                arithmetic = F((2*nmax+3)*10**growth+4, 10**working)
                self.assertLess(tail*10**scale, F(1, 250))
                self.assertLess(F(1, 2)+(tail+arithmetic)*10**scale, F(505, 1000))

    def test_bessel_turning_degree_rules_match_source_and_certify_tail(self):
        source = Path(__file__).with_name('math-library.bc').read_text()
        start = source.find('Order-aware Bessel degree rules')
        self.assertNotEqual(start, -1)
        block = source[start:source.index('k=(h*s+p)/d', start)]
        entries = [tuple(map(int, match)) for match in re.findall(
            r'if \(x<=([0-9]+)\) \{h=([0-9]+);p=([0-9]+);d=([0-9]+)\}', block)]
        self.assertEqual([entry[0] for entry in entries], [128, 96, 64, 48, 32, 24])
        lower = 16
        for upper, slope, intercept, divisor in reversed(entries):
            q = F(upper**2, 4)
            for scale in range(1, 21):
                degree = (slope*scale+intercept)//divisor
                next_term = F(upper, 2)**lower*q**(degree+1)
                next_term /= factorial(degree+1)*factorial(lower+degree+1)
                self.assertLessEqual(F(upper, 2), lower+degree+2)
                ratio = q/((degree+2)*(lower+degree+2))
                self.assertLess(ratio, 1)
                tail = next_term/(1-ratio)
                self.assertLess(tail*10**scale, F(1, 250))
                self.assertLess(F(1, 2)+tail*10**scale+F(2, 10**4), F(505, 1000))
            lower = upper

    def test_exp_even_odd_evaluation_against_the_exact_rational_kernel(self):
        rng = random.Random(21719783)
        for working in (15, 16, 34):
            unit = 10**working
            for x in [F(), F(1, 8)] + [F(rng.randrange(10**12), 8*10**12)
                                       for _ in range(32)]:
                xi = trunc(x.numerator*unit, x.denominator)
                x = F(xi, unit)
                q = xi*xi//unit
                even = fixed_polynomial(EXP[::2], q, unit)
                odd = trunc(xi*fixed_polynomial(EXP[1::2], q, unit), unit)
                self.assertGreater(even, odd)
                actual = F((even+odd)*unit//(even-odd), unit)
                exact = polynomial(EXP, x)/polynomial(
                    [v*(-1)**i for i, v in enumerate(EXP)], x)
                self.assertGreaterEqual(actual, 1)
                self.assertLess(abs(actual-exact), F(4, unit))

    def test_weighted_horner_roundoff_against_the_exact_finite_polynomial(self):
        for a, slope, intercept, divisor in HORNER:
            for scale in (1, 20):
                degree = (slope*scale+intercept)//divisor
                nmax = max(-(-3*a.numerator//a.denominator), 4*(scale+1))-1
                growth = -(-(7*a/16).numerator//(7*a/16).denominator)
                digits = 0
                while 10**digits < 2*nmax+3:
                    digits += 1
                unit = 10**(scale+growth+digits+4)
                for n in sorted({0, 1, 2, nmax}):
                    for x in (a, a*F(7, 8)):
                        yi = trunc(x.numerator*unit, 2*x.denominator)
                        y = F(yi, unit)
                        q = yi*yi//unit
                        lead = unit
                        for i in range(1, n+1):
                            lead = trunc(trunc(lead*yi, unit), i)
                        exact_lead = y**n/factorial(n)
                        self.assertLessEqual(F(lead, unit), exact_lead)
                        value = unit
                        for k in range(degree, 0, -1):
                            value = unit-trunc(trunc(q*value, unit), k*(n+k))
                        actual = F(trunc(lead*value, unit), unit)
                        total, term = F(1), F(1)
                        for k in range(1, degree+1):
                            term *= -y*y/(k*(n+k))
                            total += term
                        exact = exact_lead*total
                        self.assertLessEqual(abs(actual-exact),
                            F((2*n+3)*10**growth+1, unit))


if __name__ == '__main__':
    unittest.main()
