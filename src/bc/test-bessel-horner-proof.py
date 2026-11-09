#!/usr/bin/env python3
"""Certify finite Bessel tail and modeled decimal truncation bounds."""
import argparse
from fractions import Fraction
import json
from math import factorial
from pathlib import Path


def truncate(value, scale):
    factor = 10 ** scale
    magnitude = abs(value.numerator) * factor // value.denominator
    return Fraction(magnitude if value >= 0 else -magnitude, factor)


def polynomial(q, order, last):
    value = Fraction(1)
    for index in range(last, 0, -1):
        value = 1 - q * value / (index * (order + index))
    return value


def tail_bound(last):
    first = Fraction(2 * 4 ** (last + 1), factorial(last + 1) ** 2)
    contraction = Fraction(4, (last + 2) ** 2)
    return first / (1 - contraction)


def check_bounds():
    # exp(4) bounds absolute recurrence weights, the polynomial and its
    # coefficient derivative. Bound its positive tail with exact rationals.
    partial = sum((Fraction(4 ** j, factorial(j)) for j in range(17)), Fraction())
    remainder = Fraction(4 ** 17, factorial(17)) / (1 - Fraction(4, 18))
    assert partial + remainder < 64

    # Squared-coefficient error <4u; each Horner step contributes <=2u.
    polynomial_error = 2 * 64 + 4 * 64
    order_one_error = 2 * polynomial_error + Fraction(64, 2) + Fraction(1, 2) + 1
    assert polynomial_error == 384 and order_one_error < 1024
    for scale in range(101):
        last = 16 + (scale + 1) // 2
        tail = tail_bound(last)
        assert tail < Fraction(1, 10 ** (scale + 12)), scale
        assert tail + Fraction(1024, 10 ** (scale + 16)) < Fraction(1, 10 ** (scale + 11)), scale


def check_models():
    points = ['0', '0.0000000000000000001', '1', '1.125', '2',
              '3.8317059702075123156144358863081607665645452742878',
              '3.' + '9' * 50, '4']
    rows = []
    for scale in (0, 1, 2, 20, 100):
        last, working = 16 + (scale + 1) // 2, scale + 16
        unit = Fraction(1, 10 ** working)
        tail = tail_bound(last)
        for order in (0, 1):
            for literal in points:
                original = Fraction(literal)
                x = truncate(original, working)
                q = truncate(truncate(x * x, working) / 4, working)
                result = Fraction(1)
                for index in range(last, 0, -1):
                    product = truncate(q * result, working)
                    result = 1 - truncate(product / (index * (order + index)), working)
                exact = polynomial(original * original / 4, order, last)
                if order == 1:
                    result = truncate(truncate(result * x, working) / 2, working)
                    exact *= original / 2
                assert abs(result - exact) <= 1024 * unit

                quantum = Fraction(1, 10 ** scale)
                margin = quantum / 10 ** 11
                shifted = abs(result) + quantum / 2
                rounded = truncate(shifted, scale)
                remainder = shifted - rounded
                accepts = margin < remainder < quantum - margin
                if accepts:
                    signed = rounded if result >= 0 else -rounded
                    assert max(abs(signed - (exact - tail)),
                               abs(signed - (exact + tail))) <= quantum
                rows.append(dict(scale=scale, order=order, input=literal, K=last, W=working,
                                 rounding_window_accepts=accepts,
                                 arithmetic_error_bound_checked=True,
                                 ULP_enclosure_checked_if_accepted=accepts))
    return rows


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    check_bounds()
    rows = check_models()
    report = dict(scope='Exact-rational proof/model only; not runtime coverage, performance or '
                        'adoption proof. Exposed diagnostic inputs, not fresh timing holdouts.',
                  certified_scales=101, cases=len(rows),
                  accepted_window_cases=sum(row['rounding_window_accepts'] for row in rows),
                  rows=rows)
    if args.output:
        args.output.write_text(json.dumps(report, indent=2) + '\n')
    print('101 scale bounds and {} rational model cases checked; no bc process or elapsed clock'.format(len(rows)))


if __name__ == '__main__':
    main()
