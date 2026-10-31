#!/usr/bin/env python3
"""Exact rational power oracles for bc (bounded and replayable)."""

import argparse
from fractions import Fraction
import os
import subprocess
import unittest


SEED = 197199


def literal(value):
    return "(" + value + ")"


def scale(value):
    return len(value.split(".")[1]) if "." in value else 0


def truncate(value, places):
    factor = 10 ** places
    return Fraction(int(value * factor), factor)


def power_result(left, exponent, places):
    a, sa = Fraction(left), scale(left)
    s = places if exponent < 0 else min(sa * exponent, max(places, sa))
    return truncate(a ** exponent, s), s


def output_base(value, base, places):
    """Encode with enough radix digits to retain the given decimal precision."""
    sign = "-" if value < 0 else ""
    value = abs(value)
    whole = value.numerator // value.denominator
    fraction = value - whole
    digits = ""
    while whole:
        whole, digit = divmod(whole, base)
        digits = "0123456789ABCDEF"[digit] + digits
    digits = digits or "0"
    if places:
        digits += "."
        radix_power = 1
        while radix_power < 10 ** places:
            fraction *= base
            digit = fraction.numerator // fraction.denominator
            fraction -= digit
            digits += "0123456789ABCDEF"[digit]
            radix_power *= base
    return sign + digits


class NumberOracle(unittest.TestCase):
    def assert_program(self, cases):
        program = []
        for expression, places, _, _ in cases:
            program.extend(("scale=" + str(places), expression, "scale(" + expression + ")"))
        try:
            run = subprocess.run([BC], input="\n".join(program) + "\n", universal_newlines=True,
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=20,
                                 env=dict(os.environ, LC_ALL="C"))
        except subprocess.TimeoutExpired:
            self.fail("oracle batch timed out (seed {})".format(SEED))
        self.assertEqual(run.returncode, 0, run.stderr)
        self.assertEqual(run.stderr, "")
        # bc wraps long decimal output using backslash-newline continuations.
        lines = run.stdout.replace("\\\n", "").splitlines()
        self.assertEqual(len(lines), 2 * len(cases))
        for i, (expr, places, expected, expected_scale) in enumerate(cases):
            with self.subTest(seed=SEED, expression=expr, scale=places):
                self.assertEqual(Fraction(lines[2 * i]), expected)
                self.assertEqual(int(lines[2 * i + 1]), expected_scale)

    def test_powers(self):
        cases = []
        for a in ("0", "1", "-1", "2", "-2", "1.23", "-1.23", ".001", "123456789"):
            for exponent in (-7, -2, -1, 0, 1, 2, 3, 16, 31):
                if a == "0" and exponent < 0:
                    continue
                for places in (0, 2, 9, 31):
                    expected, s = power_result(a, exponent, places)
                    cases.append((literal(a) + "^" + literal(str(exponent)), places, expected, s))
        self.assert_program(cases)

    def test_addition_and_subtraction_scales(self):
        cases = []
        for a in ("0", "0.000", "-0.000", "1", "-1", "1.25", "-1.25", ".000000001"):
            for b in ("0", "0.000", "-0.000", "1", "-1", "1.25", "-1.25"):
                for op in ("+", "-"):
                    expected = Fraction(a) + (Fraction(b) if op == "+" else -Fraction(b))
                    cases.append((literal(a) + op + literal(b), 0, expected, max(scale(a), scale(b))))
        self.assert_program(cases)

    def test_zero_terms_preserve_expression_precision(self):
        # POSIX counterpart of the imported Howard "misc" fixture. Its saved
        # answers drop scale after subtracting a tiny power truncated to zero.
        expression = "4.1*1.-13^ - 74 - 1284597623841*1.-13^ - 757"
        continued = expression.replace("1.-13", "1.\\\n-1\\\n3")
        program = expression + "\n" + continued + "\nobase=9\n" + expression + "\n" + continued + "\n"
        value = Fraction(41, 10) - truncate(Fraction(13) ** -74, 20) - 1284597623841
        value -= truncate(Fraction(13) ** -757, 20)
        expected = [output_base(value, 10, 20)] * 2 + [output_base(value, 9, 20)] * 2
        try:
            run = subprocess.run([BC, "-l"], input=program, universal_newlines=True,
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=20,
                                 env=dict(os.environ, LC_ALL="C"))
        except subprocess.TimeoutExpired:
            self.fail("zero-term precision test timed out")
        self.assertEqual(run.returncode, 0, run.stderr)
        self.assertEqual(run.stderr, "")
        self.assertEqual(run.stdout.replace("\\\n", "").splitlines(), expected)



if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("bc")
    args, remaining = parser.parse_known_args()
    BC = args.bc
    unittest.main(argv=[__file__, *remaining])
