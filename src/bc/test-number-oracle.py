#!/usr/bin/env python3
"""Exact rational arithmetic oracles for bc (bounded and replayable)."""

import argparse
from fractions import Fraction
import os
import random
import subprocess
import unittest


SEED = 197199


def integer_sqrt(value):
    """Floor square root with integer Newton steps; also supports Python 3.6."""
    if value == 0:
        return 0
    root = 1 << ((value.bit_length() + 1) // 2)
    while True:
        next_root = (root + value // root) // 2
        if next_root >= root:
            return root
        root = next_root


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

    def test_scaled_arithmetic(self):
        rng = random.Random(SEED)
        pairs = [("0", "1"), ("1.23", "1.11"), ("5.3", "1.11"),
                 ("-5.3", "1.11"), ("5.3", "-1.11"), ("-5.3", "-1.11"),
                 ("999999999.999999999", ".000000001"),
                 ("1000000000000000001", "999999999"),
                 ("1000000000000000001000000001", "999999999999999999"),
                 ("123456789123456789123456789", "1000000000000000001")]
        for digits in (1, 8, 9, 10, 17, 18, 19, 27, 36, 99, 257, 1000, 1998, 2000):
            # Dense and asymmetric operands complement all-nine and sparse cases.
            a = "".join(str(rng.randrange(1, 10)) for _ in range(digits))
            b = "".join(str(rng.randrange(1, 10)) for _ in range(max(1, digits // 3)))
            balanced = "".join(str(rng.randrange(10)) for _ in range(digits - 1))
            balanced = str(rng.randrange(1, 10)) + balanced
            pairs.extend(((a, b), (b, a), (a, balanced), (a, a)))
        pairs.extend((("0.000", "1.000"), ("1", "100.00"),
                      (".000000000000000001", ".000000003"),
                      ("999999999.999999999", "999999999.999999998"),
                      ("-123.456789", "0.000000009"),
                      ("123.456789", "-0.000000009")))
        # Algebraic quotient-estimate boundaries at each supported limb radix.
        for base in (100, 10000, 1000000000):
            v = (base // 2) * base ** 2 + base + 1
            pairs.append((str(v * base - 1), str(v)))
            v = (base // 2) * base ** 2 + (base - 1) * base + 1
            pairs.append((str((base // 2) * base ** 3), str(v)))
            v = (base // 2) * base ** 2 + 1
            pairs.append((str(v * (base - 1) - 1), str(v)))
        for op in ("*", "/"):
            cases = []
            for a, b in pairs:
                for places in (0, 1, 2, 9, 18, 31):
                    s = min(scale(a) + scale(b), max(places, scale(a), scale(b))) if op == "*" else places
                    value = Fraction(a) * Fraction(b) if op == "*" else Fraction(a) / Fraction(b)
                    expected = truncate(value, s)
                    cases.append((literal(a) + op + literal(b), places, expected, s))
            with self.subTest(operator=op):
                self.assert_program(cases)

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

    def test_modulo(self):
        rng = random.Random(SEED)
        pairs = [("0", "1"), ("1.23", "1.11"), ("5.3", "1.11"),
                 ("-5.3", "1.11"), ("5.3", "-1.11"), ("-5.3", "-1.11"),
                 ("999999999.999999999", ".000000001"),
                 ("1000000000000000001", "999999999"),
                 ("1000000000000000001000000001", "999999999999999999"),
                 ("123456789123456789123456789", "1000000000000000001"),
                 ("0", "1.00"), ("1", "100.00"), ("5.000", "1")]
        for digits in (1, 8, 9, 10, 17, 18, 19, 27, 36, 99, 257, 1000):
            a = "".join(str(rng.randrange(1, 10)) for _ in range(digits))
            b = "".join(str(rng.randrange(1, 10)) for _ in range(max(1, digits // 3)))
            pairs.extend(((a, b), (b, a)))
        cases = []
        for a, b in pairs:
            for places in (0, 1, 2, 9, 18, 31):
                # Keep the entire quotient product: only the division truncates.
                expected = Fraction(a) - truncate(Fraction(a) / Fraction(b), places) * Fraction(b)
                s = max(scale(a), places + scale(b))
                cases.append((literal(a) + "%" + literal(b), places, expected, s))
        self.assert_program(cases)

    def test_square_roots(self):
        cases = []
        values = ["0", ".0000", "1", "2", "4", "3.999999999", "4.000000001",
                  ".00000000000000000001", "999999999999999999999999999999"]
        for digits in (3, 5, 10, 31, 100):
            square = 10 ** (2 * digits)
            values.extend(str(square + offset) for offset in (-1, 0, 1, 2, 3))
        for a in values:
            for places in (0, 1, 2, 9, 18, 50):
                s = max(places, scale(a))
                scaled = Fraction(a) * 10 ** (2 * s)
                root = integer_sqrt(scaled.numerator // scaled.denominator)
                expected = Fraction(root, 10 ** s)
                # Independent exact floor bounds, without using bc multiplication.
                self.assertLessEqual(expected ** 2, Fraction(a))
                self.assertGreater((expected + Fraction(1, 10 ** s)) ** 2, Fraction(a))
                cases.append(("sqrt(" + a + ")", places, expected, s))
        self.assert_program(cases)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("bc")
    args, remaining = parser.parse_known_args()
    BC = args.bc
    unittest.main(argv=[__file__, *remaining])
