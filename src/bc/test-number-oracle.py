#!/usr/bin/env python3
"""Exact rational arithmetic oracles for bc (bounded and replayable)."""

import argparse
from fractions import Fraction
import os
import random
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


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("bc")
    args, remaining = parser.parse_known_args()
    BC = args.bc
    unittest.main(argv=[__file__, *remaining])
