#!/usr/bin/env python3
"""Assert VM behavior and runtime failures in bounded, isolated processes."""

import argparse
import os
import subprocess
import unittest


class VMTests(unittest.TestCase):
    def run_bc(self, text, output="", success=True, options=(), diagnostic=None):
        try:
            run = subprocess.run([BC, *options], input=text, universal_newlines=True,
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=2,
                                 env=dict(os.environ, LC_ALL="C"))
        except subprocess.TimeoutExpired:
            self.fail("VM did not terminate within two seconds")
        self.assertEqual(run.stdout, output)
        self.assertEqual(run.returncode, 0 if success else 1, run.stderr)
        if success:
            self.assertEqual(run.stderr, "")
        else:
            self.assertNotEqual(run.stderr, "")
        if diagnostic is not None:
            self.assertIn(diagnostic, run.stderr)

    def test_runtime_numeric_errors(self):
        for text, diagnostic in (("1/0\n", "divide by zero"),
                                 ("2%0\n", "divide by zero"),
                                 ("sqrt(-1)\n", "square root of the negative number"),
                                 ("ibase=1\n", "not a valid ibase"),
                                 ("ibase=17\n", "not a valid ibase"),
                                 ("ibase = -2\n", "it is negative"),
                                 ("obase=1\n", "not a valid obase"),
                                 ("obase = -2\n", "it is negative"),
                                 ("scale = -1\n", "it is negative"),
                                 ("a[-1]=1\n", "it is negative")):
            with self.subTest(text=text):
                self.run_bc(text, success=False, diagnostic=diagnostic)

    def test_fractional_power_identifies_exponent(self):
        for text, exponent in (("2^1.5\n", "1.5"), ("3^(-1.25)\n", "-1.25")):
            with self.subTest(text=text):
                self.run_bc(text, success=False,
                            diagnostic="Attempted to raise to fractional power " + exponent + ".")

    def test_radix_bounds_and_output(self):
        for text, output in (("ibase=2\n10\n", "2\n"), ("ibase=16\nF\n", "15\n"),
                             ("obase=2\n5\n", "101\n"), ("obase=16\n15\n", "F\n")):
            with self.subTest(text=text):
                self.run_bc(text, output)

    def test_positive_fractional_whole_value_conversion(self):
        for text, output in (("scale=2.9999999999999999999\nscale\n", "2\n"),
                             ("scale=.0000000000000000001\nscale\n", "0\n"),
                             ("ibase=2.9999999999999999999\n10\n", "2\n"),
                             ("obase=2.9999999999999999999\n5\n", "101\n"),
                             ("a[1.0000000001]=7\na[1]\n", "7\n"),
                             ("a[.0000000000000000001]=7\na[0]\n", "7\n"),
                             ("scale=2.0000000000000000000\nscale\n", "2\n")):
            with self.subTest(text=text):
                self.run_bc(text, output)

    def test_whole_value_overflow_does_not_wrap(self):
        for text in ("scale=1000000000\n", "a[1000000000]=7\n"):
            with self.subTest(text=text):
                self.run_bc(text, success=False)

    def test_function_argument_errors(self):
        for text in ("f()\n", "define f(a) {\nreturn (a)\n}\nf()\n",
                     "define f() {\nreturn (0)\n}\nf(1)\n",
                     "define f(a) {\nreturn (a)\n}\nf(b[])\n",
                     "define f(a[]) {\nreturn (a[0])\n}\nf(1)\n"):
            with self.subTest(text=text):
                self.run_bc(text, success=False)

    def test_recursive_calls_and_argument_order(self):
        self.run_bc("define f(n) {\nif(n<=1) return (1)\nreturn (n*f(n-1))\n}\nf(6)\n", "720\n")
        self.run_bc("define f(a,b) {\nreturn (a*10+b)\n}\n"
                    "define g(a) {\nreturn (f(a,3))\n}\ng(2)\n", "23\n")

    def test_locals_and_redefinition(self):
        self.run_bc("a=10\ndefine f(a) {\nauto b\nb=a+1\nreturn (b)\n}\n"
                    "f(2)\na\nb\n", "3\n10\n0\n")
        self.run_bc("define f() {\nreturn (1)\n}\nf()\n"
                    "define f() {\nreturn (2)\n}\nf()\n", "1\n2\n")

    def test_function_special_variable_modes(self):
        text = ("scale=2\ndefine f() {\nscale=3\nibase=16\nobase=2\nreturn (0)\n}\n"
                "f()\ns=scale\ni=ibase\no=obase\nibase=2\nibase=1010\nobase=10\ns\ni\no\n")
        self.run_bc(text, "0\n3\n16\n2\n")
        if EXTENSIONS:
            self.run_bc(text, "0\n2\n10\n10\n", options=("-g",))

    def test_runtime_ibase_for_reused_literal(self):
        self.run_bc("define f() {\nreturn (10)\n}\nf()\nibase=2\nf()\n", "10\n2\n")

    def test_repeated_function_literals_follow_each_runtime_base(self):
        # Re-enter the same executable function repeatedly under each base.
        text = "define f() {\nreturn (10)\n}\n"
        expected = ""
        for spelling, value in (("2", 2), ("8", 8), ("A", 10), ("16", 16), ("A", 10)):
            text += "ibase={}\nf()\nf()\n".format(spelling)
            expected += "{}\n{}\n".format(value, value)
        text += ("for(i=0;i<4;i++) {\nibase=A\nif(i%2==0) ibase=2\nf()\n}\n")
        self.run_bc(text, expected + "2\n10\n2\n10\n")

    def test_fractional_and_single_digit_literals_keep_their_rules(self):
        text = ("define f() {\nreturn (10.1)\n}\n"
                "define g() {\nreturn (F)\n}\n")
        expected = ""
        for spelling, value in (("2", "2.5"), ("8", "8.1"), ("A", "10.1"),
                                 ("16", "16.0"), ("A", "10.1")):
            text += "ibase={}\nf()\nf()\ng()\ng()\n".format(spelling)
            expected += "{0}\n{0}\n15\n15\n".format(value)
        self.run_bc(text, expected)

    def test_literal_results_are_independent_snapshots(self):
        value = "123456789012345678901234567890"
        text = ("define f() {\nreturn (" + value + ")\n}\n"
                "a=f()\na+=1\na\nf()\nf()\n"
                "define f() {\nreturn (10)\n}\nf()\nibase=2\nf()\nf()\n")
        self.run_bc(text, str(int(value) + 1) + "\n" + value + "\n" + value + "\n10\n2\n2\n")

    def test_literals_after_internal_base_change_and_special_restore(self):
        text = ("define f() {\nibase=2\nreturn (10)\n}\nf()\nf()\nibase\n")
        self.run_bc(text, "2\n2\n2\n")
        if EXTENSIONS:
            self.run_bc(text, "2\n2\n10\n", options=("-g",))

    def test_sparse_array_defaults(self):
        self.run_bc("a[1000000]=9\na[0]\na[1000000]\n", "0\n9\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("bc")
    parser.add_argument("--extensions", choices=("ON", "OFF"), required=True)
    args, remaining = parser.parse_known_args()
    BC, EXTENSIONS = args.bc, args.extensions == "ON"
    unittest.main(argv=[__file__, *remaining])
