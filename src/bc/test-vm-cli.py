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

    def test_sparse_array_defaults(self):
        self.run_bc("a[1000000]=9\na[0]\na[1000000]\n", "0\n9\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("bc")
    parser.add_argument("--extensions", choices=("ON", "OFF"), required=True)
    args, remaining = parser.parse_known_args()
    BC, EXTENSIONS = args.bc, args.extensions == "ON"
    unittest.main(argv=[__file__, *remaining])
