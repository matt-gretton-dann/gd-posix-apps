#!/usr/bin/env python3
"""Assert VM behavior and runtime failures in bounded, isolated processes."""

import argparse
from decimal import Context, Decimal
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

    def test_bare_if_conditions(self):
        cases = [('n=0\n', 'n', ''), ('n = -2\n', 'n', '7\n'),
                 ('a[2]=0\n', 'a[2]', ''), ('a[2] = -3\n', 'a[2]', '7\n'),
                 ('scale=0\n', 'scale', ''), ('scale=2\n', 'scale', '7\n'),
                 ('', 'ibase', '7\n'), ('', 'obase', '7\n')]
        for setup, condition, expected in cases:
            with self.subTest(condition=condition, setup=setup):
                self.run_bc(setup + 'if(' + condition + ') 7\n', expected)

    def test_comparison_branches_preserve_operand_order(self):
        self.run_bc("""define f(a,b) {
auto r
r=0
if(a<b) r=r+1
if(a<=b) r=r+2
if(a==b) r=r+4
if(a!=b) r=r+8
if(a>b) r=r+16
if(a>=b) r=r+32
return(r)
}
f(-.1,0)
f(1.00,1)
f(2,1)
""", '11\n38\n56\n')

    def test_bare_loop_conditions_observe_each_iteration(self):
        for target in ('n', 'a[2]', 'scale'):
            for kind in ('while', 'for'):
                with self.subTest(target=target, kind=kind):
                    header = ('while(' + target + ')' if kind == 'while' else
                              'for(i=0;' + target + ';i=i+1)')
                    self.run_bc(target + '=3\ns=0\n' + header + ' {\n'
                                's=s+' + target + '\n' + target + '=' + target + '-1\n}\n'
                                's\n' + target + '\n', '6\n0\n')
        for target in ('ibase', 'obase'):
            with self.subTest(target=target):
                self.run_bc('while(' + target + ') {\n7\nbreak\n}\n', '7\n')
                self.run_bc('for(i=0;' + target + ';i=i+1) {\n7\nbreak\n}\n', '7\n')

    def test_scalar_block_preserves_arithmetic_operand_order(self):
        self.run_bc('{a=20;b=3;c=(a-b)*2;d=a/(b+1);if(c>d) c-=d;c;d;}\n'
                    'a\nb\n', '29\n5\n20\n3\n')

    def test_scalar_loop_state_and_exact_fallbacks(self):
        cases = [
            ('for(i=0;i<100000;i++) s+=i\ns\ni\n', '4999950000\n100000\n'),
            ('for(i=0;i<0;i++) s+=i\ns\ni\n', '0\n0\n'),
            ('for(i=0;i<2;i++) s+=18446744073709551615\ns\ni\n',
             '36893488147419103230\n2\n'),
            ('s=7\nfor(i=0;i<4;i++) s+=i\ns\ni\n', '13\n4\n'),
            ('s=0.0\nfor(i=0;i<4;i++) s+=i\ns\nscale(s)\n', '6.0\n1\n'),
            ('ibase=2\nfor(i=0;i<101;i++) s+=i\ns\ni\n', '10\n5\n'),
            ('for(i=0;i<4;i++) s+=.2\nif(s==.8) 1\nscale(s)\ni\n', '1\n1\n4\n'),
            ('for(i=0;i<4;i++) { s+=i; i; }\ns\n', '0\n1\n2\n3\n6\n'),
        ]
        for program, expected in cases:
            with self.subTest(program=program):
                self.run_bc(program, expected)

    def test_condition_side_effects_run_once(self):
        cases = [('n=0\nif(n=2) 7\nn\n', '7\n2\n'),
                 ('n=0\nif(n++) 7\nn\n', '1\n'),
                 ('n=3\ns=0\nwhile(n--) s=s+n\ns\nn\n', '3\n-1\n'),
                 ('s=0\nfor(n=3;n--;n=n) s=s+n\ns\nn\n', '3\n-1\n')]
        for program, expected in cases:
            with self.subTest(program=program):
                self.run_bc(program, expected)

    def test_bare_parameter_and_local_conditions(self):
        self.run_bc('define f(n) {\nif(n) return(f(n-1)+n)\nreturn(0)\n}\n'
                    'n=7\nf(3)\nn\n', '6\n7\n')
        self.run_bc('define f(x) {\nauto n,s\ns=0\n'
                    'for(n=x;n;n=n-1) s=s+n\nreturn(s)\n}\nf(3)\n', '6\n')

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

    def test_long_repeated_coefficient_products(self):
        lhs = '19' + '7' * 3204
        rhs = '543' + '9' * 2700
        interrupted = lhs[:600] + '8' + lhs[601:]
        for left, right, expression in ((lhs, rhs, 'a*b'),
                                        (rhs, lhs, 'a*b'),
                                        (lhs, lhs, 'a*a'),
                                        (interrupted, rhs, 'a*b'),
                                        ('-' + lhs, rhs, 'a*b')):
            with self.subTest(expression=expression, left_prefix=left[:3], right_prefix=right[:3]):
                expected = Context(prec=len(left) + len(right)).multiply(Decimal(left), Decimal(right))
                self.run_bc(f'a = {left}\nb = {right}\nif({expression} == {expected}) 1\n', '1\n')

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

    def test_scalar_call_with_nested_and_redefined_callees(self):
        large = "1234567890" * 12
        self.run_bc("define f(a) {\nreturn (a+1)\n}\n"
                    "define g(a,b) {\nreturn (a-b)\n}\n"
                    "define h(a) {\nreturn (g(a,f(a)))\n}\n"
                    f"h({large})\n"
                    "define f(a) {\nreturn (a+2)\n}\n"
                    f"h({large})\n", "-1\n-2\n")

    def test_function_results_survive_block_changes_and_early_return(self):
        large = 10 ** 39 + 7
        program = """define f(a[],n) {
auto i,j,r,t
if(n<0) return(a[0])
r=a[0]
for(i=0;i<n;i++) {
if(i%2==0) r=r+a[1]
if(i%2!=0) r=r-a[2]
if(i==3) {
t=0
for(j=0;j<5;j++) t=t+j*j
r=r+t-t
}
}
return(r)
}
"""
        program += f"a[0]={large}\na[1]=1\na[2]=2\n"
        for count in (-1, 0, 1, 8, 9):
            program += f"f(a[],{count})\n"
        program += "a[0]\n"
        expected = [large, large, large + 1, large - 4, large - 3, large]
        self.run_bc(program, "".join(str(value) + "\n" for value in expected))

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

    def test_large_sparse_products_and_dense_fallback(self):
        radix = 10 ** 9
        cases = (
            ((0, 2, 198, 300), (0, 2, 198, 300)),
            ((0, 2, 198, 300), (3, 101, 256, 320)),
            ((0, 2, 198, 299, 300), (0, 2, 198, 300)),
            ((0, 255), (0, 256)),
        )
        for left_indices, right_indices in cases:
            with self.subTest(left=left_indices, right=right_indices):
                # Full limbs force carries through overlapping diagonals.
                left = sum((radix - 1) * radix ** i for i in left_indices)
                right = sum((radix - 1) * radix ** i for i in right_indices)
                operands = [format(Decimal(n), 'f') for n in (left, right, left * right)]
                a, b, product = operands
                self.run_bc(f'a={a}\nb={b}\nc={product}\nif(a*b==c) 1\nif((-a)*b== -c) 1\n',
                            '1\n1\n')

    def test_sparse_array_defaults(self):
        self.run_bc("a[1000000]=9\na[0]\na[1000000]\n", "0\n9\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("bc")
    parser.add_argument("--extensions", choices=("ON", "OFF"), required=True)
    args, remaining = parser.parse_known_args()
    BC, EXTENSIONS = args.bc, args.extensions == "ON"
    unittest.main(argv=[__file__, *remaining])
