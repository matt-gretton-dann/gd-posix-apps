#!/usr/bin/env python3
"""Independent rational guards for interpreter divisor reuse and invalidation."""
import argparse
from fractions import Fraction
import os
import subprocess
import unittest

A = 10 ** 99 + 314159265358979323846264338327950288419716939937510
B = 10 ** 50 + 271828182845904523536028747135266249775724709369995


def pair(a, b, target, sa=0, sb=0):
    scaled = a / b * 10 ** target
    coefficient = abs(scaled.numerator) // scaled.denominator
    if scaled < 0:
        coefficient = -coefficient
    quotient = Fraction(coefficient, 10 ** target)
    return [quotient, target, a - quotient * b, max(sa, target + sb)]


def decimal(coefficient, scale):
    text = str(coefficient).zfill(scale + 1)
    return text[:-scale] + '.' + text[-scale:] if scale else text


EMIT = 'q=a/b;q;scale(q);r=a%b;r;scale(r)\n'


class Reuse(unittest.TestCase):
    def invoke(self, program):
        try:
            return subprocess.run([BC], input=program, universal_newlines=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                  timeout=2, env=dict(os.environ, LC_ALL='C'))
        except subprocess.TimeoutExpired:
            self.fail('divisor reuse program exceeded two seconds')

    def check(self, program, expected):
        result = self.invoke(program)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stderr, '')
        tokens = result.stdout.replace('\\\n', '').splitlines()
        self.assertEqual(len(tokens), len(expected), result.stdout)
        self.assertEqual([Fraction(token) for token in tokens], expected)

    def test_stable_divisor_and_original_value(self):
        program = f'scale=7\na={A}\nb={B}\nfor(i=0;i<24;i++) {{\n{EMIT}}}\nb\n'
        self.check(program, pair(Fraction(A), Fraction(B), 7) * 24 + [B])

    def test_changing_divisor_each_iteration(self):
        program = (f'scale=7\na={A}\nb={B}\nfor(i=0;i<24;i++) {{\n'
                   + EMIT + 'b=b+1\n}\nb\n')
        expected = []
        for i in range(24):
            expected += pair(Fraction(A), Fraction(B + i), 7)
        self.check(program, expected + [B + 24])

    def test_unique_temporary_divisors(self):
        program = (f'scale=7\na={A}\nb={B}\nfor(i=0;i<20;i++) {{\n'
                   'q=a/(b+i);q;scale(q);r=a%(b+i);r;scale(r)\n}\n')
        expected = []
        for i in range(20):
            expected += pair(Fraction(A), Fraction(B + i), 7)
        self.check(program, expected)

    def test_scale_and_sign_changes(self):
        program = f'a={decimal(A, 17)}\nb={decimal(B, 3)}\n'
        expected = []
        sign = 1
        for target in (0, 2, 13, 1, 7, 0, 13):
            program += f'scale={target}\nfor(j=0;j<5;j++) {{\n{EMIT}}}\nb= -b\n'
            expected += pair(Fraction(A, 10 ** 17), sign * Fraction(B, 10 ** 3),
                             target, 17, 3) * 5
            sign = -sign
        self.check(program, expected)

    def test_array_element_mutation_after_admission(self):
        program = (f'scale=7\na={A}\nb[0]={B}\nfor(i=0;i<12;i++) {{\n'
                   'if(i%3==0) b[0]=b[0]+1\n'
                   'q=a/b[0];q;scale(q);r=a%b[0];r;scale(r)\n}\nb[0]\n')
        expected = []
        for i in range(12):
            expected += pair(Fraction(A), Fraction(B + 1 + i // 3), 7)
        self.check(program, expected + [B + 4])

    def test_input_base_changes(self):
        literal = '123456789012345678901234567890123456789012345678901'
        program = (f'scale=3\na={A}\nfor(i=0;i<8;i++) {{\n'
                   f'scale=0\nibase=A\nif(i%2==0) ibase=16\nscale=3\nb={literal}\n'
                   f'for(j=0;j<4;j++) {{\n{EMIT}}}\n}}\nibase\n')
        expected = []
        for i in range(8):
            expected += pair(Fraction(A), Fraction(int(literal, 16 if i % 2 == 0 else 10)), 3) * 4
        self.check(program, expected + [10])

    def test_recursive_local_divisor_lifetimes(self):
        program = (f'scale=7\na={A}\nb=11\n'
                   'define f(n) {\nauto b,q,s,i\n'
                   f'b={B}+n\nfor(i=0;i<4;i++) q=a/b\ns=0\n'
                   'if(n>0) s=f(n-1)\nq=a/b\nreturn(q+s)\n}\n'
                   'f(4)\nscale(f(4))\nb\n')
        total = sum(pair(Fraction(A), Fraction(B + i), 7)[0] for i in range(5))
        self.check(program, [total, 7, 11])

    def test_function_redefinition(self):
        program = (f'scale=7\na={A}\nb={B}\n'
                   'define f() {\nauto q,i\nfor(i=0;i<5;i++) q=a/b\nreturn(q)\n}\nf()\n'
                   'define f() {\nauto q,i\nfor(i=0;i<5;i++) q=a/(b+1)\nreturn(q)\n}\nf()\nb\n')
        self.check(program, [pair(Fraction(A), Fraction(B), 7)[0],
                             pair(Fraction(A), Fraction(B + 1), 7)[0], B])

    def test_alias_reload(self):
        program = (f'scale=5\nx={A}\nfor(i=0;i<12;i++) {{\n'
                   'a=x;a=a/a;a;scale(a);a=x;a=a%a;a;scale(a)\n}\nx\n')
        self.check(program, [Fraction(1), 5, Fraction(0), 5] * 12 + [A])

    def test_zero_diagnostic_after_warmup(self):
        for operation in ('/', '%'):
            with self.subTest(operation=operation):
                result = self.invoke(f'a={A}\nb={B}\nfor(i=0;i<5;i++) q=a{operation}b\n'
                                     f'b=0\nq=a{operation}b\n')
                self.assertEqual(result.returncode, 1)
                self.assertEqual(result.stdout, '')
                self.assertIn('divide by zero', result.stderr)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('bc')
    args, remainder = parser.parse_known_args()
    BC = args.bc
    unittest.main(argv=[__file__, *remainder])
