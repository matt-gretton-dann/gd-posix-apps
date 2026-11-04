#!/usr/bin/env python3
"""Certify ordinary -l functions and their observable language behavior."""
import argparse
from decimal import Decimal, localcontext
from fractions import Fraction
import importlib.util
import json
import os
import re
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location('math_oracle', ROOT / 'scripts/math_oracle.py')
ORACLE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(ORACLE)
if hasattr(sys, 'set_int_max_str_digits'):
    sys.set_int_max_str_digits(1000000)


def environment():
    result = {key: value for key, value in os.environ.items()
              if not key.startswith(('BC_', 'DC_', 'GD_BC_PROFILE_'))}
    result.update(LC_ALL='C', BC_ENV_ARGS='', BC_LINE_LENGTH='70')
    return result


def unit_atan_enclosure():
    """Bound pi/4 by Machin's identity and exact alternating remainders."""
    def inverse_atan_bounds(denominator, terms):
        total = sum((Fraction((-1) ** k, (2 * k + 1) * denominator ** (2 * k + 1))
                     for k in range(terms)), Fraction())
        next_sum = total + Fraction((-1) ** terms,
                                    (2 * terms + 1) * denominator ** (2 * terms + 1))
        return min(total, next_sum), max(total, next_sum)

    lo5, hi5 = inverse_atan_bounds(5, 130)
    lo239, hi239 = inverse_atan_bounds(239, 40)
    return 4 * lo5 - hi239, 4 * hi5 - lo239


class LibraryCli(unittest.TestCase):
    def invoke(self, program, options=('-l',), files=(), binary=None, timeout=30):
        try:
            return subprocess.run([binary or BC, *options, *map(str, files)], input=program,
                                  universal_newlines=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                  env=environment(), timeout=timeout)
        except subprocess.TimeoutExpired:
            self.fail('math-library invocation exceeded its correctness deadline')

    def checked_lines(self, program, **kwargs):
        result = self.invoke(program, **kwargs)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stderr, '')
        return result.stdout.replace('\\\r\n', '').replace('\\\n', '').splitlines()

    def certified(self, options=('-l',), files=(), binary=None):
        cases = []
        for filename in ('math-oracles.json', 'math-crossover-oracles.json', 'math-trig-oracles.json',
                         'math-bessel-zero-oracles.json'):
            cases.extend(json.loads((ROOT / 'fixtures' / filename).read_text())['cases'])
        program = []
        for case in cases:
            program += ['scale=' + str(case['scale']),
                        'r=' + case['function'] + '(' + ','.join(case['arguments']) + ')',
                        'r', 'scale', 'ibase', 'scale(r)']
        lines = self.checked_lines('\n'.join(program) + '\n', options=options, files=files,
                                   binary=binary, timeout=300)
        self.assertEqual(len(lines), 4 * len(cases))
        for index, case in enumerate(cases):
            with self.subTest(case=case['id'], scale=case['scale']):
                candidate = ORACLE.decimal_fraction(lines[4 * index])
                status = ORACLE.classify(candidate, Fraction(case['lower']),
                                         Fraction(case['upper']), case['scale'])
                diagnostic = ('certified reference needs refinement' if status == 'refine'
                              else 'result exceeds the one-decimal-ULP contract')
                self.assertEqual(status, 'pass', diagnostic)
                self.assertEqual(lines[4 * index + 1:4 * index + 4],
                                 [str(case['scale']), '10', str(case['scale'])])

    def test_exponential_reconstruction(self):
        cases = [(2, '-.125'), (20, '-.125000000001'), (100, '-200'),
                 (300, '-125.00000001'), (1000, '-1000'), (1000, '-2000'),
                 (1000, '-2300'), (1000, '-3002.999'), (1000, '-3003'),
                 (20, '1.999999999'), (20, '2'), (20, '2.000000001'),
                 (100, '200'), (300, '500.125'), (1000, '1000'), (1000, '2300')]
        program = ''.join(f'scale={scale}\nr=e({argument})\nr\nscale(r)\n'
                          for scale, argument in cases)
        binaries = [BC] if OTHER_BC is None else [BC, OTHER_BC]
        for binary in binaries:
            lines = self.checked_lines(program, options=(), files=(SOURCE,), binary=binary)
            self.assertEqual(len(lines), 2 * len(cases))
            for index, (scale, argument) in enumerate(cases):
                with self.subTest(binary=binary, scale=scale, argument=argument):
                    # Decimal.exp is correctly rounded. Its adjacent decimal
                    # values enclose the result for the exact input.
                    with localcontext() as context:
                        exact_argument = Decimal(argument)
                        # Allow integer digits as well as the requested scale.
                        context.prec = scale + max(0, int(exact_argument)) + 32
                        reference = exact_argument.exp()
                        lower = Fraction(reference.next_minus())
                        upper = Fraction(reference.next_plus())
                    value = ORACLE.decimal_fraction(lines[2 * index])
                    self.assertEqual(ORACLE.classify(value, lower, upper, scale), 'pass')
                    self.assertEqual(lines[2 * index + 1], str(scale))

    def test_atan_unit_literal_matches_the_certified_enclosure(self):
        source = Path(SOURCE).read_text(encoding='utf-8')
        literals = re.findall(r'(?m)^\s*r=(\.\d{160})$', source)
        self.assertEqual(len(literals), 1)
        lower, upper = unit_atan_enclosure()
        places = 10 ** 160
        lo = lower.numerator * places // lower.denominator
        hi = upper.numerator * places // upper.denominator
        self.assertEqual(lo, hi, 'reference must certify every stored decimal place')
        self.assertEqual(Fraction(literals[0]), Fraction(lo, places))
        for scale in range(145):
            factor = 10 ** scale
            self.assertEqual(lower.numerator * factor // lower.denominator,
                             upper.numerator * factor // upper.denominator)
            def rounded(value):
                return (2 * value.numerator * factor + value.denominator) // (2 * value.denominator)
            self.assertEqual(rounded(lower), rounded(upper))
            self.assertEqual(rounded(Fraction(literals[0])), rounded(lower))

    def test_atan_unit_argument_precision_boundary(self):
        # Negating an exact enclosure reverses its endpoints.
        lower, upper = unit_atan_enclosure()
        tests = [(scale, sign, trailing, base)
                 for scale in (0, 1, 2, 20, 63, 64, 65, 143, 144, 145)
                 for sign in (1, -1) for trailing in ('', '.000000000000')
                 for base in (10, 2, 16)]
        program = ['ibase=A', 'obase=A']
        for scale, sign, trailing, base in tests:
            argument = ('-' if sign < 0 else '') + '1' + trailing
            program += ['ibase=A', 'scale=' + str(scale), 'ibase=' + str(base),
                        'r=a(' + argument + ')', 'r', 'scale', 'ibase', 'scale(r)']
        modes = [(BC, ('-l',), ()), (BC, (), (SOURCE,))]
        if OTHER_BC:
            modes.append((OTHER_BC, (), (SOURCE,)))
        for binary, options, files in modes:
            lines = self.checked_lines('\n'.join(program) + '\n', binary=binary,
                                       options=options, files=files)
            self.assertEqual(len(lines), 4 * len(tests))
            for index, (scale, sign, trailing, base) in enumerate(tests):
                with self.subTest(binary=binary, scale=scale, sign=sign,
                                  trailing=trailing, base=base, options=options):
                    lo, hi = (lower, upper) if sign > 0 else (-upper, -lower)
                    candidate = ORACLE.decimal_fraction(lines[4 * index])
                    self.assertEqual(ORACLE.classify(candidate, lo, hi, scale), 'pass')
                    factor = 10 ** scale
                    rounded_lower = (2 * lower.numerator * factor + lower.denominator) // (2 * lower.denominator)
                    rounded_upper = (2 * upper.numerator * factor + upper.denominator) // (2 * upper.denominator)
                    self.assertEqual(rounded_lower, rounded_upper)
                    self.assertEqual(candidate, sign * Fraction(rounded_lower, factor))
                    self.assertEqual(lines[4 * index + 1:4 * index + 4],
                                     [str(scale), str(base), str(scale)])

    def test_atan_near_one_reduction_boundaries(self):
        lo, hi = unit_atan_enclosure()
        cases = [(scale, sign, text) for scale in (0, 20, 64, 100, 144, 145)
                 for sign in ('', '-') for text in
                 ('.799', '.8', '.801', '.999', '1.001', '1.125', '1.249', '1.25', '1.251')]
        program = ''.join(f'scale={scale}\na({sign}{text})\n'
                          for scale, sign, text in cases)
        lines = self.checked_lines(program)
        self.assertEqual(len(lines), len(cases))
        for (scale, sign, text), output in zip(cases, lines):
            with self.subTest(scale=scale, argument=sign + text):
                x = Fraction(text)
                reduced = (x - 1) / (x + 1)
                term, total = reduced, Fraction()
                for k in range(90):
                    total += term / (2 * k + 1)
                    term *= -reduced * reduced
                other = total + term / 181
                lower, upper = lo + min(total, other), hi + max(total, other)
                if sign:
                    lower, upper = -upper, -lower
                self.assertEqual(ORACLE.classify(Fraction(output), lower, upper, scale), 'pass')

    def test_trig_argument_representation_preserves_certified_values(self):
        # Adding fractional zeroes changes stored precision, never the exact
        # input. Reuse independently enclosed +/-1 values for both shapes.
        references = {(case['function'], case['scale']): case for case in
                      json.loads((ROOT / 'fixtures/math-oracles.json').read_text())['cases']
                      if case['function'] in ('s', 'c') and case['arguments'] == ['1']}
        tests = [(function, scale, sign, base)
                 for function in ('s', 'c') for scale in (0, 20, 100, 1000)
                 for sign in (1, -1) for base in (10, 2, 16)]
        program = ['ibase=A', 'obase=A']
        for function, scale, sign, base in tests:
            argument = ('-' if sign < 0 else '') + '1.' + '0' * 512
            program += ['ibase=A', 'scale=' + str(scale), 'ibase=' + str(base),
                        'r=' + function + '(' + argument + ')',
                        'r', 'scale', 'ibase', 'scale(r)']
        modes = [(BC, ('-l',), ()), (BC, (), (SOURCE,))]
        if OTHER_BC:
            modes.append((OTHER_BC, (), (SOURCE,)))
        for binary, options, files in modes:
            lines = self.checked_lines('\n'.join(program) + '\n', binary=binary,
                                       options=options, files=files, timeout=300)
            self.assertEqual(len(lines), 4 * len(tests))
            for index, (function, scale, sign, base) in enumerate(tests):
                with self.subTest(binary=binary, function=function, scale=scale,
                                  sign=sign, base=base, options=options):
                    reference = references[(function, scale)]
                    lower, upper = Fraction(reference['lower']), Fraction(reference['upper'])
                    if function == 's' and sign < 0:
                        lower, upper = -upper, -lower
                    value = ORACLE.decimal_fraction(lines[4 * index])
                    self.assertEqual(ORACLE.classify(value, lower, upper, scale), 'pass')
                    self.assertEqual(lines[4 * index + 1:4 * index + 4],
                                     [str(scale), str(base), str(scale)])

    def test_basic_exact_values_and_result_scale(self):
        lines = self.checked_lines('scale=3\ne(0)\nl(1)\na(0)\ns(0)\nc(0)\n'
                                   'j(0,0)\nj(1,0)\nscale(e(0))\nscale(s(0))\nscale\n')
        self.assertEqual([Fraction(x) for x in lines], [1, 0, 0, 0, 1, 1, 0, 3, 3, 3])

    def test_all_certified_function_regimes(self):
        self.certified()

    def test_bessel_ascending_rounding_midpoint(self):
        arguments = ('1.52114405766876514815130187', '1.52114405766876514815130188')
        for argument, expected in zip(arguments, (1, 0)):
            x = Fraction(argument)
            term = partial = Fraction(1)
            # The terms alternate and decrease for these arguments below two.
            for k in range(1, 40):
                term *= -x * x / (4 * k * k)
                partial += term
            upper = partial - term * x * x / (4 * 40 * 40)
            self.assertEqual(ORACLE.classify(Fraction(expected), partial, upper, 0), 'pass')
        program = 'scale=0\n' + ''.join(f'j(0,{x})\n' for x in arguments)
        self.assertEqual(self.checked_lines(program), ['1', '0'])

    def test_bessel_ascending_with_large_cancellation(self):
        for order, argument, scale in ((0, 100, 150), (300, 300, 100)):
            with self.subTest(order=order, argument=argument, scale=scale):
                term = Fraction(1)
                for j in range(1, order + 1):
                    term *= Fraction(argument, 2 * j)
                partial = term
                for k in range(1, 2000):
                    ratio = Fraction(argument**2, 4 * k * (order + k))
                    term *= -ratio
                    if ratio < 1 and abs(term) < Fraction(1, 10**(scale + 2)):
                        break
                    partial += term
                else:
                    self.fail('Bessel rational enclosure did not converge')
                # After the ratio falls below one, all subsequent ratios
                # decrease. The first omitted term bounds the alternating tail.
                lower, upper = sorted((partial, partial + term))
                result, = self.checked_lines(f'scale={scale}\nj({order},{argument})\n')
                self.assertEqual(ORACLE.classify(Fraction(result), lower, upper, scale), 'pass')

    def test_identical_source_loads_as_ordinary_input(self):
        self.certified(options=(), files=(SOURCE,))

    def test_no_library_means_no_math_definitions(self):
        result = self.invoke('e(0)\n', options=())
        self.assertNotEqual(result.returncode, 0)
        self.assertNotEqual(result.stderr, '')

    def test_ordinary_source_initializes_decimal_scale_under_any_base(self):
        binaries = [BC] + ([OTHER_BC] if OTHER_BC else [])
        for binary in binaries:
            for base in (2, 8, 16):
                with self.subTest(binary=binary, base=base):
                    with tempfile.TemporaryDirectory() as directory:
                        first = Path(directory) / 'caller base.bc'
                        value = 'A+6' if base == 16 else str(base)
                        first.write_text('ibase={}\nobase={}\n'.format(value, value))
                        lines = self.checked_lines(
                            'i=ibase\no=obase\nibase=A\nobase=A\nscale\ni\no\n',
                            options=(), files=(first, SOURCE), binary=binary)
                        self.assertEqual(lines, ['20', str(base), str(base)])

    def test_user_definition_replaces_library_function_normally(self):
        lines = self.checked_lines('define e(x) {\nreturn(7)\n}\ne(0)\nscale(e(0))\nscale\n')
        # The replacement is an ordinary user function, including its scale.
        self.assertEqual(lines, ['7', '0', '20'])

    def test_user_file_redefinition_precedes_stdin(self):
        with tempfile.TemporaryDirectory() as directory:
            user = Path(directory) / 'user math.bc'
            user.write_text('define e(x) {\nreturn(9)\n}\ne(0)\n')
            self.assertEqual(self.checked_lines('e(0)\n', files=(user,)), ['9', '9'])

    def test_locals_and_parameters_preserve_all_user_scalar_bindings(self):
        letters = 'abcdefghijklmnopqrstuvwxyz'
        initialization = ''.join('{}={}\n'.format(name, 100 + index)
                                 for index, name in enumerate(letters))
        queries = '\n'.join(letters) + '\n'
        for call, expected in [('e(0)', 1), ('l(1)', 0), ('a(0)', 0),
                               ('s(0)', 0), ('c(0)', 1), ('j(0,0)', 1)]:
            with self.subTest(call=call):
                lines = self.checked_lines(initialization + call + '\n' + queries)
                self.assertEqual([Fraction(x) for x in lines], [expected, *range(100, 126)])

    def test_nondecimal_bases_and_scale_are_restored(self):
        for ibase in (2, 8, 16):
            for obase in (2, 8, 16):
                for call, expected in [('e(0)', 1), ('l(1)', 0), ('a(0)', 0),
                                       ('s(0)', 0), ('c(0)', 1), ('j(0,0)', 1)]:
                    with self.subTest(ibase=ibase, obase=obase, call=call):
                        # A+6 uses single-digit, base-independent constants.
                        input_base = 'A+6' if ibase == 16 else str(ibase)
                        output_base = 'A+6' if obase == 16 else str(obase)
                        program = ('ibase={}\nobase={}\ni=ibase\no=obase\nv={}\nh=0\n'
                                   'if(ibase!=i) h+=1\nif(obase!=o) h+=1\n'
                                   'obase=A\nibase=A\nh\nv\nscale(v)\nscale\ni\no\n').format(
                                       input_base, output_base, call)
                        lines = self.checked_lines(program)
                        self.assertEqual([Fraction(x) for x in lines],
                                         [0, expected, 20, 20, ibase, obase])

    def test_nested_calls_and_integer_order_normalization(self):
        lines = self.checked_lines('scale=20\ne(l(1))\nl(e(0))\nj(0.00,0)\n'
                                   'j(-1.00,0)\nscale\nibase\n')
        self.assertEqual([Fraction(x) for x in lines], [1, 0, 1, 0, 20, 10])

    def test_argument_values_survive_function_parameter_mutation(self):
        for fraction in ('', '.' + '1234567890' * 10):
            places = 0 if not fraction else len(fraction) - 1
            order = '-3' + ('.' + '0' * places if places else '')
            for call, sign in [('e(x)', '-'), ('a(x)', '-'), ('s(x)', '-'),
                               ('c(x)', '-'), ('l(x)', ''), ('j(n,x)', '-')]:
                x = sign + '2' + fraction
                with self.subTest(call=call, places=places):
                    # The long fractions force heap-backed COW coefficients;
                    # checking only small inline arguments misses that contract.
                    program = ('x= {}\nn= {}\nv={}\nx\nn\nscale(x)\nscale(n)\n'
                               'scale(v)\n').format(x, order, call)
                    lines = self.checked_lines(program)
                    self.assertEqual([Fraction(value) for value in lines],
                                     [Fraction(x), -3, places, places, 20])

    def test_silent_host_scale_clamping_cannot_return_uncertified_values(self):
        # Portable test-side fault injection models a host that ignores the
        # working-scale request; production has no fault-injection hooks.
        source = Path(SOURCE).read_text(encoding='utf-8')
        changed = re.sub(r'(?m)^( *)scale=w$', r'\1scale=0', source)
        self.assertNotEqual(changed, source)
        with tempfile.TemporaryDirectory() as directory:
            program = Path(directory) / 'clamped library.bc'
            program.write_text(changed, encoding='utf-8')
            for call in ('e(0)', 'l(1)', 'a(0)', 's(0)', 'c(0)', 'j(0,0)'):
                with self.subTest(call=call):
                    result = self.invoke(call + '\n', options=(), files=(program,))
                    self.assertNotEqual(result.returncode, 0)
                    self.assertNotEqual(result.stderr, '')
                    self.assertEqual(result.stdout, '')

    def test_unrepresentable_working_precision_reports_failure(self):
        # These valid-domain values require unrepresentable working scales,
        # so assignment must fail before an enormous coefficient is allocated.
        for call in ('e(10^20)', 'j(10^20,10^20)'):
            with self.subTest(call=call):
                result = self.invoke(call + '\n')
                self.assertNotEqual(result.returncode, 0)
                self.assertNotEqual(result.stderr, '')
                self.assertEqual(result.stdout, '')

    def test_optional_external_source_portability(self):
        if OTHER_BC is None:
            self.skipTest('external conforming bc not supplied')
        # Load the exact source as data, without the host's competing -l library.
        self.certified(options=(), files=(SOURCE,), binary=OTHER_BC)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('bc')
    parser.add_argument('--source', default=str(ROOT / 'math-library.bc'))
    parser.add_argument('--other-bc')
    args, remaining = parser.parse_known_args()
    BC, SOURCE, OTHER_BC = args.bc, str(Path(args.source).resolve()), args.other_bc
    unittest.main(argv=[__file__, *remaining])
