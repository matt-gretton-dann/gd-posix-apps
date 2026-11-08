#!/usr/bin/env python3
"""Certify ordinary -l functions and their observable language behavior."""
import argparse
from decimal import Decimal, localcontext
from fractions import Fraction
import importlib.util
import json
import math
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
    def test_bessel_four_term_identity_and_local_error_bounds(self):
        # Compare the common-denominator identity with four independently
        # advanced exact terms. This includes fractional arguments, unlike
        # the earlier integer-only grouping experiment.
        def truncate(value, places):
            return Fraction(int(value * 10 ** places), 10 ** places)

        for order in (0, 1, 7, 23, 403):
            for index in (1, 2, 7, 8, 31, 234):
                denominators = [(index + offset) * (order + index + offset)
                                for offset in range(4)]
                d1, d2, d3, d4 = denominators
                denominator = d1 * d2 * d3 * d4
                self.assertGreaterEqual(denominator, 576)
                for q in map(Fraction, ('-16', '-15.999999', '-4', '-.25', '-.000001', '0')):
                    polynomial = d2*d3*d4 + q*(d3*d4 + q*(d4+q))
                    term = Fraction(7, 3)
                    direct = Fraction()
                    for divisor in denominators:
                        term = term*q/divisor
                        direct += term
                    self.assertEqual(direct, Fraction(7, 3)*q*polynomial/denominator)
                    self.assertEqual(term, Fraction(7, 3)*q**4/denominator)
                    self.assertLess(abs(polynomial)/denominator, 20)
                    for places in (6, 12):
                        unit = Fraction(1, 10**places)
                        stored_q = truncate(q, places)
                        squared = truncate(stored_q*stored_q, places)
                        fourth = truncate(squared*squared, places)
                        self.assertLessEqual(abs(fourth-stored_q**4), 513*unit)
                        stored_p = truncate(truncate((stored_q+d4)*stored_q, places)
                                            + d3*d4, places)
                        stored_p = truncate(stored_p*stored_q, places) + d2*d3*d4
                        exact_p = d2*d3*d4 + stored_q*(d3*d4 + stored_q*(d4+stored_q))
                        self.assertLessEqual(abs(stored_p-exact_p), 17*unit)
                        for value in (Fraction(0), Fraction(1), Fraction(2999),
                                      Fraction(2999999999, 1000000)):
                            advanced = truncate(truncate(value*fourth, places)/denominator, places)
                            self.assertLess(abs(advanced-value*stored_q**4/denominator), 2675*unit)
                            block = truncate(truncate(truncate(value*stored_q, places)
                                                      * stored_p, places)/denominator, places)
                            self.assertLess(abs(block-value*stored_q*exact_p/denominator), 1441*unit)

    def test_bessel_four_term_bounded_error_budget(self):
        # A first pass through |x|<=8, S<=100 uses W=S+12. Reuse
        # exp(8)<3000 for propagation, but account for every extra product
        # truncation introduced by the four-term recurrence.
        prefix = sum((Fraction(8)**k / math.factorial(k)
                      for k in range(65)), Fraction())
        tail = Fraction(8)**65 / math.factorial(65)
        self.assertLess(prefix + tail/(1-Fraction(8, 66)), 3000)
        self.assertLess(2675*3000 + 1441, 8030000)
        for scale in range(101):
            working = scale + 12
            coefficient_steps = max(24, 4*(scale+1))-1
            term_steps = 2*working+16
            blocks = (term_steps+3)//4
            self.assertLess(Fraction(3000, 4**(2*working+8)), Fraction(1, 10**working))
            error_units = 1 + 2*3000*(coefficient_steps+1) + 8030000*blocks
            self.assertLess(Fraction(error_units, 10**12), Fraction(1, 1000))

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
                         'math-bessel-zero-oracles.json', 'math-bessel-bounded-oracles.json'):
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

    def test_logarithm_argument_reduction(self):
        cases = []
        for scale in (20, 100, 1000):
            cutoff = 64 + scale // 2
            for exponent in (cutoff - 1, cutoff, cutoff + 33):
                cases.append((scale, '9' + '0' * exponent))
                cases.append((scale, '.' + '0' * (exponent - 1) + '9'))
        for scale in (999, 1000):
            for argument in ('.02', '.949999', '.95', '1.05', '1.050001', '2'):
                cases.append((scale, argument))
        # Check both ends of the root-free band and the W=64/65 switch.
        for argument in ('1.599999', '1.6', '1.600001', '2.399999', '2.4', '2.400001'):
            cases.append((20, argument))
        cases.extend((scale, '2') for scale in (35, 36))
        # Exercise multiple blocks and the short tail close to one, on both
        # sides of zero in the logarithm's result.
        for argument in ('.96', '2.25', '.' + '9' * 100, '1.' + '0' * 99 + '1'):
            cases.append((2400, argument))
        program = ''.join(f'scale={scale}\nr=l({argument})\nr\nscale(r)\nibase\n'
                          for scale, argument in cases)
        binaries = [BC] if OTHER_BC is None else [BC, OTHER_BC]
        for binary in binaries:
            lines = self.checked_lines(program, options=(), files=(SOURCE,), binary=binary)
            self.assertEqual(len(lines), 3 * len(cases))
            for index, (scale, argument) in enumerate(cases):
                with self.subTest(binary=binary, scale=scale, argument=argument):
                    # The correctly rounded Decimal logarithm is enclosed
                    # by its adjacent representable values.
                    with localcontext() as context:
                        context.prec = scale + 64
                        reference = Decimal(argument).ln()
                        lower = Fraction(reference.next_minus())
                        upper = Fraction(reference.next_plus())
                    value = ORACLE.decimal_fraction(lines[3 * index])
                    self.assertEqual(ORACLE.classify(value, lower, upper, scale), 'pass')
                    self.assertEqual(lines[3 * index + 1:3 * index + 3], [str(scale), '10'])

    def test_logarithm_binary_reduction_literal(self):
        source = Path(SOURCE).read_text(encoding='utf-8')
        literals = re.findall(r'(?m)^\s*h\+=(0\.\d{80})/1$', source)
        self.assertEqual(len(literals), 1)
        # ln(2) = 2*atanh(1/3). The positive tail is bounded by the
        # first omitted term times the geometric sum with ratio 1/9.
        count = 100
        lower = 2 * sum(Fraction(1, (2*k+1)*3**(2*k+1)) for k in range(count))
        tail = Fraction(2, (2*count+1)*3**(2*count+1)) * Fraction(9, 8)
        upper = lower + tail
        for scale in (*range(65), 80):
            factor = 10**scale
            floors = [v.numerator * factor // v.denominator
                      for v in (lower, upper, Fraction(literals[0]))]
            self.assertEqual(floors, [floors[0]] * 3, scale)

    def test_atan_unit_literal_matches_the_certified_enclosure(self):
        source = Path(SOURCE).read_text(encoding='utf-8')
        literals = re.findall(r'(?m)^\s*r=(\.\d{160})$', source)
        correction = re.findall(r'(?m)^\s*if \(w<=144\) y=(\.\d{160})/1$', source)
        self.assertEqual(len(literals), 1)
        self.assertEqual(correction, literals)
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
            if scale == 20:
                shortcut = re.findall(r'(?m)^\s*r=(\.\d{20})$', source)
                self.assertEqual([Fraction(value) for value in shortcut],
                                 [Fraction(rounded(lower), factor)])

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
        # Entry scales 114 and 115 produce working scales 144 and 145
        # for these short arguments, straddling constant admission.
        cases = [(scale, sign, text) for scale in (0, 20, 64, 100, 114, 115, 144, 145)
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

    def test_bounded_bessel_guard_error_budget(self):
        # Bound exp(8) using a rational Taylor prefix and a geometric tail.
        term = total = Fraction(1)
        for k in range(1, 65):
            term *= Fraction(8, k)
            total += term
        next_term = term * Fraction(8, 65)
        upper = total + next_term / (1 - Fraction(8, 66))
        self.assertLess(upper, 3000)
        for scale in range(101):
            with self.subTest(scale=scale):
                working = scale + 9
                # Otherwise the existing n>=3*x, n>=4*(scale+1)
                # tiny-result branch would already have returned zero.
                coefficient_steps = max(24, 4 * (scale + 1)) - 1
                series_steps = 2 * working + 16
                self.assertLessEqual(coefficient_steps, 403)
                self.assertLessEqual(series_steps, 234)
                # At k>=8, |q|/(k*(n+k))<=16/64. Stored
                # magnitudes never exceed the exact absolute terms.
                self.assertLess(Fraction(3000, 4 ** (2 * working + 8)),
                                Fraction(1, 10 ** working))
                # Each coefficient/term step loses at most two working
                # units, with absolute propagation below exp(8). Add
                # the squared-argument and input-truncation budgets.
                working_units = 1 + 2 * 3000 * (coefficient_steps + series_steps + 1)
                output_units = Fraction(working_units, 10 ** 9)
                self.assertLess(output_units, Fraction(4, 1000))
                self.assertLess(output_units + Fraction(1, 2), 1)

    def test_bessel_ascending_with_exact_rational_enclosures(self):
        arguments = ((0, 100, 150), (300, 300, 100), (1, '1.125', 100),
                     (1, '1.' + '23456789' * 12, 20))
        for order, argument, scale in arguments:
            with self.subTest(order=order, argument=argument, scale=scale):
                x = Fraction(argument)
                term = Fraction(1)
                for j in range(1, order + 1):
                    term *= x / (2 * j)
                partial = term
                for k in range(1, 2000):
                    ratio = x**2 / (4 * k * (order + k))
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

    def test_user_definition_replaces_a_called_library_function(self):
        lines = self.checked_lines('e(0)\ndefine e(x) {\nreturn(7)\n}\ne(0)\nscale(e(0))\n')
        self.assertEqual([Fraction(line) for line in lines], [1, 7, 0])

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
