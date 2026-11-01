#!/usr/bin/env python3
"""Certify ordinary -l functions and their observable language behavior."""
import argparse
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
        cases = json.loads((ROOT / 'fixtures/math-oracles.json').read_text())['cases']
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

    def test_basic_exact_values_and_result_scale(self):
        lines = self.checked_lines('scale=3\ne(0)\nl(1)\na(0)\ns(0)\nc(0)\n'
                                   'j(0,0)\nj(1,0)\nscale(e(0))\nscale(s(0))\nscale\n')
        self.assertEqual([Fraction(x) for x in lines], [1, 0, 0, 0, 1, 1, 0, 3, 3, 3])

    def test_all_certified_function_regimes(self):
        self.certified()

    def test_identical_source_loads_as_ordinary_input(self):
        self.certified(options=(), files=(SOURCE,))

    def test_no_library_means_no_math_definitions(self):
        result = self.invoke('e(0)\n', options=())
        self.assertNotEqual(result.returncode, 0)
        self.assertNotEqual(result.stderr, '')

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
