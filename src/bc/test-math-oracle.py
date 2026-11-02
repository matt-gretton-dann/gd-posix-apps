#!/usr/bin/env python3
"""Test exact ULP decisions and committed certified references, without FLINT."""
from fractions import Fraction
import hashlib
import importlib.util
import json
from pathlib import Path
import sys
import types
import unittest
from unittest import mock

ROOT = Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location('math_oracle', ROOT / 'scripts/math_oracle.py')
ORACLE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(ORACLE)
if hasattr(sys, 'set_int_max_str_digits'):
    sys.set_int_max_str_digits(1000000)


class Decisions(unittest.TestCase):
    def test_decimal_input_is_exact(self):
        self.assertEqual(ORACLE.decimal_fraction('.1'), Fraction(1, 10))
        self.assertEqual(ORACLE.decimal_fraction('-.00100'), Fraction(-1, 1000))
        self.assertEqual(ORACLE.decimal_fraction('0.000'), 0)
        for malformed in ('nan', 'inf', '1e20', '1/3', '+1', '1_000', ' 1', '1\n', '.'):
            with self.subTest(malformed=malformed), self.assertRaises(ValueError):
                ORACLE.decimal_fraction(malformed)

    def test_both_endpoints_control_acceptance(self):
        self.assertEqual(ORACLE.classify(Fraction(0), Fraction(-1), Fraction(1), 0), 'pass')
        self.assertEqual(ORACLE.classify(Fraction(0), Fraction(1), Fraction(2), 0), 'refine')
        self.assertEqual(ORACLE.classify(Fraction(0), Fraction(2), Fraction(3), 0), 'fail')
        self.assertEqual(ORACLE.classify(Fraction(0), Fraction(-3), Fraction(-2), 0), 'fail')
        self.assertEqual(ORACLE.classify(Fraction(0), Fraction(-2), Fraction(2), 0), 'refine')

    def test_exact_one_ulp_is_inclusive(self):
        ulp = Fraction(1, 100)
        self.assertEqual(ORACLE.classify(Fraction(0), ulp, ulp, 2), 'pass')
        self.assertEqual(ORACLE.classify(Fraction(0), -ulp, -ulp, 2), 'pass')
        self.assertEqual(ORACLE.classify(Fraction(0), ulp, ulp + ulp / 100, 2), 'refine')
        self.assertEqual(ORACLE.classify(Fraction(0), ulp + ulp / 100,
                                       ulp + ulp / 50, 2), 'fail')

    def test_refinement_resolves_ambiguous_decisions(self):
        candidate = Fraction(1)
        self.assertEqual(ORACLE.classify(candidate, Fraction(0), Fraction(3), 0), 'refine')
        self.assertEqual(ORACLE.classify(candidate, Fraction(3, 4), Fraction(5, 4), 0), 'pass')
        self.assertEqual(ORACLE.classify(candidate, Fraction(9, 4), Fraction(5, 2), 0), 'fail')

    def test_invalid_bounds_types_and_resource_limits_are_errors(self):
        with self.assertRaises(ValueError):
            ORACLE.classify(Fraction(0), Fraction(2), Fraction(1), 20)
        with self.assertRaises(TypeError):
            ORACLE.classify(0.1, Fraction(0), Fraction(1), 20)
        for scale in (-1, True, 1.5, '20', ORACLE.MAX_SCALE + 1):
            with self.subTest(scale=scale), self.assertRaises(ValueError):
                ORACLE.decimal_ulp(scale)

    def test_nearest_decimal_preserves_requested_scale_and_sign(self):
        for value, scale, expected in [(Fraction(1, 3), 2, '0.33'),
                                       (Fraction(-1, 3), 2, '-0.33'),
                                       (Fraction(0), 2, '0.00'),
                                       (Fraction(2), 0, '2')]:
            self.assertEqual(ORACLE.nearest_decimal(value, value, scale), expected)


class GeneratorFailures(unittest.TestCase):
    def module(self):
        spec = importlib.util.spec_from_file_location(
            'bc_math_generator', ROOT / 'scripts/generate-math-oracles.py')
        module = importlib.util.module_from_spec(spec)
        with mock.patch.dict(sys.modules, {'math_oracle': ORACLE}):
            spec.loader.exec_module(module)
        return module

    def test_exhausted_reference_budget_is_an_error(self):
        context = types.SimpleNamespace(prec=53)
        dependency = types.SimpleNamespace(arb=None, ctx=context, fmpq=None)
        with mock.patch.dict(sys.modules, {'flint': dependency}):
            with self.assertRaisesRegex(ValueError, 'precision budget'):
                self.module().enclosure('s', ['1'], 1000, 16, 128)
        self.assertEqual(context.prec, 53)

    def test_reference_exception_restores_precision(self):
        context = types.SimpleNamespace(prec=91)

        def unavailable(value):
            raise RuntimeError('deliberate reference failure')

        dependency = types.SimpleNamespace(arb=unavailable, ctx=context, fmpq=Fraction)
        with mock.patch.dict(sys.modules, {'flint': dependency}):
            with self.assertRaisesRegex(RuntimeError, 'deliberate reference failure'):
                self.module().enclosure('s', ['1'], 20, 16, 65536)
        self.assertEqual(context.prec, 91)


class CertifiedFixtures(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.report = json.loads((ROOT / 'fixtures/math-oracles.json').read_text(encoding='utf-8'))
        cls.cases = cls.report['cases']

    def test_generator_and_input_identities_are_preserved(self):
        for key, path in [('generator_sha256', ROOT / 'scripts/generate-math-oracles.py'),
                          ('case_file_sha256', ROOT / 'fixtures/math-oracle-cases.json')]:
            self.assertEqual(self.report[key], hashlib.sha256(path.read_bytes()).hexdigest())
        self.assertEqual(self.report['python_flint_version'], '0.9.0')
        self.assertEqual(self.report['flint_version'], '3.6.0')
        self.assertEqual(self.report['endpoint_format'], 'exact rational outward dyadic bounds')
        for path in (ROOT / 'fixtures/math-oracles.json',
                     ROOT / 'fixtures/math-oracle-cases.json',
                     ROOT / 'scripts/generate-math-oracles.py'):
            self.assertNotIn(b'\r', path.read_bytes())

    def test_boundary_reference_identities_and_enclosures(self):
        for prefix in ('math-crossover', 'math-trig'):
            report = json.loads((ROOT / ('fixtures/' + prefix + '-oracles.json')).read_text())
            source = ROOT / ('fixtures/' + prefix + '-cases.json')
            specification = json.loads(source.read_text())
            self.assertEqual(report['case_file_sha256'], hashlib.sha256(source.read_bytes()).hexdigest())
            self.assertEqual(report['generator_sha256'], self.report['generator_sha256'])
            expected = {(case['id'], scale) for case in specification['cases']
                        for scale in specification['scales']}
            self.assertEqual(len(report['cases']), len(expected))
            self.assertEqual({(case['id'], case['scale']) for case in report['cases']}, expected)
            by_id = {case['id']: case for case in specification['cases']}
            for case in report['cases']:
                with self.subTest(case=case['id'], scale=case['scale']):
                    self.assertEqual(case['function'], by_id[case['id']]['function'])
                    self.assertEqual(case['arguments'], by_id[case['id']]['arguments'])
                    lower, upper = Fraction(case['lower']), Fraction(case['upper'])
                    ulp = ORACLE.decimal_ulp(case['scale'])
                    self.assertLessEqual(lower, upper)
                    self.assertLessEqual(upper - lower, ulp / 10 ** report['guard_digits'])
                    candidate = ORACLE.decimal_fraction(ORACLE.nearest_decimal(lower, upper, case['scale']))
                    self.assertEqual(ORACLE.classify(candidate, lower, upper, case['scale']), 'pass')
                    self.assertEqual(ORACLE.classify(candidate + 3 * ulp, lower, upper,
                                                    case['scale']), 'fail')

    def test_imported_decisions_account_for_computed_arguments(self):
        spec = importlib.util.spec_from_file_location('imported_math', ROOT / 'test-math-imported.py')
        adapter = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(adapter)
        reference = dict(function='s', arguments=['0'], scale=2, lower='0', upper='0')
        actual = dict(function='s', arguments=['.001'], scale=2, value='.001')
        adapter.certify(actual, reference)
        with self.assertRaises(AssertionError):
            adapter.certify(dict(actual, value='.02'), reference)
        # An uncertain boundary must request refinement, not pass or count as
        # a proven numerical failure. The widened enclosure crosses one ULP.
        with self.assertRaisesRegex(AssertionError, 'refine'):
            adapter.certify(dict(actual, value='.010'), reference)
        with self.assertRaisesRegex(AssertionError, 'regeneration'):
            adapter.certify(dict(actual, function='e'), dict(reference, function='e'))
        replacement = dict(reference, function='e', arguments=['.5'],
                           user_redefinition='identity replacement of e')
        identity = dict(function='e', arguments=['.5'], scale=2, value='.5')
        adapter.certify(identity, replacement)
        with self.assertRaises(AssertionError):
            adapter.certify(dict(identity, value='.6'), replacement)

    def test_every_case_and_scale_has_a_narrow_dyadic_enclosure(self):
        specification = json.loads((ROOT / 'fixtures/math-oracle-cases.json').read_text())
        expected = {(c['id'], s) for c in specification['cases'] for s in specification['scales']}
        self.assertEqual({(c['id'], c['scale']) for c in self.cases}, expected)
        self.assertEqual(len(self.cases), len(expected))
        by_id = {c['id']: c for c in specification['cases']}
        for case in self.cases:
            self.assertEqual(case['function'], by_id[case['id']]['function'])
            self.assertEqual(case['arguments'], by_id[case['id']]['arguments'])
        self.assertEqual({c['function'] for c in self.cases}, set('scalej'))
        self.assertEqual(set(specification['scales']), {0, 1, 2, 20, 100, 1000, 2000})
        for case in self.cases:
            with self.subTest(case=case['id'], scale=case['scale']):
                lower, upper = Fraction(case['lower']), Fraction(case['upper'])
                self.assertLessEqual(lower, upper)
                self.assertLessEqual(upper - lower, ORACLE.decimal_ulp(case['scale']) /
                                     10 ** self.report['guard_digits'])
                for endpoint in (lower, upper):
                    self.assertEqual(endpoint.denominator & (endpoint.denominator - 1), 0)

    def test_all_enclosures_accept_nearby_values_and_reject_deliberate_faults(self):
        for case in self.cases:
            with self.subTest(case=case['id'], scale=case['scale']):
                lower, upper = Fraction(case['lower']), Fraction(case['upper'])
                value = ORACLE.decimal_fraction(ORACLE.nearest_decimal(lower, upper, case['scale']))
                ulp = ORACLE.decimal_ulp(case['scale'])
                self.assertEqual(ORACLE.classify(value, lower, upper, case['scale']), 'pass')
                self.assertEqual(ORACLE.classify(value + 3 * ulp, lower, upper,
                                                case['scale']), 'fail')
                self.assertEqual(ORACLE.classify(value - 3 * ulp, lower, upper,
                                                case['scale']), 'fail')

    def test_exact_special_cases_have_exact_references(self):
        expected = {'s-zero': 0, 'c-zero': 1, 'a-zero': 0, 'l-one': 0,
                    'e-zero': 1, 'j-zero-zero': 1, 'j-one-zero': 0, 'j-negative-zero': 0}
        for case in self.cases:
            if case['id'] in expected:
                self.assertEqual(Fraction(case['lower']), expected[case['id']])
                self.assertEqual(Fraction(case['upper']), expected[case['id']])


if __name__ == '__main__':
    unittest.main()
