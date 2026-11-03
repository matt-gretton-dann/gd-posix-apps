#!/usr/bin/env python3
"""Prevent new implementation objects or build modes disappearing from coverage."""
import importlib.util
from pathlib import Path
import tempfile
import unittest

SPEC = importlib.util.spec_from_file_location(
    'coverage_collector', Path(__file__).with_name('scripts') / 'collect-coverage.py')
COLLECTOR = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(COLLECTOR)


class Coverage(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.source = Path(self.temporary.name).resolve()
        self.directory = self.source / 'src/bc'
        self.directory.mkdir(parents=True)

    def files(self, *names):
        result = set()
        for name in names:
            path = self.directory / name
            path.write_text('// coverage inventory fixture\n')
            result.add(path)
        return result

    def test_missing_new_implementation_fails_the_inventory(self):
        baseline = self.files('bc.cc', 'vm.cc', 'bc.hh', 'number.hh')
        self.files('number-division-cache.cc', 'future-kernel.cc')
        with self.assertRaisesRegex(RuntimeError, 'future-kernel.cc'):
            COLLECTOR.require_production_mappings(self.source, baseline)
        COLLECTOR.require_production_mappings(
            self.source, baseline | {self.directory / 'number-division-cache.cc',
                                     self.directory / 'future-kernel.cc'})

    def test_standalone_and_incompatible_diagnostics_are_not_application_objects(self):
        expected = self.files('bc.cc', 'number.hh')
        self.files('test-extra.cc', 'benchmark-components.cc', 'find-multiply-split-point.cc',
                   'profiling.cc', 'profiling-entry.cc')
        self.assertEqual(COLLECTOR.production_mappings(self.source), expected)
        COLLECTOR.require_production_mappings(self.source, expected)

    def test_empty_mapping_cannot_hide_an_implementation(self):
        self.files('number-sqrt.cc', 'number-remainder.cc', 'execution-plan.cc')
        with self.assertRaisesRegex(RuntimeError, 'missing production mappings'):
            COLLECTOR.require_production_mappings(self.source, set())

    def test_activation_modes_remain_separate(self):
        cache = {'ENABLE_NON_POSIX_EXTENSIONS': 'OFF', 'ENABLE_BC_PREPARED_DIVISORS': 'ON',
                 'ENABLE_BC_COMPACT_EXECUTION': 'ON'}
        self.assertEqual(COLLECTOR.build_modes(cache),
                         {'extensions': 'OFF', 'prepared_divisors': 'ON', 'compact_execution': 'ON',
                          'outlined_division': None})
        self.assertIsNone(COLLECTOR.build_modes({})['prepared_divisors'])
        self.assertIsNone(COLLECTOR.build_modes({})['compact_execution'])
        cache['ENABLE_BC_COMPACT_EXECUTION'] = 'OFF'
        self.assertEqual(COLLECTOR.build_modes(cache)['compact_execution'], 'OFF')

    def test_division_driver_modes_and_legacy_unknown_remain_separate(self):
        self.assertIsNone(COLLECTOR.build_modes({}).get('outlined_division'))
        for setting in ('ON', 'OFF'):
            self.assertEqual(COLLECTOR.build_modes(
                {'ENABLE_BC_OUTLINED_DIVISION': setting}).get('outlined_division'), setting)


if __name__ == '__main__':
    unittest.main()
