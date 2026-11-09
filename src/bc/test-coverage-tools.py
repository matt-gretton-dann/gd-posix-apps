#!/usr/bin/env python3
"""Prevent new implementation objects or build modes disappearing from coverage."""
import importlib.util
import json
from pathlib import Path
import sys
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
                          'typed_results': None})
        self.assertIsNone(COLLECTOR.build_modes({})['prepared_divisors'])
        self.assertIsNone(COLLECTOR.build_modes({})['compact_execution'])
        cache['ENABLE_BC_COMPACT_EXECUTION'] = 'OFF'
        self.assertEqual(COLLECTOR.build_modes(cache)['compact_execution'], 'OFF')

    def test_typed_result_mode_distinguishes_old_and_explicit_builds(self):
        self.assertIsNone(COLLECTOR.build_modes({})['typed_results'])
        for mode in ('ON', 'OFF'):
            self.assertEqual(COLLECTOR.build_modes({'ENABLE_BC_TYPED_RESULTS': mode})[
                'typed_results'], mode)

    def compilation(self, names):
        build = self.source / 'build'
        build.mkdir(exist_ok=True)
        rows = []
        for name in names:
            target = 'bc' if name == 'bc.cc' else 'libbc'
            output = build / ('src/bc/CMakeFiles/' + target + '.dir/' + name + '.o')
            rows.append(dict(directory=str(build), file=str(self.directory / name),
                             output=str(output), command='clang++ -c ' + name + ' -o ' + str(output)))
        (build / 'compile_commands.json').write_text(json.dumps(rows))
        return build, rows

    def test_active_inventory_excludes_foreign_disabled_and_build_only_units(self):
        active = self.files('bc.cc', 'vm.cc', 'verification.cc', 'bc.hh', 'number.hh')
        self.files('native-expression-arm64.cc', 'native-expression-x64.cc',
                   'stack-execution.cc', 'verification-linear.cc', 'preparse-math-library.cc')
        build, _ = self.compilation(['bc.cc', 'vm.cc', 'verification.cc'])
        self.assertEqual(COLLECTOR.production_mappings(self.source, build), active)
        COLLECTOR.require_production_mappings(self.source, active, build)

    def test_active_optional_and_new_implementations_cannot_disappear(self):
        active = self.files('bc.cc', 'vm.cc', 'verification.cc', 'bc.hh', 'number.hh',
                            'native-expression-arm64.cc', 'future-kernel.cc')
        build, _ = self.compilation(['bc.cc', 'vm.cc', 'verification.cc',
                                     'native-expression-arm64.cc', 'future-kernel.cc'])
        for name in ('native-expression-arm64.cc', 'future-kernel.cc', 'vm.cc'):
            with self.subTest(name=name), self.assertRaisesRegex(RuntimeError, name):
                COLLECTOR.require_production_mappings(self.source, active - {self.directory / name}, build)
        COLLECTOR.require_production_mappings(self.source, active, build)

    def test_test_target_inventory_cannot_replace_a_production_core(self):
        self.files('bc.cc', 'vm.cc', 'verification.cc')
        build, rows = self.compilation(['bc.cc', 'vm.cc', 'verification.cc'])
        rows[1]['output'] = rows[1]['output'].replace('libbc.dir', 'test-libbc.dir')
        rows[1]['command'] = rows[1]['command'].replace('libbc.dir', 'test-libbc.dir')
        (build / 'compile_commands.json').write_text(json.dumps(rows))
        with self.assertRaisesRegex(RuntimeError, 'vm.cc'):
            COLLECTOR.production_mappings(self.source, build)

    def test_compile_command_fallback_supports_older_cmake_inventory(self):
        expected = self.files('bc.cc', 'vm.cc', 'verification.cc', 'bc.hh', 'number.hh')
        build, rows = self.compilation(['bc.cc', 'vm.cc', 'verification.cc'])
        for row in rows:
            del row['output']
        (build / 'compile_commands.json').write_text(json.dumps(rows))
        self.assertEqual(COLLECTOR.production_mappings(self.source, build), expected)

    def test_missing_inventory_fails_instead_of_shrinking_mapping_scope(self):
        self.files('bc.cc', 'vm.cc')
        build = self.source / 'build'
        build.mkdir()
        with self.assertRaisesRegex(RuntimeError, 'compile_commands.json'):
            COLLECTOR.production_mappings(self.source, build)

    def test_nonrunnable_selected_test_fails_before_profile_collection(self):
        for command in (None, [], [str(self.source / 'test-divisor-cache-lifetime')]):
            test = dict(name='bc/divisor-cache-lifetime')
            if command is not None:
                test['command'] = command
            with self.subTest(command=command), self.assertRaisesRegex(RuntimeError, 'bc/divisor-cache-lifetime'):
                COLLECTOR.coverage_tests([test])

    def test_coverage_selection_keeps_bc_and_unit_tests_and_explicit_skips(self):
        selected = [dict(name='bc/cli', command=[sys.executable, 'test-cli.py']),
                    dict(name='Number adds', command=[sys.executable, 'test-libbc']),
                    dict(name='bc/imported-skip', command=[sys.executable, 'skip.py'])]
        self.assertEqual(COLLECTOR.coverage_tests(selected + [dict(name='ar/unbuilt')]), selected)


if __name__ == '__main__':
    unittest.main()
