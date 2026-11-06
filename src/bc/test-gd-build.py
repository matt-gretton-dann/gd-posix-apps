#!/usr/bin/env python3
"""Check immutable source requests and reject misleading GD build provenance."""
import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


class GdBuild(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        spec = importlib.util.spec_from_file_location('gd_builder', BUILDER)
        self.module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.module)
        self.helper = self.module.helpers()
        self.build = self.root / 'build'
        (self.build / 'src/bc').mkdir(parents=True)
        self.cache = self.build / 'CMakeCache.txt'
        self.cache.write_text('ENABLE_BC_PROFILE:BOOL=OFF\nENABLE_BC_COVERAGE:BOOL=OFF\n'
                              'CMAKE_BUILD_TYPE:STRING=Release\nCMAKE_CXX_FLAGS:STRING=\n'
                              'CMAKE_CXX_FLAGS_RELEASE:STRING=-O3 -DNDEBUG\n')
        self.header = self.build / 'src/bc/find-multiply-split-point.hh'
        self.header.write_text('// explicit BC_MULTIPLY_SPLIT_POINT\n'
                               '#define BC_MULTIPLY_SPLIT_POINT 126\n')
        (self.build / 'compile_commands.json').write_text(json.dumps([
            {'command': 'c++ -O3 -DNDEBUG -std=c++20 -c source.cc'}]))

    def metadata(self, prepared_mode='AUTO'):
        return self.module.build_metadata(self.helper, self.root, self.root / 'source',
                                           self.build, 'a' * 40, 126,
                                           {'version': 'C'}, {'version': 'C++'}, prepared_mode=prepared_mode)

    def test_records_compiled_threshold_and_commands(self):
        value = self.metadata()
        self.assertEqual(value['configuration']['multiply_split_point'], 126)
        self.assertEqual(value['configuration']['threshold_header_sha256'],
                         hashlib.sha256(self.header.read_bytes()).hexdigest())
        self.assertEqual((self.root / 'compile_commands.json').read_bytes(),
                         (self.build / 'compile_commands.json').read_bytes())
        self.assertFalse(value['instrumented'])
        self.assertEqual(value['configuration']['ipo_policy']['status'], 'legacy-auto')

    def prepared_commands(self, value, arguments=False):
        rows = []
        for name in ('vm.cc', 'bc.cc'):
            row = {'file': '/source/src/bc/' + name}
            flags = ['c++', '-DGD_BC_PREPARED_DIVISORS=' + value, '-c', name]
            row['arguments' if arguments else 'command'] = flags if arguments else ' '.join(flags)
            rows.append(row)
        (self.build / 'compile_commands.json').write_text(json.dumps(rows))

    def live_commands(self, enabled=True, arguments=False):
        self.cache.write_text(self.cache.read_text() + 'ENABLE_BC_LIVE_RESULTS:BOOL=' +
                              ('ON' if enabled else 'OFF') + '\n')
        rows = []
        for name in ('vm.cc', 'live-result-plan.cc') if enabled else ('vm.cc',):
            flags = ['c++', '-DGD_BC_LIVE_RESULTS=' + ('1' if enabled else '0'),
                     '-o', 'src/bc/CMakeFiles/libbc.dir/' + name + '.o', '-c', name]
            rows.append({'file': '/source/src/bc/' + name,
                         'arguments' if arguments else 'command': flags if arguments else ' '.join(flags)})
        path = self.build / 'compile_commands.json'
        path.write_text(json.dumps(rows))
        return rows

    def test_live_legacy_mode_is_unknown_and_explicit_modes_decline(self):
        value = self.module.verified_live_results(self.helper, self.build, 'AUTO')
        self.assertEqual(value['status'], 'legacy-auto')
        self.assertIsNone(value['enabled'])
        for requested in ('ON', 'OFF', 'invalid'):
            with self.subTest(mode=requested), self.assertRaises(self.helper.BuildError):
                self.module.verified_live_results(self.helper, self.build, requested)

    def test_live_compilation_proves_enabled_and_disabled_library_inventory(self):
        for enabled in (False, True):
            self.live_commands(enabled, arguments=enabled)
            value = self.module.verified_live_results(self.helper, self.build, 'ON' if enabled else 'OFF')
            self.assertIs(value['enabled'], enabled)
            self.assertEqual(value['status'], 'recorded')
            self.assertEqual(value['verification']['translation_units'], 2 if enabled else 1)
            with self.assertRaises(self.helper.BuildError):
                self.module.verified_live_results(self.helper, self.build, 'OFF' if enabled else 'ON')

    def test_live_options_have_compiler_token_boundaries(self):
        path = self.build / 'compile_commands.json'
        for spelling in ('-D GD_BC_LIVE_RESULTS=1', '/DGD_BC_LIVE_RESULTS=1',
                         '/D"GD_BC_LIVE_RESULTS=1"'):
            rows = self.live_commands()
            for row in rows:
                row['command'] = row['command'].replace('-DGD_BC_LIVE_RESULTS=1', spelling)
            path.write_text(json.dumps(rows))
            self.assertTrue(self.module.verified_live_results(self.helper, self.build, 'ON')['enabled'])
        rows = self.live_commands()
        for row in rows:
            row['command'] = row['command'].replace('-DGD_BC_LIVE_RESULTS=1',
                                                  '-DOTHER="literal -DGD_BC_LIVE_RESULTS=1"')
        path.write_text(json.dumps(rows))
        with self.assertRaises(self.helper.BuildError):
            self.module.verified_live_results(self.helper, self.build, 'ON')

    def test_live_cache_only_conflicts_and_unverifiable_inputs_decline(self):
        path = self.build / 'compile_commands.json'
        for change in ('missing-vm', 'missing-planner', 'test-only-planner', 'opposite', 'undefine',
                       'bare', 'extended', 'response', 'malformed', 'bad-arguments'):
            rows = self.live_commands()
            if change == 'missing-vm':
                rows.pop(0)
            elif change == 'missing-planner':
                rows.pop()
            elif change == 'test-only-planner':
                rows[1]['command'] = rows[1]['command'].replace('libbc.dir', 'test-libbc.dir')
            elif change == 'opposite':
                rows[0]['command'] += ' -DGD_BC_LIVE_RESULTS=0'
            elif change == 'undefine':
                rows[0]['command'] += ' -UGD_BC_LIVE_RESULTS'
            elif change == 'bare':
                rows[0]['command'] = rows[0]['command'].replace('LIVE_RESULTS=1', 'LIVE_RESULTS')
            elif change == 'extended':
                rows[0]['command'] = rows[0]['command'].replace('LIVE_RESULTS=1', 'LIVE_RESULTS=10')
            elif change == 'response':
                rows[0]['command'] += ' @unexpanded-options.rsp'
            elif change == 'bad-arguments':
                rows[0]['arguments'] = [42]
            path.write_text('{' if change == 'malformed' else json.dumps(rows))
            with self.subTest(change=change), self.assertRaises(self.helper.BuildError):
                self.module.verified_live_results(self.helper, self.build, 'ON')

    def live_project(self, enabled=True, override=None, exclude=False, configuration='Release'):
        from xml.etree import ElementTree as ET
        commands = self.build / 'compile_commands.json'
        if commands.exists():
            commands.unlink()
        self.cache.write_text(self.cache.read_text() + 'CMAKE_GENERATOR:INTERNAL=Visual Studio 17 2022\n'
                              'ENABLE_BC_LIVE_RESULTS:BOOL=' + ('ON' if enabled else 'OFF') + '\n')
        root = ET.Element('Project', xmlns='http://schemas.microsoft.com/developer/msbuild/2003')
        group = ET.SubElement(root, 'ItemDefinitionGroup',
                              Condition="'$(Configuration)|$(Platform)'=='" + configuration + "|x64'")
        compile = ET.SubElement(group, 'ClCompile')
        ET.SubElement(compile, 'PreprocessorDefinitions').text = (
            '%(PreprocessorDefinitions);GD_BC_LIVE_RESULTS=' + ('1' if enabled else '0'))
        items = ET.SubElement(root, 'ItemGroup')
        vm = ET.SubElement(items, 'ClCompile', Include='C:\\source\\src\\bc\\vm.cc')
        if override is not None:
            ET.SubElement(vm, 'PreprocessorDefinitions').text = override
        if exclude:
            ET.SubElement(vm, 'ExcludedFromBuild').text = 'true'
        if enabled:
            ET.SubElement(items, 'ClCompile', Include='C:\\source\\src\\bc\\live-result-plan.cc')
        path = self.build / 'src/bc/libbc.vcxproj'
        ET.ElementTree(root).write(str(path), encoding='utf-8')
        return path

    def test_live_native_projects_prove_release_mode_and_source_inventory(self):
        for enabled in (True, False):
            self.live_project(enabled)
            value = self.module.verified_live_results(self.helper, self.build, 'ON' if enabled else 'OFF')
            self.assertIs(value['enabled'], enabled)
            self.assertEqual(value['verification']['kind'], 'msbuild-projects')
            self.assertIn('src/bc/libbc.vcxproj', value['verification']['files'])
        for options in ({'override': '%(PreprocessorDefinitions);GD_BC_LIVE_RESULTS=0'},
                        {'override': 'OTHER=1'}, {'exclude': True}, {'configuration': 'Debug'}):
            self.live_project(**options)
            with self.subTest(options=options), self.assertRaises(self.helper.BuildError):
                self.module.verified_live_results(self.helper, self.build, 'ON')

    def test_live_mode_is_retained_in_build_metadata(self):
        self.live_commands()
        value = self.metadata()['configuration']['live_results_policy']
        self.assertTrue(value['enabled'])
        self.assertEqual(value['verification']['sha256'],
                         hashlib.sha256((self.root / 'compile_commands.json').read_bytes()).hexdigest())
        project = self.live_project()
        value = self.metadata()['configuration']['live_results_policy']
        relative = 'src/bc/libbc.vcxproj'
        self.assertEqual((self.root / 'live-results-policy' / relative).read_bytes(), project.read_bytes())
        self.assertEqual(value['verification']['files'][relative], hashlib.sha256(project.read_bytes()).hexdigest())

    def test_live_candidate_requires_explicit_verified_activation(self):
        policy = dict(status='recorded', mode='ON', enabled=True)
        def entry(value):
            return dict(metadata=dict(configuration=dict(live_results_policy=value)))
        self.module.require_live_candidate(self.helper, entry(policy))
        for value in (None, {}, dict(policy, mode='AUTO'), dict(policy, status='legacy-auto'),
                      dict(policy, enabled=False), dict(policy, enabled=1)):
            with self.subTest(policy=value), self.assertRaises(self.helper.BuildError):
                self.module.require_live_candidate(self.helper, entry(value))

    def test_live_option_is_exposed_by_builder(self):
        child = subprocess.run([sys.executable, BUILDER, '--help'], stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE, timeout=5, universal_newlines=True)
        self.assertEqual(child.returncode, 0)
        self.assertIn('--live-results', child.stdout)

    def test_candidate_requires_explicit_verified_activation(self):
        policy = dict(status='recorded', mode='ON', enabled=True)
        def entry(value):
            return dict(metadata=dict(configuration=dict(prepared_divisors_policy=value)))
        self.module.require_prepared_candidate(self.helper, entry(policy))
        invalid = [None, {}, dict(policy, status='legacy-auto'), dict(policy, mode='AUTO'),
                   dict(policy, mode='OFF'), dict(policy, enabled=False), dict(policy, enabled=1)]
        for value in invalid:
            with self.subTest(policy=value), self.assertRaises(self.helper.BuildError):
                self.module.require_prepared_candidate(self.helper, entry(value))
        for value in (None, {}, dict(metadata=None), dict(metadata=dict(configuration=None))):
            with self.subTest(entry=value), self.assertRaises(self.helper.BuildError):
                self.module.require_prepared_candidate(self.helper, value)

    def test_prepared_legacy_auto_is_not_mislabeled_disabled(self):
        policy = self.metadata()['configuration']['prepared_divisors_policy']
        self.assertEqual(policy['status'], 'legacy-auto')
        self.assertIsNone(policy['enabled'])
        for requested in ('ON', 'OFF'):
            with self.assertRaises(self.helper.BuildError):
                self.module.verified_prepared_divisors(self.helper, self.build, requested)

    def test_prepared_modes_verify_both_compiled_anchors(self):
        for value, enabled in (('ON', True), ('TRUE', True), ('OFF', False), ('0', False)):
            self.cache.write_text(self.cache.read_text() +
                                  'ENABLE_BC_PREPARED_DIVISORS:BOOL=' + value + '\n')
            self.prepared_commands('1' if enabled else '0', arguments=enabled)
            policy = self.module.verified_prepared_divisors(self.helper, self.build, 'AUTO')
            self.assertEqual(policy['enabled'], enabled)
            self.assertEqual(policy['verification']['translation_units'], 2)
            with self.assertRaises(self.helper.BuildError):
                self.module.verified_prepared_divisors(
                    self.helper, self.build, 'OFF' if enabled else 'ON')

    def test_prepared_ignored_key_missing_anchor_and_override_are_rejected(self):
        self.cache.write_text(self.cache.read_text() + 'ENABLE_BC_PREPARED_DIVISORS:BOOL=ON\n')
        with self.assertRaises(self.helper.BuildError):
            self.module.verified_prepared_divisors(self.helper, self.build, 'ON')
        self.prepared_commands('1')
        path = self.build / 'compile_commands.json'
        original = json.loads(path.read_text())
        for change in ('missing', 'opposite', 'undefine', 'extended-value', 'unvalued'):
            rows = json.loads(json.dumps(original))
            if change == 'missing':
                rows.pop()
            elif change == 'opposite':
                rows[0]['command'] += ' -DGD_BC_PREPARED_DIVISORS=0'
            elif change == 'undefine':
                rows[0]['command'] += ' -UGD_BC_PREPARED_DIVISORS'
            elif change == 'extended-value':
                rows[0]['command'] = rows[0]['command'].replace('DIVISORS=1', 'DIVISORS=10')
            else:
                rows[0]['command'] = rows[0]['command'].replace('DIVISORS=1', 'DIVISORS')
            path.write_text(json.dumps(rows))
            with self.subTest(change=change), self.assertRaises(self.helper.BuildError):
                self.module.verified_prepared_divisors(self.helper, self.build, 'ON')

    def test_prepared_policy_is_retained_in_metadata(self):
        self.cache.write_text(self.cache.read_text() + 'ENABLE_BC_PREPARED_DIVISORS:BOOL=OFF\n')
        self.prepared_commands('0')
        policy = self.metadata('OFF')['configuration']['prepared_divisors_policy']
        self.assertEqual(policy['status'], 'recorded')
        self.assertFalse(policy['enabled'])
        self.assertEqual(policy['verification']['sha256'],
                         hashlib.sha256((self.root / 'compile_commands.json').read_bytes()).hexdigest())

    def prepared_projects(self, value='1', configuration='Release'):
        commands = self.build / 'compile_commands.json'
        if commands.exists():
            commands.unlink()
        self.cache.write_text(self.cache.read_text() + 'CMAKE_GENERATOR:INTERNAL=Visual Studio 17 2022\n'
                              'ENABLE_BC_PREPARED_DIVISORS:BOOL=ON\n')
        for name in ('libbc', 'bc'):
            (self.build / ('src/bc/' + name + '.vcxproj')).write_text(
                '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'
                '<ItemDefinitionGroup Condition="config==' + configuration + '|x64">'
                '<ClCompile><PreprocessorDefinitions>WIN32;GD_BC_PREPARED_DIVISORS=' + value +
                ';%(PreprocessorDefinitions)</PreprocessorDefinitions></ClCompile>'
                '</ItemDefinitionGroup></Project>')

    def test_prepared_native_vs_projects_are_verified_and_retained(self):
        self.prepared_projects()
        policy = self.metadata('ON')['configuration']['prepared_divisors_policy']
        self.assertTrue(policy['enabled'])
        self.assertEqual(policy['verification']['kind'], 'msbuild-projects')
        for relative, expected in policy['verification']['files'].items():
            self.assertEqual(hashlib.sha256((self.root / 'prepared-policy' / relative).read_bytes()).hexdigest(),
                             expected)
        for value, configuration in (('0', 'Release'), ('1', 'Debug')):
            self.prepared_projects(value, configuration)
            with self.assertRaises(self.helper.BuildError):
                self.module.verified_prepared_divisors(self.helper, self.build, 'ON')

    def test_prepared_without_compilation_evidence_is_rejected(self):
        self.cache.write_text(self.cache.read_text() + 'ENABLE_BC_PREPARED_DIVISORS:BOOL=ON\n')
        (self.build / 'compile_commands.json').unlink()
        with self.assertRaises(self.helper.BuildError):
            self.module.verified_prepared_divisors(self.helper, self.build, 'ON')

    def test_explicit_ipo_rejects_ignored_cache_variable(self):
        # An older revision can cache any -D value without using it. Require
        # the production module's result in addition to the requested key.
        self.cache.write_text(self.cache.read_text() + 'IPO_MODE:STRING=OFF\n')
        for requested in ('OFF', 'REQUIRED'):
            with self.assertRaises(self.helper.BuildError):
                self.module.verified_ipo_policy(self.helper, self.build, requested)

    def test_explicit_ipo_checks_requested_and_applied_modes(self):
        report = self.build / 'gd-ipo-policy.json'
        for mode, enabled in (('AUTO', True), ('AUTO', False),
                              ('OFF', False), ('REQUIRED', True)):
            self.cache.write_text('IPO_MODE:STRING=' + mode + '\n')
            value = dict(schema=1, mode=mode, automatic_ipo=enabled)
            report.write_text(json.dumps(value))
            self.assertEqual(self.module.verified_ipo_policy(self.helper, self.build, mode),
                             dict(status='recorded', **value))
            if mode != 'AUTO':
                value['automatic_ipo'] = not enabled
                report.write_text(json.dumps(value))
                with self.assertRaises(self.helper.BuildError):
                    self.module.verified_ipo_policy(self.helper, self.build, mode)

    def test_stale_or_malformed_ipo_reports_are_rejected(self):
        self.cache.write_text('IPO_MODE:STRING=OFF\n')
        report = self.build / 'gd-ipo-policy.json'
        for payload in ('not json', '[]',
                        '{"schema":1,"mode":"AUTO","automatic_ipo":false}',
                        '{"schema":1,"mode":"OFF","automatic_ipo":"false"}',
                        '{"schema":true,"mode":"OFF","automatic_ipo":false}',
                        '{"schema":2,"mode":"OFF","automatic_ipo":false}'):
            report.write_text(payload)
            with self.assertRaises(self.helper.BuildError):
                self.module.verified_ipo_policy(self.helper, self.build, 'OFF')

    def test_ipo_policy_bytes_are_retained_in_build_provenance(self):
        self.cache.write_text(self.cache.read_text() + 'IPO_MODE:STRING=AUTO\n')
        report = self.build / 'gd-ipo-policy.json'
        report.write_bytes(b'{"schema":1,"mode":"AUTO","automatic_ipo":true}\n')
        value = self.metadata()['configuration']['ipo_policy']
        self.assertEqual(value['status'], 'recorded')
        self.assertEqual(value['report_sha256'], hashlib.sha256(report.read_bytes()).hexdigest())
        self.assertEqual((self.root / report.name).read_bytes(), report.read_bytes())

    def test_experimental_layout_requires_compiled_selection(self):
        self.assertEqual(self.module.verified_array_layout(self.helper, self.build, 'hybrid'), 'hybrid')
        for number, layout in enumerate(('hybrid', 'paged', 'flat', 'hash', 'ordered')):
            self.cache.write_text('BC_ARRAY_STORAGE:STRING=' + layout + '\n')
            commands = self.build / 'compile_commands.json'
            commands.write_text(json.dumps([{'command': 'c++ -DGD_BC_ARRAY_STORAGE=' + str(number)}]))
            self.assertEqual(self.module.verified_array_layout(self.helper, self.build, layout), layout)
            if layout != 'hybrid':
                commands.write_text(json.dumps([{'command': 'c++ -DGD_BC_ARRAY_STORAGE=' + str(number + 10)}]))
                with self.assertRaises(self.helper.BuildError):
                    self.module.verified_array_layout(self.helper, self.build, layout)
                commands.unlink()
                with self.assertRaises(self.helper.BuildError):
                    self.module.verified_array_layout(self.helper, self.build, layout)
        self.cache.write_text('BC_ARRAY_STORAGE:STRING=hybrid\n')
        with self.assertRaises(self.helper.BuildError):
            self.module.verified_array_layout(self.helper, self.build, 'paged')

    def test_instrumented_configuration_is_rejected(self):
        for variable in ('ENABLE_BC_PROFILE', 'ENABLE_BC_COVERAGE'):
            previous = self.cache.read_text()
            self.cache.write_text(previous.replace(variable + ':BOOL=OFF', variable + ':BOOL=ON'))
            with self.assertRaises(self.helper.BuildError):
                self.metadata()
            self.cache.write_text(previous)
        for flag in ('-fsanitize=address', '-fprofile-instr-generate', '-fprofile-arcs', '--coverage'):
            (self.build / 'compile_commands.json').write_text(json.dumps([{'command': 'c++ ' + flag}]))
            with self.assertRaises(self.helper.BuildError):
                self.metadata()

    def test_diagnostic_mode_is_explicit_and_forbidden_for_release_metadata(self):
        self.cache.write_text(self.cache.read_text().replace('ENABLE_BC_PROFILE:BOOL=OFF',
                                                           'ENABLE_BC_PROFILE:BOOL=ON'))
        commands = self.build / 'compile_commands.json'
        commands.write_text(json.dumps([{'command': 'c++ -O3 -DGD_BC_PROFILE=1 -c source.cc'}]))
        with self.assertRaises(self.helper.BuildError):
            self.metadata()
        value = self.module.build_metadata(self.helper, self.root, self.root / 'source',
                                           self.build, 'a' * 40, 126,
                                           {'version': 'C'}, {'version': 'C++'}, diagnostic_profile=True)
        self.assertTrue(value['instrumented'])
        self.assertTrue(value['configuration']['diagnostic_profile'])
        commands.write_text(json.dumps([{'command': 'c++ -O3 -c source.cc'}]))
        with self.assertRaises(self.helper.BuildError):
            self.module.build_metadata(self.helper, self.root, self.root / 'source', self.build,
                                       'a' * 40, 126, {'version': 'C'}, {'version': 'C++'}, diagnostic_profile=True)
        self.cache.write_text(self.cache.read_text().replace('ENABLE_BC_COVERAGE:BOOL=OFF',
                                                           'ENABLE_BC_COVERAGE:BOOL=ON'))
        with self.assertRaises(self.helper.BuildError):
            self.module.build_metadata(self.helper, self.root, self.root / 'source', self.build,
                                       'a' * 40, 126, {'version': 'C'}, {'version': 'C++'}, diagnostic_profile=True)

    def test_wrong_or_automatic_threshold_is_rejected(self):
        for content in ('#define BC_MULTIPLY_SPLIT_POINT 127\n',
                        '#define BC_MULTIPLY_SPLIT_POINT 126\n',
                        '#include "find-multiply-split-point-auto.hh"\n'):
            self.header.write_text(content)
            with self.assertRaises(self.helper.BuildError):
                self.metadata()

    def test_only_immutable_revisions_and_valid_points(self):
        self.assertEqual(self.module.revision_value('A' * 40), 'a' * 40)
        for value in ('main', 'HEAD', 'a' * 39, '--revision'):
            with self.assertRaises(argparse.ArgumentTypeError):
                self.module.revision_value(value)
        for value in ('0', '-1', '1e3', '01', '1000000001'):
            with self.assertRaises(argparse.ArgumentTypeError):
                self.module.positive_point(value)

    @unittest.skipUnless(os.name == 'posix', 'Unix compiler invocation aliases')
    def test_compiler_alias_preserves_driver_behavior(self):
        driver = self.root / 'compiler-driver'
        driver.write_text('#!/bin/sh\nprintf "%s\\n" "${0##*/}"\n')
        driver.chmod(0o755)
        alias = self.root / 'clang++'
        alias.symlink_to(driver)
        builder = self.helper.Builder(self.root, 3)
        value = self.module.compiler_identity(self.helper, builder, str(alias))
        self.assertEqual(value['realpath'], str(driver.resolve()))
        process = subprocess.run([value['argv0'], '--version'], stdout=subprocess.PIPE,
                                 universal_newlines=True, timeout=3)
        self.assertEqual(process.stdout.strip(), 'clang++')

    def test_failed_source_checkout_never_publishes_config(self):
        source = self.root / 'not-a-repository'
        source.mkdir()
        output = self.root / 'failed-build'
        child = subprocess.run([sys.executable, BUILDER, '--source', str(source),
                                '--revision', 'a' * 40, '--split-point', '126',
                                '--output', str(output)], stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE, timeout=5)
        self.assertEqual(child.returncode, 1)
        self.assertFalse((output / 'implementations.json').exists())
        manifest = json.loads((output / 'build-manifest.json').read_text())
        self.assertEqual(manifest['status'], 'incomplete')
        self.assertIn('failure', manifest)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('builder')
    args, remaining = parser.parse_known_args()
    BUILDER = str(Path(args.builder).resolve())
    unittest.main(argv=[__file__, *remaining])
