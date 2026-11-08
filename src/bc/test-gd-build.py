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
from unittest import mock


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

    def test_missing_profile_policy_evidence_is_not_mislabeled_disabled(self):
        policy = self.metadata()['configuration']['profiling_execution_policy']
        self.assertEqual(policy['schema'], 1)
        self.assertEqual(policy['path_identity'], 'not established')
        forwarding = policy['scalar_argument_forwarding']
        self.assertEqual(forwarding['status'], 'unavailable')
        self.assertIsNone(forwarding['enabled'])

    def profile_policy_source(self):
        source = self.root / 'source/src/bc/vm.cc'
        source.parent.mkdir(parents=True, exist_ok=True)
        source.write_text('#if defined(__APPLE__) && defined(__aarch64__) && !GD_BC_PROFILE\n'
                          '#  define GD_BC_FORWARD_SCALAR_ARGUMENT 1\n'
                          '#else\n#  define GD_BC_FORWARD_SCALAR_ARGUMENT 0\n#endif\n')
        return source

    @unittest.skipUnless(os.name == 'posix', 'GNU-style compiler probe fixture')
    def test_profile_policy_uses_the_vm_compile_context(self):
        source = self.profile_policy_source()
        compiler = self.root / 'c++'
        compiler.write_text('#!' + sys.executable + '\n'
            'from pathlib import Path\nimport os\nimport sys\n'
            'args = sys.argv[1:]\n'
            'assert "-E" in args and "-P" in args\n'
            'assert not any(key in os.environ for key in ["CXXFLAGS", "CL", "BC_PROFILE_FIXTURE"])\n'
            'assert not any(x in args for x in ["-c", "-o", "-MF", "-MMD", "-flto=thin"])\n'
            'probe = next(Path(x) for x in args if x.endswith("profile-forwarding-probe.cc"))\n'
            'assert "vm.cc" in probe.read_text()\n'
            'print("GD_BC_PROFILE_FORWARDING", 0 if "-DGD_BC_PROFILE=1" in args else 1)\n')
        compiler.chmod(0o755)
        for diagnostic in (False, True):
            with self.subTest(diagnostic=diagnostic):
                self.cache.write_text(self.cache.read_text().replace(
                    'ENABLE_BC_PROFILE:BOOL=' + ('OFF' if diagnostic else 'ON'),
                    'ENABLE_BC_PROFILE:BOOL=' + ('ON' if diagnostic else 'OFF')))
                commands = [dict(file=str(source), directory=str(self.root), arguments=[
                    str(compiler), '-DGD_BC_PROFILE=' + str(int(diagnostic)), '-O3',
                    '-flto=thin', '-MMD', '-MF', 'ignored.d', '-c', str(source), '-o', 'ignored.o'])]
                (self.build / 'compile_commands.json').write_text(json.dumps(commands))
                builder = self.helper.Builder(self.root, 3)
                with mock.patch.dict(os.environ, {'CXXFLAGS': 'fixture', 'CL': 'fixture',
                                                  'BC_PROFILE_FIXTURE': 'fixture'}):
                    value = self.module.build_metadata(
                        self.helper, self.root, self.root / 'source', self.build, 'a' * 40, 126,
                        {'version': 'C'}, {'version': 'C++'}, diagnostic_profile=diagnostic,
                        builder=builder)
                policy = value['configuration']['profiling_execution_policy']
                self.assertEqual(policy['path_identity'], 'not established')
                forwarding = policy['scalar_argument_forwarding']
                self.assertEqual(forwarding['status'], 'recorded')
                self.assertEqual(forwarding['enabled'], not diagnostic)
                self.assertEqual(forwarding['source_sha256'],
                                 hashlib.sha256(source.read_bytes()).hexdigest())
                evidence = forwarding['verification']
                self.assertEqual(evidence['kind'], 'vm-preprocessor')
                self.assertEqual(evidence['log_sha256'], hashlib.sha256(
                    (self.root / evidence['log']).read_bytes()).hexdigest())
                self.assertFalse((self.root / 'ignored.o').exists())
                self.assertFalse((self.root / 'ignored.d').exists())

    def test_unrecognized_forwarding_guard_stays_unknown(self):
        source = self.profile_policy_source()
        source.write_text(source.read_text().replace('!GD_BC_PROFILE', 'GD_BC_PROFILE'))
        policy = self.metadata()['configuration']['profiling_execution_policy']
        self.assertEqual(policy['scalar_argument_forwarding']['status'], 'unknown')
        self.assertIsNone(policy['scalar_argument_forwarding']['enabled'])

    def test_missing_vm_commands_and_probe_runner_stay_unavailable(self):
        self.profile_policy_source()
        policy = self.metadata()['configuration']['profiling_execution_policy']
        self.assertEqual(policy['scalar_argument_forwarding']['status'], 'unavailable')
        self.assertIsNone(policy['scalar_argument_forwarding']['enabled'])

    @unittest.skipUnless(os.name == 'posix', 'GNU-style compiler probe fixture')
    def test_failed_or_ambiguous_profile_probe_does_not_claim_disabled(self):
        source = self.profile_policy_source()
        compiler = self.root / 'c++'
        (self.build / 'compile_commands.json').write_text(json.dumps([dict(
            file=str(source), directory=str(self.root),
            arguments=[str(compiler), '-c', str(source), '-o', 'ignored.o'])]))
        for program in ('raise SystemExit(2)', 'print("GD_BC_PROFILE_FORWARDING 10")',
                        'print("GD_BC_PROFILE_FORWARDING 0\\nGD_BC_PROFILE_FORWARDING 1")'):
            with self.subTest(program=program):
                compiler.write_text('#!' + sys.executable + '\n' + program + '\n')
                compiler.chmod(0o755)
                value = self.module.build_metadata(
                    self.helper, self.root, self.root / 'source', self.build, 'a' * 40, 126,
                    {'version': 'C'}, {'version': 'C++'}, builder=self.helper.Builder(self.root, 3))
                policy = value['configuration']['profiling_execution_policy']
                self.assertEqual(policy['scalar_argument_forwarding']['status'], 'unavailable')
                self.assertIsNone(policy['scalar_argument_forwarding']['enabled'])

    def prepared_commands(self, value, arguments=False):
        rows = []
        for name in ('vm.cc', 'bc.cc'):
            row = {'file': '/source/src/bc/' + name}
            flags = ['c++', '-DGD_BC_PREPARED_DIVISORS=' + value, '-c', name]
            row['arguments' if arguments else 'command'] = flags if arguments else ' '.join(flags)
            rows.append(row)
        (self.build / 'compile_commands.json').write_text(json.dumps(rows))

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
