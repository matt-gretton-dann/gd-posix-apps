#!/usr/bin/env python3
"""Reject ignored prefix activation and preserve independent collector roles."""
import importlib.util
import contextlib
import io
import json
from pathlib import Path
import sys
import tempfile
import types
import unittest
from unittest import mock


SCRIPTS = Path(__file__).resolve().parent / 'scripts'


def load(name):
    spec = importlib.util.spec_from_file_location(name, SCRIPTS / (name + '.py'))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class PrefixBuild(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.build = Path(temporary.name)
        self.gd = load('build-gd-bc')
        self.helper = self.gd.helpers()
        self.cache = self.build / 'CMakeCache.txt'
        self.cache.write_text('CMAKE_GENERATOR:INTERNAL=Ninja\n')
        self.commands = self.build / 'compile_commands.json'

    def configure(self, enabled=True):
        self.cache.write_text('CMAKE_GENERATOR:INTERNAL=Ninja\n'
                             'ENABLE_BC_PREFIX_RESULTS:BOOL=' + ('ON' if enabled else 'OFF') + '\n')
        self.rows = [dict(file='/source/src/bc/vm.cc', arguments=[
            'c++', '-DGD_BC_PREFIX_RESULTS=' + ('1' if enabled else '0'), '-c', 'vm.cc'])]
        self.commands.write_text(json.dumps(self.rows))

    def policy(self, mode='ON'):
        return self.gd.verified_prefix_results(self.helper, self.build, mode)

    def test_legacy_auto_cannot_certify_explicit_mode(self):
        policy = self.policy('AUTO')
        self.assertEqual(policy['status'], 'legacy-auto')
        self.assertIsNone(policy['enabled'])
        for mode in ('ON', 'OFF', 'invalid'):
            with self.subTest(mode=mode), self.assertRaises(self.helper.BuildError):
                self.policy(mode)

    def test_explicit_modes_require_the_compiled_vm_and_reject_opposite_request(self):
        for enabled in (True, False):
            self.configure(enabled)
            policy = self.policy('ON' if enabled else 'OFF')
            self.assertIs(policy['enabled'], enabled)
            self.assertEqual(policy['verification']['translation_units'], 1)
            self.assertEqual(policy['verification']['sha256'], self.helper.digest(self.commands))
            with self.assertRaises(self.helper.BuildError):
                self.policy('OFF' if enabled else 'ON')

    def test_missing_ignored_undefined_and_contradictory_flags_fail_closed(self):
        for flags in ([], ['-DGD_BC_PREFIX_RESULTS=0'], ['-DGD_BC_PREFIX_RESULTS=10'],
                      ['-DGD_BC_PREFIX_RESULTS'], ['-DGD_BC_PREFIX_RESULTS_EXTRA=1'],
                      ['-DGD_BC_PREFIX_RESULTS=1', '-DGD_BC_PREFIX_RESULTS=0'],
                      ['-DGD_BC_PREFIX_RESULTS=1', '-UGD_BC_PREFIX_RESULTS']):
            self.configure()
            self.rows[0]['arguments'] = ['c++'] + flags + ['-c', 'vm.cc']
            self.commands.write_text(json.dumps(self.rows))
            with self.subTest(flags=flags), self.assertRaises(self.helper.BuildError):
                self.policy()
        for content in ('[]', '{}', '[null]', 'not json'):
            self.commands.write_text(content)
            with self.subTest(inventory=content), self.assertRaises(self.helper.BuildError):
                self.policy()

    def test_native_release_projects_supply_actual_mode_evidence(self):
        self.configure()
        self.commands.unlink()
        self.cache.write_text('CMAKE_GENERATOR:INTERNAL=Visual Studio 18 2026\n'
                             'ENABLE_BC_PREFIX_RESULTS:BOOL=ON\n')
        project = self.build / 'src/bc/libbc.vcxproj'
        project.parent.mkdir(parents=True)
        project.write_text('''<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
          <ItemDefinitionGroup Condition="'$(Configuration)|$(Platform)'=='Release|x64'">
            <ClCompile><PreprocessorDefinitions>GD_BC_PREFIX_RESULTS=1;%(PreprocessorDefinitions)</PreprocessorDefinitions></ClCompile>
          </ItemDefinitionGroup></Project>''')
        policy = self.policy()
        self.assertEqual(policy['verification']['kind'], 'msbuild-projects')
        self.assertEqual(policy['verification']['files']['src/bc/libbc.vcxproj'],
                         self.helper.digest(project))
        original = project.read_text()
        for changed in (original.replace('Release|x64', 'Debug|x64'),
                        original.replace('RESULTS=1', 'RESULTS=0'),
                        original.replace('</ClCompile>', '<UndefinePreprocessorDefinitions>GD_BC_PREFIX_RESULTS</UndefinePreprocessorDefinitions></ClCompile>')):
            project.write_text(changed)
            with self.subTest(project=changed), self.assertRaises(self.helper.BuildError):
                self.policy()

    def test_candidate_cannot_accept_an_auto_or_unverified_activation(self):
        policy = dict(status='recorded', mode='ON', enabled=True)
        entry = lambda value: dict(metadata=dict(configuration=dict(prefix_results_policy=value)))
        self.gd.require_prefix_candidate(self.helper, entry(policy))
        for value in (None, {}, dict(policy, mode='AUTO'), dict(policy, status='legacy-auto'),
                      dict(policy, mode='OFF'), dict(policy, enabled=False), dict(policy, enabled=1)):
            with self.subTest(policy=value), self.assertRaises(self.helper.BuildError):
                self.gd.require_prefix_candidate(self.helper, entry(value))

    def test_arithmetic_keeps_baseline_aa_and_enables_only_candidate(self):
        ci = load('benchmark-ci')
        args = types.SimpleNamespace(source=self.build, split_point=126,
                                     candidate_prefix_results=True)
        for name in ('base', 'candidate', 'aa'):
            command = ci.gd_build_command(args, name, 'a' * 40, self.build / name, 'cc', 'c++')
            self.assertEqual('--prefix-results' in command, name == 'candidate')
            if name == 'candidate':
                self.assertEqual(command[command.index('--prefix-results') + 1], 'ON')

    def test_math_candidate_aa_and_profile_select_the_same_mode(self):
        ci = load('benchmark-math-ci')
        args = types.SimpleNamespace(source=self.build, base='a' * 40, candidate='b' * 40,
                                     candidate_prefix_results=True)
        for name in ('gd', 'aa', 'base'):
            command = ci.gd_build_command(args, name, self.build / name, 'cc', 'c++')
            self.assertEqual('--prefix-results' in command, name != 'base')
        command = ci.profile_build_command(args, self.build / 'profile', 'cc', 'c++')
        self.assertIn('--diagnostic-profile', command)
        self.assertEqual(command[command.index('--prefix-results') + 1], 'ON')
        self.assertIn(args.candidate, command)

    def test_collectors_reject_unverified_candidate_before_any_timing(self):
        for kind in ('arithmetic', 'math'):
            for valid in (True, False):
                with self.subTest(kind=kind, valid=valid), tempfile.TemporaryDirectory() as temporary:
                    source = Path(temporary)
                    (source / 'src/bc').mkdir(parents=True)
                    (source / 'src/bc/math-library.bc').write_text('scale=20\n')
                    ci = load('benchmark-math-ci' if kind == 'math' else 'benchmark-ci')
                    helper = ci.load('build-bc-comparators')
                    calls = []

                    class Commands:
                        def __init__(self, root, timeout):
                            self.commands = calls

                        def run(self, argv, **unused):
                            calls.append(argv)
                            if Path(argv[1]).name != 'build-gd-bc.py':
                                error = ci.CommandFailure if kind == 'math' else helper.BuildError
                                raise error('stop fixture before timings')
                            destination = Path(argv[argv.index('--output') + 1])
                            destination.mkdir()
                            (destination / 'prefix-policy').mkdir()
                            (destination / 'prefix-policy/libbc.vcxproj').write_text('retained fixture')
                            entry = dict(name=argv[argv.index('--name') + 1], argv=['fixture'],
                                         metadata=dict(configuration=dict(prefix_results_policy=dict(
                                             status='recorded', mode='ON', enabled=valid))))
                            (destination / 'implementations.json').write_text(json.dumps(
                                dict(implementations=[entry])))

                    original_load = ci.load

                    def modules(name):
                        if name == 'benchmark-ci':
                            return types.SimpleNamespace(host_info=lambda: {})
                        if kind == 'arithmetic' and name == 'build-bc-comparators':
                            return types.SimpleNamespace(Builder=Commands, BuildError=helper.BuildError)
                        if name == 'collect-bc-codegen':
                            return types.SimpleNamespace(collect=lambda entries, directory: None)
                        return original_load(name)

                    output = source / 'results'
                    argv = ['fixture', '--source', str(source), '--base', 'a' * 40,
                            '--candidate', 'b' * 40, '--output', str(output), '--candidate-prefix-results']
                    with mock.patch.object(ci, 'load', modules), mock.patch.object(sys, 'argv', argv), \
                            mock.patch.object(ci, 'Commands', Commands, create=True), \
                            mock.patch.object(ci, 'host_info', lambda: {}, create=True), \
                            contextlib.redirect_stderr(io.StringIO()):
                        self.assertEqual(ci.main(), 1)
                    self.assertEqual(len(calls), 4 if valid else (1 if kind == 'math' else 2))
                    error = json.loads((output / 'evidence/failure.json').read_text())['error']
                    self.assertIn('stop fixture' if valid else 'verified explicit', error)
                    role = 'gd' if kind == 'math' else 'candidate'
                    self.assertEqual((output / ('evidence/' + role + '/prefix-policy/libbc.vcxproj')).read_text(),
                                     'retained fixture')
                    record = json.loads((output / 'evidence/orchestration.json').read_text())
                    self.assertIs(record['candidate_prefix_results'], True)


if __name__ == '__main__':
    unittest.main()
