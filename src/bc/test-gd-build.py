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

    def native_inventory(self, option='ON', definition='-DGD_BC_NATIVE_SCALAR=1',
                         backend='native-scalar-x64.cc', shared=True):
        self.cache.write_text(self.cache.read_text() + 'ENABLE_BC_NATIVE_SCALAR:BOOL=' + option + '\n')
        rows = [dict(file='/source/src/bc/vm.cc', command='c++ ' + definition + ' -c vm.cc')]
        if backend:
            rows.append(dict(file='/source/src/bc/' + backend, command='c++ -c ' + backend))
        if shared:
            rows.append(dict(file='/source/src/bc/native-scalar-plan.cc', command='c++ -c native-scalar-plan.cc'))
        (self.build / 'compile_commands.json').write_text(json.dumps(rows))

    def test_native_selection_verifies_compiled_backend(self):
        self.native_inventory()
        value = self.metadata()['configuration']['native_scalar_policy']
        self.assertTrue(value['configured'])
        self.assertTrue(value['enabled'])
        self.assertEqual(value['backend'], 'native-scalar-x64.cc')
        self.module.require_native_candidate(self.helper, dict(metadata=dict(
            configuration=dict(native_scalar_policy=value))))
        self.assertEqual((self.root / 'native-policy/CMakeCache.txt').read_bytes(), self.cache.read_bytes())

    def test_native_unsupported_target_remains_generic_and_cannot_qualify(self):
        self.native_inventory(definition='', backend=None, shared=False)
        value = self.module.verified_native_scalars(self.helper, self.build)
        self.assertTrue(value['configured'])
        self.assertFalse(value['enabled'])
        self.assertIsNone(value['backend'])
        with self.assertRaises(self.helper.BuildError):
            self.module.require_native_candidate(self.helper, dict(metadata=dict(
                configuration=dict(native_scalar_policy=value))))

    def test_native_disabled_mode_is_verified_without_presence_definition(self):
        self.native_inventory(option='OFF', definition='', backend=None, shared=False)
        value = self.module.verified_native_scalars(self.helper, self.build)
        self.assertFalse(value['configured'])
        self.assertFalse(value['enabled'])

    def test_native_legacy_source_is_not_mislabeled_disabled(self):
        value = self.module.verified_native_scalars(self.helper, self.build)
        self.assertEqual(value['status'], 'legacy-auto')
        self.assertIsNone(value['enabled'])
        with self.assertRaises(self.helper.BuildError):
            self.module.require_native_candidate(self.helper, dict(metadata=dict(
                configuration=dict(native_scalar_policy=value))))

    def test_native_ignored_cache_option_cannot_qualify(self):
        self.native_inventory()
        self.cache.write_text(self.cache.read_text().replace('NATIVE_SCALAR:BOOL', 'NATIVE_SCALAR:UNINITIALIZED'))
        with self.assertRaises(self.helper.BuildError):
            self.module.verified_native_scalars(self.helper, self.build)

    def test_native_presence_definition_zero_or_override_is_rejected(self):
        self.native_inventory()
        path = self.build / 'compile_commands.json'
        original = json.loads(path.read_text())
        for definition in ('-DGD_BC_NATIVE_SCALAR=0', '-DGD_BC_NATIVE_SCALAR',
                           '-DGD_BC_NATIVE_SCALAR=1 -DGD_BC_NATIVE_SCALAR=1',
                           '-DGD_BC_NATIVE_SCALAR=1 -UGD_BC_NATIVE_SCALAR'):
            with self.subTest(definition=definition):
                rows = [dict(row) for row in original]
                rows[0]['command'] = 'c++ ' + definition + ' -c vm.cc'
                path.write_text(json.dumps(rows))
                with self.assertRaises(self.helper.BuildError):
                    self.module.verified_native_scalars(self.helper, self.build)

    def test_native_missing_or_ambiguous_backend_is_rejected(self):
        self.native_inventory()
        path = self.build / 'compile_commands.json'
        original = json.loads(path.read_text())
        for rows in (original[:1], original + [dict(file='/source/src/bc/native-scalar.cc')],
                     original + [original[0]], original[1:]):
            with self.subTest(rows=rows):
                path.write_text(json.dumps(rows))
                with self.assertRaises(self.helper.BuildError):
                    self.module.verified_native_scalars(self.helper, self.build)

    def test_native_cached_off_cannot_compile_a_backend(self):
        self.native_inventory(option='OFF')
        with self.assertRaises(self.helper.BuildError):
            self.module.verified_native_scalars(self.helper, self.build)

    def test_native_shared_plan_is_required_for_candidate(self):
        self.native_inventory(shared=False)
        value = self.module.verified_native_scalars(self.helper, self.build)
        self.assertTrue(value['enabled'])
        with self.assertRaises(self.helper.BuildError):
            self.module.require_native_candidate(self.helper, dict(metadata=dict(
                configuration=dict(native_scalar_policy=value))))

    def native_project(self, definitions='GD_BC_NATIVE_SCALAR=1', item=''):
        self.native_inventory()
        (self.build / 'compile_commands.json').unlink()
        self.cache.write_text(self.cache.read_text() + 'CMAKE_GENERATOR:INTERNAL=Visual Studio 17 2022\n')
        project = self.build / 'src/bc/libbc.vcxproj'
        project.write_text('<Project><ItemDefinitionGroup Condition="Release|x64">'
            '<ClCompile><PreprocessorDefinitions>' + definitions + '</PreprocessorDefinitions>'
            '</ClCompile></ItemDefinitionGroup><ItemGroup><ClCompile Include="vm.cc">' + item +
            '</ClCompile><ClCompile Include="native-scalar-x64.cc"/>'
            '<ClCompile Include="native-scalar-plan.cc"/></ItemGroup></Project>')
        return project

    def test_native_visual_studio_release_project_is_retained(self):
        project = self.native_project()
        value = self.metadata()['configuration']['native_scalar_policy']
        self.assertTrue(value['enabled'])
        self.assertEqual(value['verification']['kind'], 'msbuild-projects')
        self.assertEqual((self.root / 'native-policy/src/bc/libbc.vcxproj').read_bytes(), project.read_bytes())

    def test_native_visual_studio_vm_override_is_rejected(self):
        self.native_project(item='<PreprocessorDefinitions>GD_BC_NATIVE_SCALAR=0</PreprocessorDefinitions>')
        with self.assertRaises(self.helper.BuildError):
            self.module.verified_native_scalars(self.helper, self.build)

    def test_native_visual_studio_zero_presence_definition_is_rejected(self):
        self.native_project(definitions='GD_BC_NATIVE_SCALAR=0')
        with self.assertRaises(self.helper.BuildError):
            self.module.verified_native_scalars(self.helper, self.build)

    def test_native_visual_studio_missing_selection_is_rejected(self):
        self.native_project(definitions='OTHER=1')
        with self.assertRaises(self.helper.BuildError):
            self.module.verified_native_scalars(self.helper, self.build)

    def function_inventory(self, option='ON', definition='-DGD_BC_NATIVE_FUNCTIONS=1',
                           backend='native-expression-x64.cc'):
        lines = [line for line in self.cache.read_text().splitlines()
                 if not line.startswith('ENABLE_BC_NATIVE_FUNCTIONS:')]
        self.cache.write_text('\n'.join(lines) + '\nENABLE_BC_NATIVE_FUNCTIONS:BOOL=' + option + '\n')
        source = self.root / 'source/src/bc'
        source.mkdir(parents=True, exist_ok=True)
        (source / 'native-expression-plan.hh').write_text('// shared fixture planner\n')
        rows = [dict(file=str(source / 'vm.cc'), command='c++ ' + definition + ' -c vm.cc')]
        if backend:
            (source / backend).write_text('#include "native-expression-plan.hh"\n')
            rows.append(dict(file=str(source / backend), command='c++ -c ' + backend))
        (self.build / 'compile_commands.json').write_text(json.dumps(rows))
        return source

    def test_native_function_backends_bind_and_retain_shared_source(self):
        for backend in ('native-expression-x64.cc', 'native-expression-arm64.cc'):
            with self.subTest(backend=backend):
                source = self.function_inventory(backend=backend)
                value = self.metadata()['configuration']['native_function_policy']
                self.assertEqual(value['backend'], backend)
                self.assertTrue(value['configured'] and value['enabled'] and value['shared_plan'])
                self.module.require_native_function_candidate(self.helper, dict(metadata=dict(
                    configuration=dict(native_function_policy=value))))
                for item in value['source_inputs']:
                    raw = (self.root / 'source' / item['path']).read_bytes()
                    self.assertEqual(item['sha256'], hashlib.sha256(raw).hexdigest())
                    self.assertEqual((self.root / 'native-function-policy' / item['retained_path']).read_bytes(), raw)
                self.assertEqual((self.root / 'native-function-policy/CMakeCache.txt').read_bytes(),
                                 self.cache.read_bytes())

    def test_native_function_missing_shared_planner_is_rejected(self):
        source = self.function_inventory()
        header = source / 'native-expression-plan.hh'
        header.unlink()
        with self.assertRaises(self.helper.BuildError):
            self.module.verified_native_functions(self.helper, self.build, self.root / 'source')
        header.write_text('// present but not included\n')
        (source / 'native-expression-x64.cc').write_text('// no planner\n')
        with self.assertRaises(self.helper.BuildError):
            self.module.verified_native_functions(self.helper, self.build, self.root / 'source')

    def test_native_function_generic_modes_and_ignored_option_cannot_qualify(self):
        for mode in ('OFF', 'ON'):
            self.function_inventory(option=mode, definition='', backend=None)
            value = self.module.verified_native_functions(self.helper, self.build, self.root / 'source')
            self.assertFalse(value['enabled'])
            self.assertEqual(value['configured'], mode == 'ON')
            with self.assertRaises(self.helper.BuildError):
                self.module.require_native_function_candidate(self.helper, dict(metadata=dict(
                    configuration=dict(native_function_policy=value))))
        self.cache.write_text(self.cache.read_text().replace('NATIVE_FUNCTIONS:BOOL',
                                                            'NATIVE_FUNCTIONS:UNINITIALIZED'))
        with self.assertRaises(self.helper.BuildError):
            self.module.verified_native_functions(self.helper, self.build, self.root / 'source')

    def test_native_function_zero_definition_and_ambiguous_backend_are_rejected(self):
        self.function_inventory(definition='-DGD_BC_NATIVE_FUNCTIONS=0')
        with self.assertRaises(self.helper.BuildError):
            self.module.verified_native_functions(self.helper, self.build, self.root / 'source')
        self.function_inventory()
        db = self.build / 'compile_commands.json'
        rows = json.loads(db.read_text())
        rows.append(dict(file='/source/src/bc/native-expression-arm64.cc'))
        db.write_text(json.dumps(rows))
        with self.assertRaises(self.helper.BuildError):
            self.module.verified_native_functions(self.helper, self.build, self.root / 'source')

    def test_native_function_release_project_is_retained_and_overrides_reject(self):
        self.function_inventory()
        (self.build / 'compile_commands.json').unlink()
        self.cache.write_text(self.cache.read_text() + 'CMAKE_GENERATOR:INTERNAL=Visual Studio 17 2022\n')
        project = self.build / 'src/bc/libbc.vcxproj'
        text = ('<Project><ItemDefinitionGroup Condition="Release|x64"><ClCompile>'
                '<PreprocessorDefinitions>GD_BC_NATIVE_FUNCTIONS=1</PreprocessorDefinitions>'
                '</ClCompile></ItemDefinitionGroup><ItemGroup><ClCompile Include="vm.cc">'
                '</ClCompile><ClCompile Include="native-expression-x64.cc"/></ItemGroup></Project>')
        project.write_text(text)
        value = self.metadata()['configuration']['native_function_policy']
        self.assertTrue(value['enabled'])
        self.assertEqual((self.root / 'native-function-policy/src/bc/libbc.vcxproj').read_bytes(),
                         project.read_bytes())
        project.write_text(text.replace('<ClCompile Include="vm.cc">',
            '<ClCompile Include="vm.cc"><PreprocessorDefinitions>GD_BC_NATIVE_FUNCTIONS=0</PreprocessorDefinitions>'))
        with self.assertRaises(self.helper.BuildError):
            self.module.verified_native_functions(self.helper, self.build, self.root / 'source')

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

    def word_inventory(self, enabled=True, tests=False, declared=True):
        self.cache.write_text(self.cache.read_text() + (
            'ENABLE_BC_WORD_FUNCTIONS:BOOL=' + ('ON' if enabled else 'OFF') + '\n' if declared else ''))
        source = self.root / 'source/src/bc'
        source.mkdir(parents=True, exist_ok=True)
        names = ['word-function.cc', 'word-lowering.cc', 'word-runtime.cc']
        for name in names + [n.replace('.cc', '.hh') for n in names]:
            (source / name).write_text('// source identity fixture\n')
        define = ' -DGD_BC_WORD_FUNCTIONS=1' if enabled else ''
        rows = [dict(file=str(source / 'vm.cc'), command='c++' + define + ' -o CMakeFiles/libbc.dir/vm.cc.o -c vm.cc')]
        if enabled:
            rows += [dict(file=str(source / name), command='c++' + define + ' -o CMakeFiles/libbc.dir/' + name + '.o -c ' + name) for name in names]
        if tests:
            rows += [dict(file=str(source / name), command='c++ -o CMakeFiles/test-bc-word-function.dir/' + name + '.o -c ' + name) for name in names]
        (self.build / 'compile_commands.json').write_text(json.dumps(rows))
        return rows

    def test_word_production_inventory_and_test_only_fallback(self):
        for enabled in (True, False):
            with self.subTest(enabled=enabled):
                self.word_inventory(enabled, tests=not enabled)
                policy = self.module.verified_word_functions(self.helper, self.build, self.root / 'source')
                self.assertIs(policy['enabled'], enabled)
                self.assertEqual(len(policy['translation_units']), 3 if enabled else 0)
                self.assertEqual(len(policy['source_inputs']), 6 if enabled else 0)
                if enabled:
                    self.module.require_word_function_candidate(self.helper, dict(metadata=dict(configuration=dict(word_function_policy=policy))))
                else:
                    self.module.require_word_function_baseline(self.helper, dict(metadata=dict(configuration=dict(word_function_policy=policy))))

    def test_legacy_word_absence_requires_real_vm_inventory(self):
        self.word_inventory(False, declared=False)
        policy = self.module.verified_word_functions(self.helper, self.build, self.root / 'source')
        self.assertEqual(policy['status'], 'recorded')
        self.assertIsNone(policy['configured'])
        self.assertIs(policy['enabled'], False)
        for mode in ('ON', 'OFF'):
            with self.assertRaises(self.helper.BuildError):
                self.module.verified_word_functions(self.helper, self.build, self.root / 'source', mode)
        (self.build / 'compile_commands.json').write_text('[{"command":"c++ -c fixture.cc"}]')
        unknown = self.module.verified_word_functions(self.helper, self.build, self.root / 'source')
        self.assertEqual(unknown['status'], 'legacy-auto')
        with self.assertRaises(self.helper.BuildError):
            self.module.require_word_function_baseline(self.helper, dict(metadata=dict(configuration=dict(word_function_policy=unknown))))

    def test_word_ignored_option_and_corrupt_inventory_are_rejected(self):
        original = self.word_inventory()
        path = self.build / 'compile_commands.json'
        for change in ('missing-vm', 'duplicate-vm', 'missing-unit', 'duplicate-unit', 'wrong-unit',
                       'undefine', 'duplicate-define', 'unvalued', 'zero', 'long-value'):
            rows = json.loads(json.dumps(original))
            if change == 'missing-vm': rows.pop(0)
            elif change == 'duplicate-vm': rows.append(rows[0])
            elif change == 'missing-unit': rows.pop()
            elif change == 'duplicate-unit': rows.append(rows[-1])
            elif change == 'wrong-unit': rows[-1]['command'] = rows[-1]['command'].replace('=1', '=0')
            elif change == 'undefine': rows[0]['command'] += ' -UGD_BC_WORD_FUNCTIONS'
            elif change == 'duplicate-define': rows[0]['command'] += ' -DGD_BC_WORD_FUNCTIONS=1'
            else:
                value = {'unvalued':'', 'zero':'=0', 'long-value':'=10'}[change]
                rows[0]['command'] = rows[0]['command'].replace('GD_BC_WORD_FUNCTIONS=1', 'GD_BC_WORD_FUNCTIONS' + value)
            path.write_text(json.dumps(rows))
            with self.subTest(change=change), self.assertRaises(self.helper.BuildError):
                self.module.verified_word_functions(self.helper, self.build, self.root / 'source')
        path.write_text(json.dumps(original))
        for invalid in ('ON', 'garbage'):
            self.cache.write_text(self.cache.read_text().replace('ENABLE_BC_WORD_FUNCTIONS:BOOL=ON', 'ENABLE_BC_WORD_FUNCTIONS:UNINITIALIZED=' + invalid))
            with self.assertRaises(self.helper.BuildError):
                self.module.verified_word_functions(self.helper, self.build, self.root / 'source')

    def test_word_requested_mode_and_source_inputs_are_verified(self):
        self.word_inventory()
        with self.assertRaises(self.helper.BuildError):
            self.module.verified_word_functions(self.helper, self.build, self.root / 'source', 'OFF')
        with self.assertRaises(self.helper.BuildError):
            self.module.verified_word_functions(self.helper, self.build, self.root / 'source', 'ignored')
        p = self.root / 'source/src/bc/word-runtime.hh'
        p.unlink()
        with self.assertRaises(self.helper.BuildError):
            self.module.verified_word_functions(self.helper, self.build, self.root / 'source')

    def test_word_candidate_requires_all_source_inputs_and_exact_booleans(self):
        self.word_inventory()
        good = self.module.verified_word_functions(self.helper, self.build, self.root / 'source')
        invalid = [None, {}, dict(good, configured=1), dict(good, enabled=1),
                   dict(good, status='legacy-auto'), dict(good, translation_units=[]),
                   dict(good, source_inputs=[]), dict(good, source_inputs=good['source_inputs'][:-1]),
                   dict(good, source_inputs=[dict(item, sha256='invalid') for item in good['source_inputs']])]
        for policy in invalid:
            with self.subTest(policy=policy), self.assertRaises(self.helper.BuildError):
                self.module.require_word_function_candidate(self.helper, dict(metadata=dict(configuration=dict(word_function_policy=policy))))
        for entry in (None, {}, dict(metadata=None), dict(metadata=dict(configuration=None))):
            for require in (self.module.require_word_function_candidate, self.module.require_word_function_baseline):
                with self.assertRaises(self.helper.BuildError): require(self.helper, entry)

    def test_word_native_release_project_and_retention(self):
        self.word_inventory()
        (self.build / 'compile_commands.json').unlink()
        self.cache.write_text(self.cache.read_text() + 'CMAKE_GENERATOR:INTERNAL=Visual Studio 17 2022\n')
        p = self.build / 'src/bc/libbc.vcxproj'
        text = '<Project><ItemDefinitionGroup Condition="config==Release|x64"><ClCompile><PreprocessorDefinitions>WIN32;GD_BC_WORD_FUNCTIONS=1;%(PreprocessorDefinitions)</PreprocessorDefinitions></ClCompile></ItemDefinitionGroup><ItemGroup>'
        text += ''.join('<ClCompile Include="' + name + '"/>' for name in ('vm.cc','word-function.cc','word-lowering.cc','word-runtime.cc')) + '</ItemGroup></Project>'
        p.write_text(text)
        policy = self.module.verified_word_functions(self.helper, self.build, self.root / 'source')
        self.assertIs(policy['enabled'], True)
        self.assertEqual(policy['verification']['kind'], 'msbuild-projects')
        # Do not run the unrelated native-backend metadata probes in this fixture.
        with mock.patch.object(self.module, 'verified_native_functions', return_value=dict(status='legacy-auto')), mock.patch.object(self.module, 'verified_native_scalars', return_value=dict(status='legacy-auto')):
            value = self.metadata()
        self.assertEqual(value['configuration']['word_function_policy'], policy)
        self.assertEqual((self.root / 'word-function-policy/src/bc/libbc.vcxproj').read_bytes(), p.read_bytes())
        for item in policy['source_inputs']:
            self.assertEqual(hashlib.sha256((self.root / 'word-function-policy' / item['retained_path']).read_bytes()).hexdigest(), item['sha256'])
        for change in ('zero', 'override', 'missing'):
            changed = text.replace('GD_BC_WORD_FUNCTIONS=1', 'GD_BC_WORD_FUNCTIONS=0') if change == 'zero' else text.replace('<ClCompile Include="vm.cc"/>', '<ClCompile Include="vm.cc"><PreprocessorDefinitions>GD_BC_WORD_FUNCTIONS=0</PreprocessorDefinitions></ClCompile>') if change == 'override' else text.replace('<ClCompile Include="word-runtime.cc"/>','')
            p.write_text(changed)
            with self.subTest(change=change), self.assertRaises(self.helper.BuildError):
                self.module.verified_word_functions(self.helper, self.build, self.root / 'source')

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
