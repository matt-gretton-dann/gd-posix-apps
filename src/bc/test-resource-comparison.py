#!/usr/bin/env python3
"""Exercise matched resource diagnostics without executing production binaries."""
import copy
import contextlib
import hashlib
import importlib.util
import io
import json
import os
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


class ResourceComparison(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.module = load('compare-bc-resources')
        self.bench = load('bench-bc')
        self.profiles, self.ordinary = {}, {}
        for role, source in [('base', 'a' * 40), ('candidate', 'b' * 40)]:
            tool = dict(name=role, argv=[sys.executable], metadata=dict(
                source_revision=source, binary_sha256=self.bench.digest_file(sys.executable),
                instrumented=False, compiler='Python fixture', flags=['fixture'], version='fixture',
                configuration=dict(diagnostic_profile=False, multiply_split_point=126,
                                   portable_scalar_policy=dict(status='recorded', mode='ON', enabled=True))))
            self.ordinary[role] = tool
            self.profiles[role] = copy.deepcopy(tool)
            self.profiles[role]['metadata']['instrumented'] = True
            self.profiles[role]['metadata']['configuration']['diagnostic_profile'] = True
        self.case = dict(name='fixture', group='fixture', program='1+1\n', expected=['2'], parameters={})
        self.calls = []

    def test_matching_absent_optional_backend_policy_is_supported(self):
        for roles in (self.profiles, self.ordinary):
            for tool in roles.values():
                del tool['metadata']['configuration']['portable_scalar_policy']
        self.module.validate_roles(self.bench, self.profiles, self.ordinary)

    def test_private_frame_cases_fix_widths_depth_orders_and_exact_sums(self):
        self.assertRaises(ValueError,self.module.private_frame_cases,'invalid')
        for preset,depth,rounds in [('full',24,16),('holdout',21,14)]:
            cases=self.module.private_frame_cases(preset)
            self.assertEqual([c['parameters']['order'] for c in cases],['ascending','descending'])
            for case in cases:
                self.assertEqual(case['parameters']['widths'],[1,13,26])
                self.assertEqual(case['parameters']['depth'],depth)
                self.assertEqual(case['expected'],[str(3*rounds*depth)])
                self.assertNotIn('[]',case['program'])
                self.assertIn('define f(x)',case['program'])
                self.assertIn('define h(x)',case['program'])

    def test_distinct_profiling_execution_policies_remain_explicit(self):
        for roles, enabled in ((self.profiles, False), (self.ordinary, True)):
            for tool in roles.values():
                tool['metadata']['configuration']['profiling_execution_policy'] = dict(
                    schema=1, path_identity='not established',
                    scalar_argument_forwarding=dict(status='recorded', enabled=enabled))
        report = self.collect()
        self.assertFalse(report['merge_eligible'])
        self.assertIn('execution-path identity not established', report['scope'])
        for role in ('base', 'candidate'):
            for key, enabled in (('profiles', False), ('ordinary', True)):
                policy = report[key][role]['metadata']['configuration']['profiling_execution_policy']
                self.assertIs(policy['scalar_argument_forwarding']['enabled'], enabled)
                self.assertEqual(policy['path_identity'], 'not established')

    def test_one_sided_optional_backend_policy_is_rejected(self):
        del self.profiles['candidate']['metadata']['configuration']['portable_scalar_policy']
        with self.assertRaisesRegex(ValueError, 'resource modes differ'):
            self.module.validate_roles(self.bench, self.profiles, self.ordinary)

    def test_native_profile_must_match_its_ordinary_mode(self):
        for roles in (self.profiles, self.ordinary):
            for name, tool in roles.items():
                tool['metadata']['configuration']['native_scalar_policy'] = dict(
                    status='recorded', mode='ON' if name=='candidate' else 'OFF',
                    enabled=name=='candidate', configured=name=='candidate', shared_plan=name=='candidate',
                    backend='native-scalar-x64.cc' if name=='candidate' else None)
        self.module.validate_roles(self.bench, self.profiles, self.ordinary)
        self.profiles['candidate']['metadata']['configuration']['native_scalar_policy'] = dict(
            status='recorded', mode='OFF', enabled=False, configured=False, shared_plan=False, backend=None)
        with self.assertRaisesRegex(ValueError, 'native_scalar_policy'):
            self.module.validate_roles(self.bench, self.profiles, self.ordinary)

    def test_instrumented_images_are_retained_separately_with_exact_recipes(self):
        for role, tool in self.profiles.items():
            binary = self.root / (role + '.fixture')
            binary.write_bytes(role.encode('ascii'))
            tool['argv'] = [str(binary)]
            tool['metadata']['binary_sha256'] = self.bench.digest_file(binary)
        destination = self.root / 'profile-images'
        self.module.preserve_profiles(self.profiles, destination)
        report = json.loads((destination / 'manifest.json').read_text())
        self.assertIn('never elapsed timing', report['scope'])
        self.assertEqual(len(report['observations']), 2)
        for row in report['observations']:
            self.assertEqual(row['implementation'], self.profiles[row['role']])
            raw = (destination / row['binary']).read_bytes()
            self.assertEqual(raw, row['role'].encode('ascii'))
            self.assertEqual(row['binary_sha256'], hashlib.sha256(raw).hexdigest())
            self.assertEqual(row['file_bytes'], len(raw))

    def test_changed_profile_image_is_not_archived_as_matching(self):
        self.profiles['base']['metadata']['binary_sha256'] = '0' * 64
        destination = self.root / 'profile-images'
        with self.assertRaisesRegex(ValueError, 'binary changed'):
            self.module.preserve_profiles(self.profiles, destination)
        self.assertEqual(json.loads((destination / 'manifest.json').read_text())['observations'], [])

    def test_uninstrumented_profile_is_not_mislabeled(self):
        self.profiles['base']['metadata']['instrumented'] = False
        with self.assertRaisesRegex(ValueError, 'declare instrumentation'):
            self.module.preserve_profiles(self.profiles, self.root / 'profile-images')

    def observed_profile(self, tool, case, timeout, program_path=None):
        if case.get('input_mode') == 'file':
            self.assertIsNotNone(program_path)
            self.assertEqual(program_path.read_bytes(), case['program'].encode('utf-8'))
        role = tool['name']
        self.calls.append(('profile', role, case['program']))
        total = dict(allocation_calls=2, deallocation_calls=2,
                     allocated_bytes=100 if role == 'base' else 200, live_bytes=0,
                     peak_live_bytes=80 if role == 'base' else 160)
        report = dict(instrumented=True, schema=1, total=total,
                      allocations=dict(results=total), events=dict(result_slot_initializations=
                          12 if role == 'base' else 1))
        Path(os.environ['GD_BC_PROFILE_OUTPUT']).write_text(json.dumps(report))
        return dict(checked=True, seconds=9.9, output_sha256=hashlib.sha256(b'2\n').hexdigest())

    def observed_rss(self, bench, tool, case, directory, timeout):
        role = tool['name']
        self.calls.append(('rss', role, case['program']))
        return dict(status='collected', checked=True, native_unit='fixture bytes',
                    peak_rss_native=100 if role == 'base' else 120)

    def collect(self, rss=None):
        with mock.patch.object(self.bench, 'run_checked', self.observed_profile):
            return self.module.collect(self.bench, types.SimpleNamespace(
                rss_profile=rss or self.observed_rss), self.profiles, self.ordinary,
                [self.case], self.root / 'results', repetitions=5, seed=657197, timeout=10)

    def test_profile_and_ordinary_require_the_same_object_boundary(self):
        ordinary = self.ordinary['candidate']['metadata']['configuration']
        profile = self.profiles['candidate']['metadata']['configuration']
        boundary = dict(kind='opaque-object', target='bc-portable-scalar', ipo=False,
                        configuration='Release', linked_object_count=2)
        ordinary['portable_scalar_policy']['boundary'] = copy.deepcopy(boundary)
        profile['portable_scalar_policy']['boundary'] = copy.deepcopy(boundary)
        self.module.validate_roles(self.bench, self.profiles, self.ordinary)
        for key, value in [('ipo', True), ('linked_object_count', 0), ('kind', 'disabled-object')]:
            profile['portable_scalar_policy']['boundary'] = dict(boundary, **{key: value})
            with self.subTest(key=key), self.assertRaises(ValueError):
                self.module.validate_roles(self.bench, self.profiles, self.ordinary)
        profile['portable_scalar_policy'].pop('boundary')
        with self.assertRaises(ValueError):
            self.module.validate_roles(self.bench, self.profiles, self.ordinary)

    def test_same_inputs_preserve_losing_resources_without_elapsed_claims(self):
        report = self.collect()
        self.assertFalse(report['merge_eligible'])
        self.assertEqual(len([c for c in self.calls if c[0] == 'profile']), 2)
        self.assertEqual(len([c for c in self.calls if c[0] == 'rss']), 10)
        self.assertEqual({c[2] for c in self.calls}, {'1+1\n'})
        row = report['results'][0]
        self.assertEqual(row['allocation_comparison']['total']['allocated_bytes']['candidate_over_base'], 2)
        self.assertEqual(row['allocation_comparison']['events']['result_slot_initializations']['delta'], -11)
        self.assertEqual(row['rss_comparison']['paired_ratios'], [1.2] * 5)
        self.assertEqual(row['rss_comparison']['complete_pairs'], 5)
        self.assertNotIn('seconds', json.loads((self.root / 'results/fixture/base-profile.json').read_text()))
        self.assertNotIn('"seconds"', json.dumps(report))
        orders = [r['order'] for r in row['rss_observations']]
        self.assertTrue(all(a == b[::-1] for a, b in zip(orders, orders[1:])))

    def test_instrumentation_source_mode_and_binary_identity_must_match(self):
        original = copy.deepcopy(self.profiles)
        for change in ('instrumented', 'source', 'mode', 'hash', 'profile-config', 'compiler', 'flags', 'arguments'):
            self.profiles = copy.deepcopy(original)
            metadata = self.profiles['candidate']['metadata']
            if change == 'instrumented':
                metadata['instrumented'] = 1
            elif change == 'source':
                metadata['source_revision'] = 'c' * 40
            elif change == 'mode':
                metadata['configuration']['portable_scalar_policy']['enabled'] = False
            elif change == 'hash':
                metadata['binary_sha256'] = '0' * 64
            elif change == 'profile-config':
                metadata['configuration']['diagnostic_profile'] = False
            elif change == 'compiler':
                metadata['compiler'] = 'different compiler'
            elif change == 'flags':
                metadata['flags'] = []
            else:
                self.profiles['candidate']['argv'].append('-l')
            with self.subTest(change=change), self.assertRaises(ValueError):
                self.module.validate_roles(self.bench, self.profiles, self.ordinary)
        self.profiles = original
        self.ordinary['candidate']['metadata']['instrumented'] = True
        with self.assertRaises(ValueError):
            self.module.validate_roles(self.bench, self.profiles, self.ordinary)

    def test_failed_counter_report_restores_environment_and_keeps_evidence(self):
        original = self.observed_profile

        def leaking(tool, case, timeout, **keywords):
            observed = original(tool, case, timeout, **keywords)
            path = Path(os.environ['GD_BC_PROFILE_OUTPUT'])
            report = json.loads(path.read_text())
            report['total']['live_bytes'] = 1
            path.write_text(json.dumps(report))
            return observed

        self.observed_profile = leaking
        with mock.patch.dict(os.environ, {'GD_BC_PROFILE_OUTPUT': 'previous-report'}):
            with self.assertRaises(ValueError):
                self.collect()
            self.assertEqual(os.environ['GD_BC_PROFILE_OUTPUT'], 'previous-report')
        self.assertTrue((self.root / 'results/fixture/base-counters.json').is_file())
        manifest = json.loads((self.root / 'results/manifest.json').read_text())
        self.assertEqual(manifest['status'], 'incomplete')
        self.assertFalse(manifest['merge_eligible'])

    def test_unavailable_or_mismatched_rss_is_not_a_complete_pair(self):
        def unavailable(bench, tool, case, directory, timeout):
            if tool['name'] == 'candidate':
                return dict(status='unavailable', checked=False, reason='fixture adapter unavailable')
            return self.observed_rss(bench, tool, case, directory, timeout)

        report = self.collect(unavailable)
        row = report['results'][0]['rss_comparison']
        self.assertEqual(row['complete_pairs'], 0)
        self.assertEqual(row['status'], 'inconclusive')
        self.assertEqual(len(report['results'][0]['rss_observations']), 5)
        observations = [dict(roles=dict(base=dict(status='collected', checked=True,
                             peak_rss_native=100, native_unit='KiB'), candidate=dict(
                             status='collected', checked=True, peak_rss_native=100, native_unit='bytes')))] * 5
        self.assertEqual(self.module.rss_comparison(observations, 5)['complete_pairs'], 0)

    def test_representatives_cover_both_presets_and_low_scale_math_boundaries(self):
        groups = {'startup', 'loop', 'calls', 'recursion', 'array', 'array_sparse',
                  'streaming', 'file_streaming', 'temporary_chain', 'frame_reuse', 'private_frame_reuse'}
        for preset in ('full', 'holdout'):
            cases = self.module.representatives(self.bench, preset, 'arithmetic')
            self.assertEqual({c['group'] for c in cases}, groups)
            math_cases = self.module.representatives(self.bench, preset, 'math')
            self.assertTrue(all(c['parameters']['mode'] == 'one_shot' for c in math_cases))
            self.assertTrue(any(c['parameters']['function'] == 'a' and
                                c['parameters']['scale'] == 20 for c in math_cases))
            self.assertTrue(any(c['parameters']['scale'] == 100 for c in math_cases))
            self.assertEqual(len(math_cases), 16)
            self.assertEqual({c['group'] for c in math_cases if
                              c['parameters']['function'] == 'j' and
                              c['parameters']['scale'] == 20},
                             {'math_j_regular', 'math_j_turning', 'math_j_order_dominates',
                              'math_j_argument_dominates', 'math_j_near_root'})

    def test_frame_cases_pair_early_return_with_dense_reuse_and_distinct_holdout(self):
        names, programs = set(), set()
        for preset in ('full', 'holdout'):
            cases = self.module.frame_cases(preset)
            self.assertEqual({c['parameters']['path'] for c in cases}, {'early_return', 'dense'})
            for case in cases:
                parameters = case['parameters']
                tails, rounds = parameters['tail_additions'], parameters['rounds']
                self.assertEqual(len(tails), 8)
                self.assertEqual(tails, sorted(set(tails)))
                self.assertGreater(rounds, 1)
                result = (len(tails) if parameters['path'] == 'early_return' else sum(tails)) * 2 * rounds
                self.assertEqual(case['expected'], [str(result), '7'])
                self.assertEqual(case['group'], 'frame_reuse')
                self.assertIn('a[]', case['program'])
                names.add(case['name'])
                programs.add(case['program'])
        self.assertEqual(len(names), 4)
        self.assertEqual(len(programs), 4)

    def test_malformed_counters_fail_and_new_counters_remain_missing_in_old_sources(self):
        total = dict(allocation_calls=0, deallocation_calls=0, allocated_bytes=0,
                     live_bytes=0, peak_live_bytes=0)
        good = dict(schema=1, instrumented=True, total=total,
                    allocations=dict(results=total), events=dict(old=0))
        for change in ('schema', 'categories', 'bool', 'negative', 'events'):
            report = copy.deepcopy(good)
            if change == 'schema':
                report['schema'] = True
            elif change == 'categories':
                report['allocations'] = None
            elif change == 'bool':
                report['total']['allocated_bytes'] = True
            elif change == 'negative':
                report['events']['old'] = -1
            else:
                report['events'] = []
            with self.subTest(change=change), self.assertRaises(ValueError):
                self.module.checked_counters(report)
        candidate = copy.deepcopy(good)
        candidate['events']['new'] = 7
        report = self.module.allocation_comparison(good, candidate)
        self.assertEqual(report['events']['new']['status'], 'missing counter')
        self.assertIsNone(report['events']['new']['base'])
        self.assertNotIn('candidate_over_base', report['events']['new'])
        self.assertIsNone(report['total']['allocated_bytes']['candidate_over_base'])

    def test_math_resource_configs_use_the_same_library_arguments(self):
        calls = []
        self.module.run_diagnostics(lambda argv: calls.append(argv), self.profiles,
                                    self.ordinary, ('full',), 'math', self.root)
        profiles = json.loads((self.root / 'resource-profiles.json').read_text())['implementations']
        ordinary = json.loads((self.root / 'resource-ordinary.json').read_text())['implementations']
        for role, profile, release in zip(('base', 'candidate'), profiles, ordinary):
            self.assertEqual(profile['argv'][1:], ['-l'])
            self.assertEqual(release['argv'][1:], ['-l'])
        self.module.validate_roles(self.bench, {e['name']: e for e in profiles},
                                   {e['name']: e for e in ordinary})
        self.assertEqual(len(calls), 1)

    def test_profile_builds_match_candidate_options_and_keep_baseline_policy(self):
        args = types.SimpleNamespace(source=self.root, base='a' * 40, candidate='b' * 40,
                                     split_point=126,
                                     candidate_prepared_divisors=True, candidate_preparsed_library=True)
        math = load('benchmark-math-ci')
        arithmetic = load('benchmark-ci')
        for baseline in (True, False):
            role = 'base' if baseline else 'candidate'
            revision = args.base if baseline else args.candidate
            commands = [math.profile_build_command(args, self.root / role, 'cc', 'c++', baseline=baseline),
                        arithmetic.profile_build_command(args, role, revision, self.root / role, 'cc', 'c++')]
            for command in commands:
                self.assertIn('--diagnostic-profile', command)
                self.assertEqual(command[command.index('--revision') + 1], revision)
                self.assertEqual('--prepared-divisors' in command, not baseline)
            self.assertEqual('--preparsed-library' in commands[0], not baseline)

    def test_file_transport_uses_the_staged_identical_program(self):
        self.case['input_mode'] = 'file'
        report = self.collect()
        self.assertEqual(report['results'][0]['input_mode'], 'file')
        self.assertEqual((self.root / 'results/fixture/input.bc').read_bytes(), b'1+1\n')

    def test_collectors_build_before_and_collect_resources_after_all_elapsed_stages(self):
        for kind in ('arithmetic', 'math'):
            with self.subTest(kind=kind), tempfile.TemporaryDirectory() as temporary:
                source = Path(temporary)
                (source / 'src/bc').mkdir(parents=True)
                (source / 'src/bc/math-library.bc').write_text('scale=20\n')
                ci = load('benchmark-math-ci' if kind == 'math' else 'benchmark-ci')
                helper, calls = ci.load('build-bc-comparators'), []

                class Commands:
                    def __init__(self, root, timeout):
                        self.commands = calls
                        self.deadline = 12345.0

                    def run(self, argv, **keywords):
                        calls.append(argv)
                        script = Path(argv[1]).name
                        if script in ('test-math-library-cli.py', 'test-divisor-reuse-cli.py'):
                            return
                        if script == 'compare-bc-resources.py':
                            directory = Path(argv[argv.index('--output') + 1])
                            directory.mkdir()
                            (directory / 'manifest.json').write_text('{"fixture":true}')
                            return
                        if script == 'bench-bc.py':
                            directory = Path(argv[argv.index('--output') + 1])
                            directory.mkdir()
                            (directory / 'summary.json').write_text('{"workloads":[]}')
                            (directory / 'samples.jsonl').write_text('')
                            return
                        if script not in ('build-gd-bc.py', 'build-bc-comparators.py'):
                            error = ci.CommandFailure if kind == 'math' else helper.BuildError
                            raise error('stop fixture before elapsed samples')
                        directory = Path(argv[argv.index('--output') + 1])
                        directory.mkdir()
                        (directory / 'source/src/bc').mkdir(parents=True)
                        (directory / 'source/src/bc/math-library.bc').write_text('scale=20\n')
                        (directory / 'portable-scalar-policy').mkdir()
                        (directory / 'portable-scalar-policy/libbc.vcxproj').write_text('fixture')
                        howard = script == 'build-bc-comparators.py'
                        name = 'howard-7.0.3' if howard else argv[argv.index('--name') + 1]
                        revision = helper.HOWARD_REVISION if howard else argv[argv.index('--revision') + 1]
                        profile = '--diagnostic-profile' in argv
                        mode = argv[argv.index('--portable-scalar') + 1] if '--portable-scalar' in argv else 'AUTO'
                        enabled = mode == 'ON'
                        entry = dict(name=name, argv=[sys.executable], metadata=dict(
                            source_revision=revision, binary_sha256=self.bench.digest_file(sys.executable),
                            compiler='fixture', flags=['fixture'], version='fixture', instrumented=profile,
                            configuration=dict(diagnostic_profile=profile, portable_scalar_policy=dict(
                                status='recorded', mode=mode, enabled=enabled))))
                        (directory / 'implementations.json').write_text(json.dumps(dict(implementations=[entry])))

                # Bind only fixture hashing to the recorder, never invoke bc.
                Commands.bench = self.bench
                original_load = ci.load

                archived = []

                def capture(entries, directory, **keywords):
                    archived.extend(entries)
                    self.assertTrue(all(e['metadata']['instrumented'] is False for e in entries))
                    directory.mkdir()

                def modules(name):
                    if name == 'benchmark-ci':
                        return types.SimpleNamespace(host_info=lambda: {})
                    if kind == 'arithmetic' and name == 'build-bc-comparators':
                        return types.SimpleNamespace(Builder=Commands, BuildError=helper.BuildError,
                                                     HOWARD_REVISION=helper.HOWARD_REVISION, digest=helper.digest)
                    if name == 'collect-bc-codegen':
                        return types.SimpleNamespace(collect=capture)
                    return original_load(name)

                output = source / 'results'
                argv = ['fixture', '--source', str(source), '--base', 'a' * 40, '--candidate', 'b' * 40,
                        '--output', str(output), '--broad', '--compare-resources']
                with mock.patch.object(ci, 'load', modules), mock.patch.object(sys, 'argv', argv), \
                        mock.patch.object(ci, 'Commands', Commands, create=True), \
                        mock.patch.object(ci, 'host_info', lambda: {}, create=True), \
                        mock.patch.object(ci, 'allocation_diagnostics', lambda *a, **k: None, create=True), \
                        mock.patch.object(ci, 'resource_diagnostics', lambda *a, **k: None, create=True), \
                        mock.patch.object(ci, 'measurements', lambda *a, **k: [], create=True), \
                        contextlib.redirect_stderr(io.StringIO()):
                    self.assertEqual(ci.main(), 0)
                resource_calls = [c for c in calls if Path(c[1]).name == 'compare-bc-resources.py']
                self.assertEqual([c[c.index('--preset') + 1] for c in resource_calls], ['full', 'holdout'])
                timings = [i for i, c in enumerate(calls) if Path(c[1]).name == 'bench-bc.py']
                self.assertTrue(timings)
                self.assertTrue(all(calls.index(c) > max(timings) for c in resource_calls))
                builds = [i for i, c in enumerate(calls) if Path(c[1]).name.startswith('build-')]
                self.assertTrue(all(i < min(timings) for i in builds))
                names = {e['name'] for e in archived}
                self.assertTrue({'gd-base', 'gd-portable', 'gd-aa', 'howard-standard', 'howard-portable'} <= names
                                if kind == 'math' else {'base', 'candidate', 'aa', 'howard-7.0.3'} <= names)
                profiles = json.loads((output / 'evidence/resource-profiles.json').read_text())['implementations']
                ordinary = json.loads((output / 'evidence/resource-ordinary.json').read_text())['implementations']
                self.assertTrue(all(e['metadata']['instrumented'] is True for e in profiles))
                self.assertTrue(all(e['metadata']['instrumented'] is False for e in ordinary))
                self.assertEqual([e['metadata']['source_revision'] for e in profiles], ['a' * 40, 'b' * 40])
                self.assertEqual([e['argv'][1:] for e in profiles], [['-l'], ['-l']] if kind == 'math' else [[], []])
                retained = json.loads((output / 'evidence/profile-images/manifest.json').read_text())
                self.assertEqual({r['role'] for r in retained['observations']}, {'base', 'candidate'})


if __name__ == '__main__':
    unittest.main()
