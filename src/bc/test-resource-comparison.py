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
                                   prefix_results_policy=dict(status='recorded', mode='ON', enabled=True))))
            self.ordinary[role] = tool
            self.profiles[role] = copy.deepcopy(tool)
            self.profiles[role]['metadata']['instrumented'] = True
            self.profiles[role]['metadata']['configuration']['diagnostic_profile'] = True
        self.case = dict(name='fixture', group='fixture', program='1+1\n', expected=['2'], parameters={})
        self.calls = []

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
                metadata['configuration']['prefix_results_policy']['enabled'] = False
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

    def test_representatives_cover_both_presets_and_the_unit_atan_boundary(self):
        groups = {'startup', 'loop', 'calls', 'recursion', 'array', 'array_sparse',
                  'streaming', 'file_streaming', 'temporary_chain', 'frame_reuse'}
        for preset in ('full', 'holdout'):
            cases = self.module.representatives(self.bench, preset, 'arithmetic')
            self.assertEqual({c['group'] for c in cases}, groups)
            math_cases = self.module.representatives(self.bench, preset, 'math')
            self.assertTrue(all(c['parameters']['mode'] == 'one_shot' for c in math_cases))
            self.assertTrue(any(c['parameters']['function'] == 'a' and
                                c['parameters']['scale'] == 20 for c in math_cases))
            self.assertTrue(any(c['parameters']['scale'] == 100 for c in math_cases))

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

    def test_profile_builds_match_candidate_options_and_keep_baseline_policy(self):
        args = types.SimpleNamespace(source=self.root, base='a' * 40, candidate='b' * 40,
                                     split_point=126, candidate_prefix_results=True,
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
                self.assertEqual('--prefix-results' in command, not baseline)
                self.assertEqual('--prepared-divisors' in command, not baseline)
            self.assertEqual('--preparsed-library' in commands[0], not baseline)

    def test_file_transport_uses_the_staged_identical_program(self):
        self.case['input_mode'] = 'file'
        report = self.collect()
        self.assertEqual(report['results'][0]['input_mode'], 'file')
        self.assertEqual((self.root / 'results/fixture/input.bc').read_bytes(), b'1+1\n')

    def test_collectors_run_both_resource_presets_before_elapsed_collection(self):
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

                    def run(self, argv, **keywords):
                        calls.append(argv)
                        script = Path(argv[1]).name
                        if script == 'test-math-library-cli.py':
                            return
                        if script == 'compare-bc-resources.py':
                            directory = Path(argv[argv.index('--output') + 1])
                            directory.mkdir()
                            (directory / 'manifest.json').write_text('{"fixture":true}')
                            return
                        if script not in ('build-gd-bc.py', 'build-bc-comparators.py'):
                            error = ci.CommandFailure if kind == 'math' else helper.BuildError
                            raise error('stop fixture before elapsed samples')
                        directory = Path(argv[argv.index('--output') + 1])
                        directory.mkdir()
                        (directory / 'source/src/bc').mkdir(parents=True)
                        (directory / 'source/src/bc/math-library.bc').write_text('scale=20\n')
                        (directory / 'prefix-policy').mkdir()
                        (directory / 'prefix-policy/libbc.vcxproj').write_text('fixture')
                        howard = script == 'build-bc-comparators.py'
                        name = 'howard-7.0.3' if howard else argv[argv.index('--name') + 1]
                        revision = helper.HOWARD_REVISION if howard else argv[argv.index('--revision') + 1]
                        profile = '--diagnostic-profile' in argv
                        enabled = '--prefix-results' in argv
                        entry = dict(name=name, argv=[sys.executable], metadata=dict(
                            source_revision=revision, binary_sha256=self.bench.digest_file(sys.executable),
                            compiler='fixture', flags=['fixture'], version='fixture', instrumented=profile,
                            configuration=dict(diagnostic_profile=profile, prefix_results_policy=dict(
                                status='recorded', mode='ON' if enabled else 'AUTO', enabled=enabled))))
                        (directory / 'implementations.json').write_text(json.dumps(dict(implementations=[entry])))

                # Bind only fixture hashing to the recorder, never invoke bc.
                Commands.bench = self.bench
                original_load = ci.load

                def capture(entries, directory):
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
                        '--output', str(output), '--broad', '--compare-resources', '--candidate-prefix-results']
                with mock.patch.object(ci, 'load', modules), mock.patch.object(sys, 'argv', argv), \
                        mock.patch.object(ci, 'Commands', Commands, create=True), \
                        mock.patch.object(ci, 'host_info', lambda: {}, create=True), \
                        mock.patch.object(ci, 'allocation_diagnostics', lambda *a, **k: None, create=True), \
                        contextlib.redirect_stderr(io.StringIO()):
                    self.assertEqual(ci.main(), 1)
                resource_calls = [c for c in calls if Path(c[1]).name == 'compare-bc-resources.py']
                self.assertEqual([c[c.index('--preset') + 1] for c in resource_calls], ['full', 'holdout'])
                first_timing = next(i for i, c in enumerate(calls) if Path(c[1]).name == 'bench-bc.py')
                self.assertTrue(all(calls.index(c) < first_timing for c in resource_calls))
                profiles = json.loads((output / 'evidence/resource-profiles.json').read_text())['implementations']
                ordinary = json.loads((output / 'evidence/resource-ordinary.json').read_text())['implementations']
                self.assertTrue(all(e['metadata']['instrumented'] is True for e in profiles))
                self.assertTrue(all(e['metadata']['instrumented'] is False for e in ordinary))
                self.assertEqual([e['metadata']['source_revision'] for e in profiles], ['a' * 40, 'b' * 40])
                self.assertEqual([e['argv'][1:] for e in profiles], [['-l'], ['-l']] if kind == 'math' else [[], []])


if __name__ == '__main__':
    unittest.main()
