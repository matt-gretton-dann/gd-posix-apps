#!/usr/bin/env python3
"""Exercise matched resource diagnostics without executing production binaries."""
import copy
import hashlib
import importlib.util
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

    def observed_profile(self, tool, case, timeout):
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
        self.assertNotIn('seconds', json.dumps(report))

    def test_instrumentation_source_mode_and_binary_identity_must_match(self):
        original = copy.deepcopy(self.profiles)
        for change in ('instrumented', 'source', 'mode', 'hash', 'profile-config'):
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
            else:
                metadata['configuration']['diagnostic_profile'] = False
            with self.subTest(change=change), self.assertRaises(ValueError):
                self.module.validate_roles(self.bench, self.profiles, self.ordinary)
        self.profiles = original
        self.ordinary['candidate']['metadata']['instrumented'] = True
        with self.assertRaises(ValueError):
            self.module.validate_roles(self.bench, self.profiles, self.ordinary)

    def test_failed_counter_report_restores_environment_and_keeps_evidence(self):
        original = self.observed_profile

        def leaking(tool, case, timeout):
            observed = original(tool, case, timeout)
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

    def test_representatives_cover_both_presets_and_the_unit_atan_boundary(self):
        groups = {'startup', 'loop', 'calls', 'recursion', 'array', 'array_sparse',
                  'streaming', 'file_streaming', 'temporary_chain'}
        for preset in ('full', 'holdout'):
            cases = self.module.representatives(self.bench, preset, 'arithmetic')
            self.assertEqual({c['group'] for c in cases}, groups)
            math_cases = self.module.representatives(self.bench, preset, 'math')
            self.assertTrue(all(c['parameters']['mode'] == 'one_shot' for c in math_cases))
            self.assertTrue(any(c['parameters']['function'] == 'a' and
                                c['parameters']['scale'] == 20 for c in math_cases))
            self.assertTrue(any(c['parameters']['scale'] == 100 for c in math_cases))


if __name__ == '__main__':
    unittest.main()
