#!/usr/bin/env python3
"""Exercise advisory decisions, independent builds and bounded confirmation."""
import contextlib
import importlib.util
import io
import json
from pathlib import Path
import sys
import subprocess
import tempfile
import types
import unittest
from unittest import mock

PATH = Path(__file__).resolve().parent / 'scripts/benchmark-ci.py'
SPEC = importlib.util.spec_from_file_location('benchmark_ci', str(PATH))
CI = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CI)


class NativeTrends(unittest.TestCase):
    def test_advisory_decisions_need_bounded_confirmation(self):
        slow = {'ci95': [1.15, 1.25]}
        self.assertEqual(CI.decision(slow), 'inconclusive-noisy')
        self.assertEqual(CI.decision(slow, slow), 'confirmed regression')
        self.assertEqual(CI.decision(slow, {'ci95': [0.9, 1.3]}), 'inconclusive-noisy')
        self.assertEqual(CI.decision({'ci95': [0.98, 1.03]}), 'pass')
        self.assertEqual(CI.decision({'ci95': [0.9, 1.1]}), 'inconclusive-noisy')
        self.assertEqual(CI.decision({'ci95': [1.05, 1.1]}, slow), 'inconclusive-noisy')
        self.assertEqual(CI.decision(slow, slow, startup=True), 'inconclusive-noisy')
        self.assertEqual(CI.decision({}), 'inconclusive-noisy')

    def test_checked_runner_detects_an_intentionally_slow_fixture(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            script = root / 'fixture.py'
            script.write_text('import sys,time\ntime.sleep(float(sys.argv[1]))\nsys.stdin.read()\nprint(2)\n')
            config = root / 'config.json'
            config.write_text(json.dumps({'implementations': [
                {'name': 'base', 'argv': [sys.executable, str(script), '0']},
                {'name': 'slow', 'argv': [sys.executable, str(script), '0.15']}]}))
            result = subprocess.run([sys.executable, str(CI.SCRIPTS / 'bench-bc.py'),
                                     '--config', str(config), '--output', str(root / 'results'),
                                     '--workload', 'startup', '--samples', '9', '--warmups', '1'],
                                    stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                    universal_newlines=True, timeout=15)
            self.assertEqual(result.returncode, 0, result.stderr)
            report = json.loads((root / 'results/summary.json').read_text())
            ratio = report['workloads'][0]['candidate_over_reference']['slow']
            self.assertGreater(ratio['ci95'][0], 1.05)
            # This is a deliberately delayed process fixture, not a calibrated
            # production startup gate or evidence about hosted VM false alarms.
            self.assertEqual(CI.decision(ratio, ratio), 'confirmed regression')

    def test_subset_retains_real_dimensions_and_broad_adds_holdouts(self):
        bench = CI.load('bench-bc')
        subset = CI.selected_cases(bench, 'full', False)
        full = CI.selected_cases(bench, 'full', True)
        held = CI.selected_cases(bench, 'holdout', True)
        self.assertIn('loop_1000000', subset)
        self.assertIn('array_200000', subset)
        self.assertIn('sparse_array_200000', subset)
        self.assertIn('multiply_100d_x100000', subset)
        self.assertIn('square_100d_x100000', subset)
        self.assertIn('square_10000d_x500', subset)
        self.assertIn('literal_repeat_2000d_x2000', subset)
        self.assertIn('streaming_statements_20000', subset)
        self.assertIn('binary_output_1000d_x300', subset)
        self.assertIn('octal_output_1000d_x300', subset)
        self.assertIn('binary_input_100000d_x3', subset)
        self.assertIn('hex_input_100000d_x3', subset)
        for position in ('high', 'low', 'equal'):
            self.assertIn('compare_{}_1000d_x100000'.format(position), subset)
        self.assertLess(len(subset), len(full))
        self.assertEqual(len(full), 44)
        self.assertEqual(set(full) & set(held), {'startup'})

    def test_pr_subset_covers_every_corpus_group(self):
        bench = CI.load('bench-bc')
        for preset in ('full', 'holdout'):
            with self.subTest(preset=preset):
                cases = bench.corpus(preset, listing=True,
                                     seed=197201 if preset == 'holdout' else 197200)
                selected = set(CI.selected_cases(bench, preset, False))
                self.assertEqual({case['group'] for case in cases
                                  if case['name'] in selected},
                                 {case['group'] for case in cases})

    def exercise(self, fail=False):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        root = Path(temporary.name) / 'results'
        calls = []
        helper = CI.load('build-bc-comparators')
        original_load = CI.load

        class Builder:
            def __init__(self, directory, timeout):
                self.commands = calls
                self.timeout = timeout

            def run(self, argv):
                calls.append(argv)
                script = Path(argv[1]).name
                output = Path(argv[argv.index('--output') + 1])
                output.mkdir()
                if script in ('build-gd-bc.py', 'build-bc-comparators.py'):
                    name = argv[argv.index('--name') + 1] if '--name' in argv else 'howard-7.0.3'
                    (output / 'implementations.json').write_text(json.dumps(
                        {'implementations': [{'name': name, 'argv': ['fixture']}]}))
                    return
                if fail:
                    raise helper.BuildError('fixture incorrect output')
                config = json.loads(Path(argv[argv.index('--config') + 1]).read_text())
                implementations = [entry['name'] for entry in config['implementations']]
                candidate = 'candidate' if 'candidate' in implementations else 'aa'
                names = [argv[index + 1] for index, value in enumerate(argv) if value == '--workload']
                results, raw = [], []
                for name in names:
                    slow = candidate == 'candidate' and name != 'startup'
                    ratio = {'median_paired_ratio': 1.2 if slow else 1.0,
                             'ci95': [1.15, 1.25] if slow else [0.99, 1.01]}
                    results.append({'name': name, 'candidate_over_reference': {candidate: ratio}})
                    for round_number in range(9):
                        for implementation in implementations:
                            raw.append({'phase': 'measurement', 'workload': name,
                                        'round': round_number, 'implementation': implementation,
                                        'seconds': 1.2 if slow and implementation == candidate else 1.0})
                (output / 'summary.json').write_text(json.dumps({'workloads': results}))
                (output / 'samples.jsonl').write_text('\n'.join(json.dumps(row) for row in raw))

        def modules(name):
            if name == 'build-bc-comparators':
                return types.SimpleNamespace(Builder=Builder, BuildError=helper.BuildError)
            return original_load(name)

        argv = [str(PATH), '--source', str(Path(temporary.name)), '--base', 'a' * 40,
                '--candidate', 'b' * 40, '--output', str(root)]
        with mock.patch.object(CI, 'load', modules), mock.patch.object(CI, 'host_info', return_value={}), \
                mock.patch.object(sys, 'argv', argv), mock.patch.dict('os.environ', {}, clear=True), \
                contextlib.redirect_stderr(io.StringIO()):
            result = CI.main()
        return root, calls, result

    def test_slow_candidate_is_reported_without_failing_performance_gate(self):
        root, calls, result = self.exercise()
        self.assertEqual(result, 0)
        self.assertEqual([Path(call[1]).name for call in calls[:4]],
                         ['build-gd-bc.py'] * 3 + ['build-bc-comparators.py'])
        self.assertEqual(calls[0][calls[0].index('--revision') + 1],
                         calls[2][calls[2].index('--revision') + 1])
        benchmarks = [call for call in calls if Path(call[1]).name == 'bench-bc.py']
        self.assertEqual(len(benchmarks), 3)  # A/A, paired, exactly one confirmation.
        report = json.loads((root / 'evidence/decisions.json').read_text())
        self.assertTrue(report['advisory'])
        candidate = [row for row in report['results'] if row['comparison'] == 'paired']
        self.assertTrue(any(row['status'] == 'confirmed regression' for row in candidate))
        aa = [row for row in report['results'] if row['comparison'] == 'aa' and row['name'] != 'startup']
        self.assertTrue(all(row['status'] == 'pass' for row in aa))
        self.assertTrue((root / 'evidence/summary.md').is_file())

    def test_failed_checked_run_remains_a_hard_failure_with_evidence(self):
        root, calls, result = self.exercise(fail=True)
        self.assertEqual(result, 1)
        self.assertIn('incorrect output', (root / 'evidence/failure.json').read_text())
        self.assertTrue((root / 'evidence/orchestration.json').is_file())
        self.assertFalse((root / 'evidence/decisions.json').exists())


if __name__ == '__main__':
    unittest.main()
