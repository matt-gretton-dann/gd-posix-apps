#!/usr/bin/env python3
"""Exercise advisory decisions, independent builds and bounded confirmation."""
import contextlib
import hashlib
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
    def test_invalid_additional_replay_fails_before_output_and_builds(self):
        with tempfile.TemporaryDirectory() as tmp:
            output=Path(tmp)/'results'
            argv=[str(PATH),'--source',tmp,'--base','a'*40,'--candidate','b'*40,
                  '--output',str(output),'--additional-replay-corpus',str(Path(tmp)/'missing')]
            with mock.patch.object(sys,'argv',argv), contextlib.redirect_stderr(io.StringIO()):
                with self.assertRaises(SystemExit) as error:
                    CI.main()
            self.assertEqual(error.exception.code,2)
            self.assertFalse(output.exists())

    def test_sparse_duration_requires_every_independent_role(self):
        row = dict(implementations={role: dict(median_seconds=.25)
                                    for role in ('base', 'aa', 'candidate')})
        self.assertTrue(CI.sparse_duration_sufficient(row))
        for role in row['implementations']:
            row['implementations'][role]['median_seconds'] = .249
            self.assertFalse(CI.sparse_duration_sufficient(row), role)
            row['implementations'][role]['median_seconds'] = .25

    def test_sparse_supplement_checks_outputs_and_rejects_short_samples(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            evidence = root / 'evidence'
            evidence.mkdir()
            fixture = root / 'sparse-fixture.py'
            fixture.write_text(
                'import re,sys\n'
                'if hasattr(sys,"set_int_max_str_digits"): sys.set_int_max_str_digits(1000000)\n'
                'source=sys.stdin.read()\n'
                'assert re.search(r"i<(1000|875|1000000|875000);",source)\n'
                'a=int(re.search(r"^a=(\\d+)$",source,re.M)[1])\n'
                'b=int(re.search(r"^b=(\\d+)$",source,re.M)[1])\n'
                'print(a*b)\n')
            binary = Path(sys.executable).resolve()
            metadata = dict(source_revision='test fixture', compiler='Python fixture',
                            flags=['fixture'], version=sys.version, instrumented=False,
                            binary_sha256=hashlib.sha256(binary.read_bytes()).hexdigest())
            entries = [dict(name=role, argv=[str(binary), str(fixture)], metadata=metadata,
                            input_artifacts=[dict(path=str(fixture),
                                sha256=hashlib.sha256(fixture.read_bytes()).hexdigest(),
                                purpose='checked sparse fixture')])
                       for role in ('base', 'candidate', 'aa')]
            builder = CI.load('build-bc-comparators').Builder(root, 30)
            reports = CI.qualify_sparse(builder, evidence, entries)
            self.assertEqual(len(reports), 8)
            self.assertTrue(all(row['status'] == 'inconclusive-duration-shortfall'
                                for row in reports if row['preset'].endswith('-long')))
            for preset, count in (('full', 1000000), ('holdout', 875000)):
                target = evidence / (preset + '-sparse-long')
                manifest = json.loads((target / 'manifest.json').read_text())
                self.assertEqual(manifest['sparse_iteration_override'], count)
                self.assertEqual(manifest['samples'], 31)
                samples = [json.loads(line) for line in (target / 'samples.jsonl').read_text().splitlines()]
                self.assertTrue(all(row['checked'] for row in samples))
                self.assertEqual(sum(row['phase'] == 'measurement' for row in samples), 93)
                latency = evidence / (preset + '-sparse-latency')
                manifest = json.loads((latency / 'manifest.json').read_text())
                self.assertNotIn('sparse_iteration_override', manifest)
                self.assertEqual(manifest['samples'], 101)
                self.assertEqual(manifest['corpus'][0]['parameters']['iterations'], count // 1000)
                samples = [json.loads(line) for line in (latency / 'samples.jsonl').read_text().splitlines()]
                self.assertTrue(all(row['checked'] for row in samples))
                self.assertEqual(sum(row['phase'] == 'measurement' for row in samples), 303)

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

    def test_aa_interval_contract_is_symmetric_and_inclusive(self):
        for interval, expected in (([.95, 1.05], 'pass'), ([.98, 1.02], 'pass'),
                                   ([.937, 1.017], 'inconclusive-noisy'),
                                   ([.98, 1.08], 'inconclusive-noisy'),
                                   ([.90, .92], 'inconclusive-noisy'),
                                   ([1.08, 1.1], 'inconclusive-noisy')):
            with self.subTest(interval=interval):
                self.assertEqual(CI.aa_decision(dict(ci95=interval)), expected)
        for interval in (None, [], [1], [float('nan'), 1.1], [.9, float('inf')],
                         [1.1, .9], [0, 1], [True, 1.01]):
            with self.subTest(invalid=interval):
                ratio = dict(ci95=interval)
                self.assertEqual(CI.aa_decision(ratio), 'inconclusive-noisy')
                self.assertFalse(CI.needs_confirmation(ratio, 'aa'))
        for interval in ([.90, .92], [1.08, 1.1]):
            ratio = dict(ci95=interval)
            self.assertTrue(CI.needs_confirmation(ratio, 'aa'))
            self.assertEqual(CI.aa_decision(ratio, ratio), 'confirmed A/A bias')
            self.assertEqual(CI.aa_decision(ratio, dict(ci95=[.98, 1.02])), 'inconclusive-noisy')
            self.assertFalse(CI.needs_confirmation(ratio, 'aa', startup=True))
            self.assertEqual(CI.aa_decision(ratio, ratio, startup=True), 'inconclusive-noisy')

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
        self.assertIn('temporary_chain_1000d_x50000', subset)
        self.assertIn('file_statements_20000', subset)
        for name in ('sqrt_scale5000_x5', 'sqrt_large_scale10000_x5',
                     'sqrt_tiny_scale1024_x30', 'sqrt_tiny_scale4096_x1',
                     'sqrt_integer_power2000_scale100_x10'):
            self.assertIn(name, subset)
        for position in ('high', 'low', 'equal'):
            self.assertIn('compare_{}_1000d_x100000'.format(position), subset)
        self.assertIn('divide_multilimb_1000d_900d_s1000_x500', subset)
        self.assertIn('divide_multilimb_1125d_1012d_s1125_x438',
                      CI.selected_cases(bench, 'holdout', False))
        self.assertIn('divide_scaled_dense_s1000_t1000_x5000', subset)
        self.assertIn('divide_scaled_trailing_zero_s1000_t1000_x5000', subset)
        self.assertLess(len(subset), len(full))
        self.assertEqual(len(full), 66)
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

    def exercise(self, fail=False, aa_first=None, aa_second=None, prepared=False,
                 identical=False, replay=False, shortfall=False):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        root = Path(temporary.name) / 'results'
        calls = []
        helper = CI.load('build-bc-comparators')
        original_load = CI.load
        declared = Path(temporary.name)/'declared'
        if replay:
            (declared/'workloads').mkdir(parents=True)
            program, oracle = b'1+1\n', b'2\n'
            (declared/'workloads/integer_fixture.bc').write_bytes(program)
            (declared/'workloads/integer_fixture.expected').write_bytes(oracle)
            (declared/'manifest.json').write_text(json.dumps(dict(suite='arithmetic',corpus=[dict(
                name='integer_fixture',group='calls',parameters=dict(mode='repeated',iterations=2,
                duration_target_seconds=.25),program_sha256=hashlib.sha256(program).hexdigest(),
                oracle_sha256=hashlib.sha256(oracle).hexdigest())])))

        class Builder:
            def __init__(self, directory, timeout):
                self.commands = calls
                self.timeout = timeout

            def run(self, argv):
                calls.append(argv)
                script = Path(argv[1]).name
                if script == 'test-divisor-reuse-cli.py':
                    return
                output = Path(argv[argv.index('--output') + 1])
                output.mkdir()
                if script in ('build-gd-bc.py', 'build-bc-comparators.py'):
                    (output / 'prepared-policy').mkdir()
                    (output / 'prepared-policy/libbc.vcxproj').write_text('retained fixture')
                    name = argv[argv.index('--name') + 1] if '--name' in argv else 'howard-7.0.3'
                    (output / 'implementations.json').write_text(json.dumps(
                        {'implementations': [{'name': name, 'argv': ['fixture'], 'metadata': dict(
                            binary_sha256=('b' if name == 'candidate' and not identical else 'a') * 64,
                            configuration=dict(prepared_divisors_policy=dict(
                                status='recorded', mode='ON', enabled=True)))}]}))
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
                    # Synthetic summaries exercise controller decisions, not timing inference.
                    if candidate == 'aa' and aa_first is not None:
                        ratio = aa_second if output.name.endswith('confirmation') and aa_second is not None else aa_first
                    result={'name': name, 'candidate_over_reference': {candidate: ratio}}
                    if '--replay-corpus' in argv:
                        result.update(parameters=dict(mode='repeated',iterations=2,
                                      duration_target_seconds=.25),
                                      duration_target_met={role: not shortfall for role in implementations})
                    results.append(result)
                    for round_number in range(9):
                        for implementation in implementations:
                            raw.append({'phase': 'measurement', 'workload': name,
                                        'round': round_number, 'implementation': implementation,
                                        'seconds': 1.2 if slow and implementation == candidate else 1.0})
                (output / 'summary.json').write_text(json.dumps({'workloads': results}))
                (output / 'samples.jsonl').write_text('\n'.join(json.dumps(row) for row in raw))

        def capture(entries, directory):
            directory.mkdir()
            (directory / 'manifest.json').write_text(json.dumps(dict(
                names=[entry['name'] for entry in entries],
                commands_before_capture=len(calls))))

        def modules(name):
            if name == 'collect-bc-codegen':
                return types.SimpleNamespace(collect=capture)
            if name == 'build-bc-comparators':
                return types.SimpleNamespace(Builder=Builder, BuildError=helper.BuildError)
            return original_load(name)

        argv = [str(PATH), '--source', str(Path(temporary.name)), '--base', 'a' * 40,
                '--candidate', 'b' * 40, '--output', str(root)]
        if prepared:
            argv += ['--candidate-prepared-divisors']
        if replay:
            argv += ['--additional-replay-corpus',str(declared)]
        with mock.patch.object(CI, 'load', modules), mock.patch.object(CI, 'host_info', return_value={}), \
                mock.patch.object(sys, 'argv', argv), mock.patch.dict('os.environ', {}, clear=True), \
                contextlib.redirect_stderr(io.StringIO()):
            result = CI.main()
        return root, calls, result

    def test_fixed_validation_keeps_replay_identity_and_sampling_controls(self):
        root,calls,result=self.exercise(replay=True)
        self.assertEqual(result,0)
        replay=[call for call in calls if '--replay-corpus' in call]
        self.assertEqual(len(replay),3)  # A/A, paired and prescribed regression confirmation.
        for command in replay:
            self.assertEqual(command[command.index('--replay-corpus')+1],str((root.parent/'declared').resolve()))
            self.assertEqual(command[command.index('--samples')+1],'9')
            self.assertNotIn('--calibrate-seconds',command)
        report=json.loads((root/'evidence/orchestration.json').read_text())
        self.assertEqual(report['additional_replay_manifest_sha256'],hashlib.sha256(
            (root.parent/'declared/manifest.json').read_bytes()).hexdigest())

    def test_validation_duration_shortfall_remains_inconclusive(self):
        root,_,result=self.exercise(replay=True,shortfall=True)
        self.assertEqual(result,0)
        rows=json.loads((root/'evidence/decisions.json').read_text())['results']
        fresh=[row for row in rows if row['preset']=='integer-validation']
        self.assertEqual(len(fresh),2)
        self.assertTrue(all(row['status']=='inconclusive duration shortfall' for row in fresh))
        reviews=json.loads((root/'evidence/acceptance.json').read_text())['results']
        paired=[row for row in reviews if row['preset']=='integer-validation' and row['comparison']=='paired']
        self.assertEqual(len(paired),1)
        self.assertEqual(paired[0]['acceptance'],'inconclusive')

    def test_prepared_candidate_keeps_controls_ordinary_and_guards_before_timing(self):
        root, calls, result = self.exercise(prepared=True)
        self.assertEqual(result, 0)
        self.assertEqual((root / 'evidence/candidate/prepared-policy/libbc.vcxproj').read_text(),
                         'retained fixture')
        for index, call in enumerate(calls[:3]):
            self.assertEqual('--prepared-divisors' in call, index == 1)
        self.assertEqual(calls[1][calls[1].index('--prepared-divisors') + 1], 'ON')
        self.assertEqual([Path(call[1]).name for call in calls[4:7]],
                         ['test-divisor-reuse-cli.py'] * 3)
        self.assertTrue(all(Path(call[1]).name == 'bench-bc.py' for call in calls[7:]))
        ordinary, calls, result = self.exercise()
        self.assertEqual(result, 0)
        self.assertFalse(any('--prepared-divisors' in call for call in calls))
        self.assertFalse(any(Path(call[1]).name == 'test-divisor-reuse-cli.py' for call in calls))

    def test_slow_candidate_is_reported_without_failing_performance_gate(self):
        root, calls, result = self.exercise()
        self.assertEqual(result, 0)
        capture = json.loads((root / 'evidence/codegen/manifest.json').read_text())
        self.assertEqual(capture['names'], ['base', 'candidate', 'aa', 'howard-7.0.3'])
        self.assertEqual(capture['commands_before_capture'], len(calls))
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

    def test_identical_candidate_retains_slow_samples_without_source_attribution(self):
        root, _, result = self.exercise(identical=True)
        self.assertEqual(result, 0)
        report = json.loads((root / 'evidence/decisions.json').read_text())
        paired = [row for row in report['results']
                  if row['comparison'] == 'paired' and row['name'] != 'startup']
        self.assertTrue(paired)
        for row in paired:
            self.assertEqual(row['status'], 'confirmed timing bias (identical binaries)')
            self.assertEqual(row['ratio']['median_paired_ratio'], 1.2)
            self.assertEqual(row['confirmation']['median_paired_ratio'], 1.2)
            self.assertEqual(row['aa_control_status'], 'pass')

    def test_aa_bias_in_either_direction_gets_one_distinct_confirmation(self):
        for ratio in (dict(median_paired_ratio=.91, ci95=[.90, .92]),
                      dict(median_paired_ratio=1.09, ci95=[1.08, 1.10])):
            with self.subTest(ratio=ratio):
                root, calls, result = self.exercise(aa_first=ratio)
                self.assertEqual(result, 0)
                confirmations = [call for call in calls if '--output' in call and
                                 Path(call[call.index('--output') + 1]).name == 'full-aa-confirmation']
                self.assertEqual(len(confirmations), 1)
                self.assertEqual(confirmations[0][confirmations[0].index('--seed') + 1], '197217')
                rows = json.loads((root / 'evidence/decisions.json').read_text())['results']
                controls = [row for row in rows if row['comparison'] == 'aa' and row['name'] != 'startup']
                self.assertTrue(all(row['status'] == 'confirmed A/A bias' for row in controls))
                paired = [row for row in rows if row['comparison'] == 'paired' and row['name'] != 'startup']
                self.assertTrue(all(row['aa_control_status'] == 'confirmed A/A bias' for row in paired))
                # Control qualification does not erase the independently reported raw A/B signal.
                self.assertTrue(all(row['status'] == 'confirmed regression' for row in paired))

    def test_aa_interval_crossing_the_lower_band_is_inconclusive_without_retry(self):
        root, calls, result = self.exercise(aa_first=dict(median_paired_ratio=.986, ci95=[.937, 1.017]))
        self.assertEqual(result, 0)
        rows = json.loads((root / 'evidence/decisions.json').read_text())['results']
        controls = [row for row in rows if row['comparison'] == 'aa' and row['name'] != 'startup']
        self.assertTrue(all(row['status'] == 'inconclusive-noisy' for row in controls))
        self.assertFalse(any('--output' in call and Path(call[call.index('--output') + 1]).name ==
                             'full-aa-confirmation' for call in calls))

    def test_aa_confirmation_reversing_bias_does_not_pass(self):
        root, calls, result = self.exercise(aa_first=dict(median_paired_ratio=.91, ci95=[.9, .92]),
                                           aa_second=dict(median_paired_ratio=1.09, ci95=[1.08, 1.1]))
        self.assertEqual(result, 0)
        rows = json.loads((root / 'evidence/decisions.json').read_text())['results']
        controls = [row for row in rows if row['comparison'] == 'aa' and row['name'] != 'startup']
        self.assertTrue(all(row['status'] == 'inconclusive-noisy' for row in controls))
        startup = [row for row in rows if row['name'] == 'startup']
        self.assertTrue(all(row['status'] == 'inconclusive-noisy' for row in startup))

    def test_failed_checked_run_remains_a_hard_failure_with_evidence(self):
        root, calls, result = self.exercise(fail=True)
        self.assertEqual(result, 1)
        self.assertIn('incorrect output', (root / 'evidence/failure.json').read_text())
        self.assertTrue((root / 'evidence/orchestration.json').is_file())
        self.assertFalse((root / 'evidence/decisions.json').exists())
        capture = json.loads((root / 'evidence/codegen/manifest.json').read_text())
        self.assertEqual(capture['names'], ['base', 'candidate', 'aa', 'howard-7.0.3'])
        self.assertEqual(capture['commands_before_capture'], len(calls))


if __name__ == '__main__':
    unittest.main()
