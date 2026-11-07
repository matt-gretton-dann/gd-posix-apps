#!/usr/bin/env python3
"""Check native math roles, acceptance limits and process-tree deadlines."""
import contextlib
import copy
import io
import importlib.util
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import types
import unittest
from unittest import mock

PATH = Path(__file__).resolve().parent / 'scripts/benchmark-math-ci.py'
SPEC = importlib.util.spec_from_file_location('benchmark_math_ci', str(PATH))
CI = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CI)


class NativeMath(unittest.TestCase):
    def test_measurements_use_remaining_total_budget_without_a_hidden_hour_limit(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            child = mock.MagicMock()
            child.communicate.return_value = (b'checked fixture\n', None)
            child.returncode = 0
            with mock.patch.object(CI.time, 'monotonic', return_value=0) as clock, \
                    mock.patch.object(CI.subprocess, 'Popen') as popen:
                popen.return_value.__enter__.return_value = child
                commands = CI.Commands(root, 18000)
                commands.run(['fixture', 'paired'])
                self.assertEqual(child.communicate.call_args[1]['timeout'], 18000)
                commands.run(['fixture', 'build'], timeout=1200)
                self.assertEqual(child.communicate.call_args[1]['timeout'], 1200)
                clock.return_value = 17990
                commands.run(['fixture', 'confirmation'])
                self.assertEqual(child.communicate.call_args[1]['timeout'], 10)
                commands.run(['fixture', 'build'], timeout=1200)
                self.assertEqual(child.communicate.call_args[1]['timeout'], 10)
            self.assertEqual([row['timeout_seconds'] for row in commands.commands],
                             [18000, 1200, 10, 10])
            self.assertTrue(all((root / row['log']).read_bytes() == b'checked fixture\n'
                                for row in commands.commands))

    def test_preparsed_image_mode_and_control_identity(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            args = types.SimpleNamespace(source=root, candidate='b' * 40, base='a' * 40,
                                         candidate_preparsed_library=True)
            for role in ('gd', 'aa', 'base'):
                command = CI.gd_build_command(args, role, root / role, 'cc', 'c++')
                self.assertEqual('--preparsed-library' in command, role != 'base')
            library = root / 'library.bc'
            library.write_bytes(b'scale=20\n')
            gd = dict(argv=['gd'], metadata=dict(source_revision=args.candidate,
                configuration=dict(preparsed_library_image_sha256='c' * 64)))
            aa = copy.deepcopy(gd)
            howard = dict(argv=['howard'], metadata=dict(
                source_revision=CI.load('build-bc-comparators').HOWARD_REVISION))
            config = CI.configurations(gd, aa, howard, library, library, args.candidate)
            for entry in config['aa']:
                self.assertEqual(entry['metadata']['library']['kind'], 'preparsed image')
                self.assertEqual(entry['metadata']['library']['image_sha256'], 'c' * 64)
            aa['metadata']['configuration'].clear()
            with self.assertRaisesRegex(CI.CommandFailure, 'identical preparsed images'):
                CI.configurations(gd, aa, howard, library, library, args.candidate)

    def test_prepared_mode_is_verified_for_both_independent_candidate_builds(self):
        for valid in (True, False):
            with self.subTest(valid=valid), tempfile.TemporaryDirectory() as temporary:
                source = Path(temporary)
                (source / 'src/bc').mkdir(parents=True)
                (source / 'src/bc/math-library.bc').write_text('scale=20\n')
                output = source / 'results'
                calls = []

                class Commands:
                    def __init__(self, root, timeout):
                        self.commands = calls

                    def run(self, argv, timeout=3600):
                        calls.append(argv)
                        if Path(argv[1]).name != 'build-gd-bc.py':
                            raise CI.CommandFailure('stop fixture before timings')
                        target = Path(argv[argv.index('--output') + 1])
                        target.mkdir()
                        (target / 'prepared-policy').mkdir()
                        (target / 'prepared-policy/libbc.vcxproj').write_text('retained fixture')
                        policy = dict(status='recorded', mode='ON', enabled=valid)
                        entry = dict(argv=['fixture'], metadata=dict(configuration=dict(
                            prepared_divisors_policy=policy)))
                        (target / 'implementations.json').write_text(json.dumps(
                            dict(implementations=[entry])))

                argv = [str(PATH), '--source', str(source), '--candidate', 'b' * 40,
                        '--base', 'a' * 40, '--output', str(output), '--candidate-prepared-divisors']
                original_load = CI.load
                def modules(name):
                    # Exercise orchestration, not native PowerShell/CIM probes.
                    # The production controller still records the real host.
                    if name == 'benchmark-ci':
                        return types.SimpleNamespace(host_info=lambda: {})
                    return original_load(name)
                with mock.patch.object(CI, 'Commands', Commands), \
                        mock.patch.object(CI, 'load', modules), \
                        mock.patch.object(sys, 'argv', argv), contextlib.redirect_stderr(io.StringIO()):
                    self.assertEqual(CI.main(), 1)
                self.assertEqual((output / 'evidence/gd/prepared-policy/libbc.vcxproj').read_text(),
                                 'retained fixture')
                self.assertEqual(len(calls), 4 if valid else 1)
                for call in calls[:2 if valid else 1]:
                    self.assertEqual(call[call.index('--prepared-divisors') + 1], 'ON')
                    self.assertEqual(call[call.index('--revision') + 1], 'b' * 40)
                if valid:
                    self.assertNotIn('--prepared-divisors', calls[2])
                    self.assertEqual(calls[2][calls[2].index('--revision') + 1], 'a' * 40)
                failure = json.loads((output / 'evidence/failure.json').read_text())['error']
                self.assertIn('stop fixture' if valid else 'verified explicit', failure)

    def test_every_regime_and_mode_has_a_prespecified_representative(self):
        bench = CI.load('bench-bc')
        for preset in ('full', 'holdout'):
            selected = set(CI.selected_cases(bench, preset, False))
            rows = [row for row in bench.corpus(preset, suite='math') if row['name'] in selected]
            regular = [row for row in rows if row['parameters']['scale'] == 100]
            self.assertEqual(len(regular), 20)
            self.assertEqual(len({(row['group'], row['parameters']['mode']) for row in regular}), 20)
            extra = [row['parameters'] for row in rows if row['parameters']['scale'] != 100]
            self.assertEqual([(p['function'], p['mode'], p['scale']) for p in extra],
                             [('a', 'repeated', 20), ('l', 'repeated', 20), ('l', 'repeated', 1000)])
            self.assertEqual(len(CI.selected_cases(bench, preset, True)), 60)

    def test_roles_pin_both_host_and_library_without_mutating_recipes(self):
        with tempfile.TemporaryDirectory() as temporary:
            library = Path(temporary) / 'library.bc'
            other = Path(temporary) / 'other.bc'
            library.write_bytes(b'scale=20\n');other.write_bytes(library.read_bytes())
            revision = 'a' * 40
            gd = dict(name='gd', argv=['gd-binary'], metadata=dict(source_revision=revision))
            aa = copy.deepcopy(gd)
            howard = dict(name='howard', argv=['howard-binary'], metadata=dict(
                source_revision=CI.load('build-bc-comparators').HOWARD_REVISION))
            original = copy.deepcopy((gd, aa, howard))
            configs = CI.configurations(gd, aa, howard, library, other, revision)
            self.assertEqual((gd, aa, howard), original)
            self.assertEqual([len(configs[key]) for key in ('preparation', 'paired', 'aa')], [4, 3, 2])
            standard, candidate, portable = configs['paired']
            self.assertEqual(standard['argv'][-1], '-l')
            self.assertEqual(candidate['argv'][-1], '-l')
            self.assertEqual(portable['argv'], ['howard-binary', str(library)])
            self.assertEqual(portable['input_artifacts'][0]['sha256'],
                             candidate['metadata']['library']['source_sha256'])
            other.write_bytes(b'scale=19\n')
            with self.assertRaisesRegex(CI.CommandFailure, 'identical'):
                CI.configurations(gd, aa, howard, library, other, revision)
            other.write_bytes(library.read_bytes())
            howard['metadata']['source_revision'] = 'b' * 40
            with self.assertRaisesRegex(CI.CommandFailure, 'pinned'):
                CI.configurations(gd, aa, howard, library, other, revision)

    def test_separate_baseline_identity_and_regression_roles(self):
        with tempfile.TemporaryDirectory() as temporary:
            library = Path(temporary) / 'candidate.bc'
            baseline = Path(temporary) / 'base.bc'
            library.write_bytes(b'candidate ordinary source\n')
            baseline.write_bytes(b'baseline ordinary source\n')
            revision, old_revision = 'a' * 40, 'b' * 40
            gd = dict(name='gd', argv=['gd-binary'], metadata=dict(source_revision=revision))
            old = dict(name='old', argv=['old-binary'], metadata=dict(
                source_revision=old_revision, configuration=dict(preparsed_library_image_sha256='c' * 64)))
            howard = dict(name='howard', argv=['howard-binary'], metadata=dict(
                source_revision=CI.load('build-bc-comparators').HOWARD_REVISION))
            original = copy.deepcopy(old)
            configs = CI.configurations(gd, gd, howard, library, library, revision,
                                         base=old, base_library=baseline, base_revision=old_revision)
            self.assertEqual(old, original)
            self.assertEqual([len(configs[k]) for k in ('preparation', 'paired', 'aa', 'regression')],
                             [5, 4, 2, 2])
            base, candidate = configs['regression']
            self.assertEqual(base['name'], 'gd-base')
            self.assertEqual(base['argv'], ['old-binary', '-l'])
            self.assertEqual(base['metadata']['library']['kind'], 'preparsed image')
            self.assertEqual(base['metadata']['library']['image_sha256'], 'c' * 64)
            self.assertEqual(candidate['metadata']['library']['kind'], 'embedded ordinary source')
            self.assertNotEqual(base['metadata']['library']['source_sha256'],
                                candidate['metadata']['library']['source_sha256'])
            with self.assertRaisesRegex(CI.CommandFailure, 'baseline'):
                CI.configurations(gd, gd, howard, library, library, revision,
                                  base=old, base_library=baseline, base_revision=revision)

    def test_regressions_require_confirmation_and_usable_controls(self):
        slow = dict(ci95=[1.1, 1.2])
        fast = dict(ci95=[.8, .9])
        duration = dict(base=True, candidate=True)
        classify = lambda first, second, control='pass', mode='repeated', target=duration: \
            CI.regression_status(first, second, control, mode, target)
        self.assertEqual(classify(slow, None), 'inconclusive-noisy')
        self.assertEqual(classify(slow, slow), 'confirmed regression')
        self.assertEqual(classify(slow, fast), 'inconclusive-noisy')
        self.assertEqual(classify(fast, None), 'pass')
        self.assertEqual(classify(fast, None, 'inconclusive'), 'pass; A/A inconclusive')
        self.assertEqual(classify(None, None), 'unavailable baseline library')
        self.assertEqual(classify(slow, slow, 'confirmed A/A bias'), 'inconclusive control bias')
        self.assertEqual(classify(slow, slow, mode='one_shot'), 'one-shot latency observation')
        self.assertEqual(classify(slow, slow, target={}), 'inconclusive duration shortfall')

    def test_diagnostic_selection_preserves_all_regimes_at_a_declared_scale(self):
        bench = CI.load('bench-bc')
        for preset in ('full', 'holdout'):
            cases = CI.diagnostic_cases(bench, preset)
            self.assertEqual(len(cases), 10)
            self.assertTrue(all(c['parameters']['mode'] == 'one_shot' and
                                c['parameters']['scale'] == 100 for c in cases))
            self.assertEqual(len({c['group'] for c in cases}), 10)

    def test_profiled_config_cannot_enter_elapsed_harness(self):
        bench = CI.load('bench-bc')
        with tempfile.TemporaryDirectory() as temporary:
            config = Path(temporary) / 'profile.json'
            config.write_text(json.dumps({'implementations': [{'name': 'profile',
                'argv': [sys.executable], 'metadata': {'instrumented': True,
                    'source_revision': 'fixture', 'compiler': 'fixture', 'flags': 'fixture',
                    'version': 'fixture', 'binary_sha256': bench.digest_file(sys.executable)}}]}))
            with self.assertRaisesRegex(bench.BenchmarkError, 'instrument'):
                bench.implementations(config, require_provenance=True)

    def test_control_bias_needs_same_direction_confirmation(self):
        high, low = dict(ci95=[1.1, 1.2]), dict(ci95=[.8, .9])
        self.assertEqual(CI.aa_status(high), 'inconclusive')
        self.assertEqual(CI.aa_status(high, low), 'inconclusive')
        self.assertEqual(CI.aa_status(high, high), 'confirmed A/A bias')
        self.assertEqual(CI.aa_status(low, low), 'confirmed A/A bias')
        self.assertEqual(CI.aa_status(dict(ci95=[.98, 1.02])), 'pass')
        self.assertEqual(CI.aa_status(dict(ci95=[.9, 1.1])), 'inconclusive')
        self.assertEqual(CI.aa_status({}), 'inconclusive')

    def test_goal_requires_checked_duration_and_preserves_inconclusive_controls(self):
        row = dict(parameters=dict(mode='repeated'), duration_target_met=dict(gd=True, howard=True),
                   candidate_over_reference={'gd-portable': dict(median_paired_ratio=.89, ci95=[.85, .95])})
        self.assertEqual(CI.goal_status(row, 'pass'), 'target observed')
        self.assertEqual(CI.goal_status(row, 'inconclusive'), 'target observed; A/A inconclusive')
        self.assertEqual(CI.goal_status(row, 'confirmed A/A bias'), 'inconclusive control bias')
        row['candidate_over_reference']['gd-portable']['median_paired_ratio'] = .91
        self.assertEqual(CI.goal_status(row, 'pass'), 'target not established')
        for duration in ({}, dict(gd=False, howard=True)):
            row['duration_target_met'] = duration
            self.assertEqual(CI.goal_status(row, 'pass'), 'insufficient measured duration')
        del row['duration_target_met']
        self.assertEqual(CI.goal_status(row, 'pass'), 'insufficient measured duration')
        row['parameters']['mode'] = 'one_shot'
        self.assertEqual(CI.goal_status(row, 'pass'), 'one-shot latency observation')

    @unittest.skipUnless(os.name in ('posix', 'nt'), 'tree termination needs a native supported API')
    def test_timeout_reaps_descendants_with_inherited_output_pipes(self):
        # Run the timeout handler under another bounded process: a surviving
        # grandchild would otherwise hold communicate() open past the deadline.
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            ready = root / 'ready'
            grandchild = 'from pathlib import Path;import time;Path(' + repr(str(ready)) + ').touch();time.sleep(30)'
            child = 'import subprocess,sys;subprocess.run([sys.executable,"-c",' + repr(grandchild) + '])'
            outer = ('import importlib.util,pathlib;'
                     's=importlib.util.spec_from_file_location("ci",' + repr(str(PATH)) + ');'
                     'm=importlib.util.module_from_spec(s);s.loader.exec_module(m);'
                     'm.Commands(pathlib.Path(' + repr(str(root)) + '),10).run(' +
                     repr([sys.executable, '-c', child]) + ',timeout=2)')
            result = subprocess.run([sys.executable, '-c', outer], stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE, timeout=12)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn(b'exceeded its deadline', result.stderr)
            self.assertTrue(ready.exists(), 'grandchild must actually start before the deadline')
            self.assertTrue((root / 'command-001.log').is_file())

    def test_missing_library_emits_unavailable_evidence_without_acceptance(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            original_load = CI.load
            # This checks unavailable-source evidence, not machine inventory.
            # Native workflow artifacts exercise the real bounded host probes;
            # a Windows CIM query can itself take longer than this unit test.
            def load(name):
                if name == 'benchmark-ci':
                    return mock.Mock(host_info=lambda: dict(test_host='isolated fixture'))
                return original_load(name)
            argv = [str(PATH), '--source', str(root), '--candidate', 'a' * 40, '--base', 'b' * 40,
                    '--output', str(root / 'results'), '--child-timeout-seconds', '.125']
            with mock.patch.object(CI, 'load', side_effect=load), mock.patch.object(sys, 'argv', argv):
                self.assertEqual(CI.main(), 0)
            evidence = root / 'results/evidence'
            self.assertFalse(json.loads((evidence / 'unavailable.json').read_text())['performance_acceptance'])
            self.assertFalse((evidence / 'decisions.json').exists())
            self.assertEqual(json.loads((evidence / 'orchestration.json').read_text())['commands'], [])
            self.assertEqual(json.loads((evidence / 'orchestration.json').read_text())['child_timeout_seconds'], .125)


    def test_invalid_child_deadlines_fail_before_creating_evidence(self):
        for value in ('0', '-1', 'nan', 'inf'):
            with self.subTest(value=value), tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary)
                argv = [str(PATH), '--source', str(root), '--candidate', 'a' * 40,
                        '--base', 'b' * 40, '--output', str(root / 'results'),
                        '--child-timeout-seconds', value]
                with mock.patch.object(sys, 'argv', argv), mock.patch.object(sys, 'stderr', io.StringIO()):
                    with self.assertRaises(SystemExit) as failure:
                        CI.main()
                self.assertEqual(failure.exception.code, 2)
                self.assertFalse((root / 'results').exists())


    def test_selected_broad_preset_is_explicit_in_unavailable_evidence(self):
        for preset, expected in ((None, ['full', 'holdout']), ('full', ['full']),
                                 ('holdout', ['holdout'])):
            with self.subTest(preset=preset), tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary)
                original_load = CI.load
                def load(name):
                    if name == 'benchmark-ci':
                        return mock.Mock(host_info=lambda: dict(test_host='isolated fixture'))
                    return original_load(name)
                argv = [str(PATH), '--source', str(root), '--candidate', 'a' * 40,
                        '--base', 'b' * 40, '--output', str(root / 'results'),
                        '--broad']
                if preset:
                    argv += ['--broad-preset', preset]
                with mock.patch.object(CI, 'load', side_effect=load), mock.patch.object(sys, 'argv', argv):
                    self.assertEqual(CI.main(), 0)
                evidence = root / 'results/evidence'
                orchestration = json.loads((evidence / 'orchestration.json').read_text())
                self.assertTrue(orchestration['broad'])
                self.assertEqual(orchestration['presets'], expected)
                self.assertFalse((evidence / 'decisions.json').exists())

    def test_single_preset_requires_broad_mode(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            argv = [str(PATH), '--source', str(root), '--candidate', 'a' * 40,
                    '--base', 'b' * 40, '--output', str(root / 'results'),
                    '--broad-preset', 'holdout']
            with mock.patch.object(sys, 'argv', argv), mock.patch.object(sys, 'stderr', io.StringIO()):
                with self.assertRaises(SystemExit) as failure:
                    CI.main()
            self.assertEqual(failure.exception.code, 2)
            self.assertFalse((root / 'results').exists())


class MathEvidenceOrchestration(unittest.TestCase):
    """Simulate a complete two-preset controller; never execute fixture images."""

    def exercise(self, fail_elapsed=False):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        source = Path(temporary.name)
        (source / 'src/bc').mkdir(parents=True)
        (source / 'src/bc/math-library.bc').write_bytes(b'scale=20\n')
        root, events = source / 'results', []
        original_load = CI.load
        bench = original_load('bench-bc')
        codegen = original_load('collect-bc-codegen')
        arithmetic = original_load('benchmark-ci')
        original_collect = codegen.collect
        howard_revision = original_load('build-bc-comparators').HOWARD_REVISION

        class Commands:
            def __init__(self, destination, timeout):
                self.commands = []
                self.deadline = CI.time.monotonic() + timeout

            def run(self, argv, timeout=None):
                self.commands.append(dict(argv=list(map(str, argv))))
                script = Path(argv[1]).name
                if script in ('build-gd-bc.py', 'build-bc-comparators.py'):
                    target = Path(argv[argv.index('--output') + 1])
                    target.mkdir()
                    binary = target / 'build/src/bc/fixture-image'
                    binary.parent.mkdir(parents=True)
                    binary.write_bytes(b'fixture image; must never be executed')
                    revision = (howard_revision if script == 'build-bc-comparators.py' else
                                argv[argv.index('--revision') + 1])
                    name = ('howard' if script == 'build-bc-comparators.py' else
                            argv[argv.index('--name') + 1])
                    entry = dict(name=name, argv=[str(binary)], metadata=dict(
                        instrumented='--diagnostic-profile' in argv,
                        source_revision=revision, binary_sha256=bench.digest_file(binary)))
                    (target / 'implementations.json').write_text(json.dumps(dict(implementations=[entry])))
                    if script == 'build-gd-bc.py':
                        library = target / 'source/src/bc/math-library.bc'
                        library.parent.mkdir(parents=True)
                        library.write_bytes(b'scale=20\n')
                    events.append(('build', name))
                    return
                if script == 'test-math-library-cli.py':
                    events.append(('correctness', 'fixture'))
                    return
                if script != 'bench-bc.py':
                    raise AssertionError('unexpected orchestration command: ' + script)
                target = Path(argv[argv.index('--output') + 1])
                target.mkdir()
                events.append(('elapsed', target.name))
                if '--calibrate-only' in argv:
                    return
                tools = json.loads(Path(argv[argv.index('--config') + 1]).read_text())['implementations']
                reference = argv[argv.index('--reference') + 1]
                preset = target.name.split('-')[0]
                name = preset + '_fixture'
                ratio = dict(median_paired_ratio=1.0, ci95=[1.0, 1.0], pairs=3)
                row = dict(name=name, group='math_fixture', parameters=dict(mode='repeated', scale=20),
                           duration_target_met={tool['name']: True for tool in tools},
                           candidate_over_reference={tool['name']: copy.deepcopy(ratio)
                                                     for tool in tools if tool['name'] != reference})
                (target / 'summary.json').write_text(json.dumps(dict(workloads=[row])))
                samples = [dict(phase='measurement', round=index, workload=name,
                                implementation=tool['name'], seconds=1.0, checked=True)
                           for index in range(3) for tool in tools]
                (target / 'samples.jsonl').write_text('\n'.join(json.dumps(r) for r in samples))
                if fail_elapsed and target.name == 'full-paired':
                    raise CI.CommandFailure('forced elapsed failure after fixture samples')

        def modules(name):
            if name == 'benchmark-ci':
                return arithmetic
            if name == 'collect-bc-codegen':
                return codegen
            return original_load(name)

        def collect(*args, **kwargs):
            events.append(('archive', 'ordinary roles'))
            return original_collect(*args, **kwargs)

        argv = [str(PATH), '--source', str(source), '--candidate', 'b' * 40,
                '--base', 'a' * 40, '--output', str(root), '--broad', '--samples', '3']
        with mock.patch.object(CI, 'Commands', Commands), mock.patch.object(CI, 'load', modules), \
                mock.patch.object(arithmetic, 'host_info', return_value=dict(test_host='fixture')), \
                mock.patch.object(CI, 'selected_cases', side_effect=lambda bench, preset, broad: [preset + '_fixture']), \
                mock.patch.object(CI, 'diagnostic_cases', return_value=[]), \
                mock.patch.object(CI, 'allocation_diagnostics', side_effect=lambda *a, **k: events.append(('allocation', 'fixture'))), \
                mock.patch.object(CI, 'resource_diagnostics', side_effect=lambda *a, **k: events.append(('resource', 'fixture'))), \
                mock.patch.object(codegen, 'collect', side_effect=collect), \
                mock.patch.object(codegen, 'native_commands', return_value=[]), \
                mock.patch.dict(os.environ, {'GITHUB_STEP_SUMMARY': ''}), \
                mock.patch.object(sys, 'argv', argv), contextlib.redirect_stderr(io.StringIO()):
            status = CI.main()
        return status, root, events

    def test_all_diagnostic_probes_follow_both_elapsed_presets(self):
        status, root, events = self.exercise()
        self.assertEqual(status, 0)
        elapsed = [i for i, event in enumerate(events) if event[0] == 'elapsed']
        diagnostics = [i for i, event in enumerate(events) if event[0] in ('allocation', 'resource')]
        self.assertEqual(len(elapsed), 6)
        self.assertGreater(min(diagnostics), max(elapsed))

    def test_math_archive_contains_all_measured_roles_and_external_inputs(self):
        status, root, events = self.exercise()
        self.assertEqual(status, 0)
        manifest = root / 'evidence/codegen/manifest.json'
        self.assertTrue(manifest.is_file())
        report = json.loads(manifest.read_text())
        self.assertEqual(report['status'], 'preserved')
        self.assertEqual({r['name'] for r in report['observations']},
                         {'gd-portable', 'gd-aa', 'gd-base', 'howard-standard', 'howard-portable'})
        for row in report['observations']:
            self.assertFalse(row['implementation']['metadata']['instrumented'])
            self.assertEqual((manifest.parent / row['binary']).read_bytes(),
                             b'fixture image; must never be executed')
            for copied in row['input_artifacts']:
                self.assertEqual((manifest.parent / copied['archive_path']).read_bytes(), b'scale=20\n')

    def test_failed_elapsed_run_still_preserves_completed_ordinary_assets(self):
        status, root, events = self.exercise(fail_elapsed=True)
        self.assertEqual(status, 1)
        self.assertIn('forced elapsed failure', (root / 'evidence/failure.json').read_text())
        manifest = root / 'evidence/codegen/manifest.json'
        self.assertTrue(manifest.is_file())
        self.assertEqual(json.loads(manifest.read_text())['status'], 'preserved')
        self.assertFalse(any(event[0] in ('allocation', 'resource') for event in events))


if __name__ == '__main__':
    unittest.main()
