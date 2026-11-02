#!/usr/bin/env python3
"""Assert fixed deployment/provenance and bounded controller decisions."""
import copy
import importlib.util
import json
from pathlib import Path
import tempfile
import types
import unittest
from unittest import mock

SCRIPTS = Path(__file__).parent / 'scripts'
SPEC = importlib.util.spec_from_file_location('image_controls', SCRIPTS / 'benchmark-image-controls.py')
CONTROL = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CONTROL)
BENCH = CONTROL.load('bench-bc')
POLICY = CONTROL.load('benchmark-policy')
GD = CONTROL.load('build-gd-bc')


class DeploymentTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.binary = self.root / 'original.exe'
        self.binary.write_bytes(b'unit fixture; never executed')
        self.entry = dict(name='source', argv=[str(self.binary)], metadata=dict(
            instrumented=False, binary_sha256=BENCH.digest_file(self.binary), source_revision='a'*40))

    def test_copies_are_exact_distinct_and_do_not_mutate_recipe(self):
        original = copy.deepcopy(self.entry)
        tools, images = CONTROL.deploy(self.entry, self.root / 'images')
        self.assertEqual(self.entry, original)
        self.assertEqual(len(tools), 6)
        self.assertEqual({t['name'] for t in tools}, {'image'+str(i) for i in range(6)})
        self.assertEqual(len({len(r['path']) for r in images}), 1)
        self.assertEqual(len({r['file_identity_sha256'] for r in images}), 6)
        for tool, image in zip(tools, images):
            self.assertEqual(BENCH.digest_file(tool['argv'][0]), original['metadata']['binary_sha256'])
            self.assertEqual(image['sha256'], original['metadata']['binary_sha256'])
            self.assertTrue(tool['metadata']['deployment']['bytes_unchanged'])
            self.assertNotEqual(Path(tool['argv'][0]).stat().st_ino, self.binary.stat().st_ino)

    def test_changed_source_is_rejected(self):
        self.binary.write_bytes(b'changed')
        with self.assertRaises(ValueError):
            CONTROL.deploy(self.entry, self.root / 'images')

    def test_instrumentation_and_extra_arguments_are_rejected(self):
        for change in ('instrumentation', 'arguments'):
            entry = copy.deepcopy(self.entry)
            if change == 'instrumentation':
                entry['metadata']['instrumented'] = True
            else:
                entry['argv'].append('--unexpected')
            with self.assertRaises(ValueError):
                CONTROL.deploy(entry, self.root / change)

    def test_changed_copy_is_rejected(self):
        def copy_bad(source, target):
            Path(target).write_bytes(b'not the source')
        with mock.patch.object(CONTROL.shutil, 'copyfile', side_effect=copy_bad):
            with self.assertRaises(ValueError):
                CONTROL.deploy(self.entry, self.root / 'images')

    def test_frozen_corpus_preserves_program_oracle_and_file_modes(self):
        CONTROL.freeze_corpus(self.root / 'prepared')
        for preset in ('full', 'holdout'):
            cases, _ = BENCH.replay_corpus(self.root / 'prepared' / preset, 'arithmetic')
            self.assertEqual(len(cases), 7)
            self.assertEqual(sum(c['name'] == 'startup' for c in cases), 1)
            self.assertEqual(sum(c['name'].startswith('compare_') for c in cases), 2)
            self.assertEqual(sum('input_' in c['name'] for c in cases), 2)
            self.assertTrue(all(c['program'] and c['expected'] for c in cases))

    def test_missing_frozen_class_is_a_hard_failure(self):
        original_load = CONTROL.load
        def load(name):
            return types.SimpleNamespace(corpus=lambda *a, **k: []) if name == 'bench-bc' else original_load(name)
        with mock.patch.object(CONTROL, 'load', side_effect=load):
            with self.assertRaises(ValueError):
                CONTROL.freeze_corpus(self.root / 'prepared')


class ControllerTests(unittest.TestCase):
    def exercise(self, native=True, bias=False, fail=False):
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        root = Path(temp.name) / 'out'
        calls = []

        class Commands:
            def __init__(self, path, seconds):
                self.root, self.commands = path, calls

            def run(self, argv, timeout):
                calls.append(dict(argv=argv, timeout=timeout))
                (self.root / ('command-{:03d}.log'.format(len(calls)))).write_text('unit orchestration only')
                output = Path(argv[argv.index('--output')+1])
                output.mkdir(exist_ok=False)
                if '--revision' in argv:
                    binary = output / 'source.exe'
                    binary.write_bytes(b'unit fixture; never executed')
                    entry = dict(name='source', argv=[str(binary)], metadata=dict(instrumented=False,
                        binary_sha256=BENCH.digest_file(binary), source_revision='a'*40))
                    (output/'implementations.json').write_text(json.dumps(dict(implementations=[entry])))
                    (output/'build-manifest.json').write_text('{}')
                    return
                prepared = Path(argv[argv.index('--replay-corpus')+1])
                cases, _ = BENCH.replay_corpus(prepared, 'arithmetic')
                if '--workload' in argv:
                    names = [argv[i+1] for i, value in enumerate(argv) if value == '--workload']
                    cases = [c for c in cases if c['name'] in names]
                rows = []
                for case in cases:
                    ratios = {}
                    for index in range(1,6):
                        slow = bias and index == 1 and case['name'].startswith('loop_')
                        ratios['image'+str(index)] = dict(ci95=[1.1,1.3] if slow else [.99,1.01],
                                                         median_paired_ratio=1.2 if slow else 1.0)
                    rows.append(dict(name=case['name'], candidate_over_reference=ratios))
                (output/'summary.json').write_text(json.dumps(dict(workloads=rows)))

        def ci_load(name):
            if name == 'bench-bc':
                return BENCH
            if name == 'benchmark-policy':
                return POLICY
            if name == 'benchmark-ci':
                return types.SimpleNamespace(host_info=lambda: dict(platform='unit fixture'))
            self.fail(name)
        ci = types.SimpleNamespace(load=ci_load, Commands=Commands, measurements=lambda _: None)
        def probe(path):
            if fail:
                raise ValueError('intentional diagnostic failure')
            return dict(status='collected', checked=True, module_base='0x0000000020000000')
        probe_api = types.SimpleNamespace(native_windows_64=lambda: native, probe=probe)
        original_load = CONTROL.load
        def load(name):
            if name == 'benchmark-math-ci':
                return ci
            if name == 'windows-image-probe':
                return probe_api
            return original_load(name)
        with mock.patch.object(CONTROL, 'load', side_effect=load):
            status = CONTROL.main(['--source',str(root.parent),'--base','a'*40,'--output',str(root)])
        return status, calls, json.loads((root/'evidence/decisions.json').read_text()), root

    def test_unavailable_host_never_builds_or_measures(self):
        status, calls, report, _ = self.exercise(native=False)
        self.assertEqual(status, 0)
        self.assertFalse(calls)
        self.assertEqual(report['status'], 'unavailable')
        self.assertFalse(report['performance_acceptance'])

    def test_all_predetermined_images_probes_and_classes_are_retained(self):
        status, calls, report, root = self.exercise()
        self.assertEqual(status, 0)
        self.assertEqual(len(calls), 3)
        self.assertEqual(len(report['images']), 6)
        self.assertEqual(len(report['probes']), 18)
        self.assertEqual(len(report['results']), 70)
        self.assertFalse(report['performance_acceptance'])
        self.assertEqual(sum(r['status']=='inconclusive' for r in report['results']), 10)
        self.assertTrue(all((root/'evidence'/('controller-command-{:03d}.log'.format(i))).is_file()
                            for i in range(1,4)))

    def test_one_bounded_confirmation_retains_persistent_bias(self):
        status, calls, report, _ = self.exercise(bias=True)
        self.assertEqual(status, 0)
        self.assertEqual(len(calls), 5)
        biased = [r for r in report['results'] if r['status']=='confirmed A/A bias']
        self.assertEqual(len(biased), 2)
        self.assertTrue(all(r['image']=='image1' for r in biased))
        self.assertTrue(all(r['confirmation'] is not None for r in biased))
        self.assertFalse(report['performance_acceptance'])

    def test_invalid_diagnostic_is_a_hard_failure_with_provenance(self):
        status, calls, report, root = self.exercise(fail=True)
        self.assertEqual(status, 1)
        self.assertEqual(len(calls), 1)
        self.assertEqual(report['status'], 'failed')
        self.assertIn('intentional diagnostic failure',report['failure'])
        self.assertTrue((root/'evidence/source-build-manifest.json').is_file())


if __name__ == '__main__':
    unittest.main()
