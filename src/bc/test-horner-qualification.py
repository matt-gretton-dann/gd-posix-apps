#!/usr/bin/env python3
"""Check prospective Bessel corpus composition and exact replay, never timing."""
from fractions import Fraction
import hashlib
import importlib.util
import json
from pathlib import Path
import random
import tempfile
import unittest

SCRIPTS = Path(__file__).resolve().parent / 'scripts'


def load(name):
    spec = importlib.util.spec_from_file_location(name, SCRIPTS / (name + '.py'))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class HornerQualification(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.bench = load('bench-bc')
        cls.corpus = load('bessel-horner-corpus')

    def test_partitions_retain_every_historical_payload_and_new_regime(self):
        for preset in ('full', 'holdout'):
            historical = self.bench.corpus(preset, suite='math')
            entire = self.corpus.cases(self.bench, preset)
            self.assertEqual(len(historical), 60)
            self.assertEqual(len(entire), 94)
            by_name = {case['name']: case for case in entire}
            self.assertEqual(len(by_name), 94)
            for old in historical:
                self.assertEqual(old, by_name[old['name']])
                self.assertEqual(self.bench.oracle_payload(old),
                                 self.bench.oracle_payload(by_name[old['name']]))
            seen = set()
            for partition, count in [('low', 34), ('middle', 30), ('high', 30)]:
                selected = self.corpus.partition(entire, partition)
                self.assertEqual(len(selected), count)
                names = {case['name'] for case in selected}
                self.assertFalse(seen & names)
                seen |= names
            self.assertEqual(seen, set(by_name))
            new = [c for c in entire if c['parameters'].get('horner_experiment')]
            self.assertEqual(len(new), 34)
            self.assertEqual(sum(c['parameters']['exposure'] == 'existing midpoint control'
                                 for c in new), 4)
            self.assertEqual({c['parameters']['scale'] for c in new}, {0, 20, 100, 101})

    def test_generation_uses_the_prospective_independent_draw_order(self):
        observed = []
        for preset, seed in [('full', 217709), ('holdout', 217710)]:
            rng = random.Random(seed)
            expected = [
                (0, '{}.{:06d}'.format(rng.randrange(1, 4), rng.randrange(10**6))),
                (1, '1.{:06d}'.format(rng.randrange(10**6))),
                (1, '2.' + ''.join(str(rng.randrange(10)) for _ in range(256))),
                (1, '4.{:06d}'.format(rng.randrange(1, 10**6))),
                (2, '1.{:06d}'.format(rng.randrange(10**6))),
            ]
            actual = self.corpus.inputs(preset)
            self.assertEqual([(n, x) for _, n, x in actual], expected)
            self.assertEqual(len({name for name, _, _ in actual}), 5)
            self.assertEqual(len(actual[2][2].split('.')[1]), 256)
            observed.append(actual)
        self.assertNotEqual(observed[0], observed[1])

    def test_fresh_enclosures_are_strict_and_compact(self):
        for preset in ('full', 'holdout'):
            for case in self.corpus.cases(self.bench, preset):
                if not case['parameters'].get('horner_experiment'):
                    continue
                oracle = case['oracle']
                lower, upper = Fraction(oracle['lower']), Fraction(oracle['upper'])
                scale = case['parameters']['scale']
                self.assertLess(lower, upper)
                self.assertLessEqual(upper - lower, Fraction(3, 10**(scale + 14)))
                self.assertLess(len(json.dumps(oracle)), 2000)
                self.assertEqual(oracle['expected_state'], [str(scale), '10', str(scale)])

    def test_replay_binds_every_partition_input_and_oracle(self):
        for preset in ('full', 'holdout'):
            with tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary) / 'corpus'
                expected = self.corpus.partition(self.corpus.cases(self.bench, preset), 'low')
                self.corpus.write_partition(self.bench, preset, 'low', root)
                replayed, metadata = self.bench.replay_corpus(root, 'math')
                self.assertEqual(replayed, expected)
                self.assertEqual(metadata['source_manifest_sha256'],
                                 hashlib.sha256((root / 'manifest.json').read_bytes()).hexdigest())
                path = root / 'workloads' / (expected[-1]['name'] + '.expected')
                path.write_bytes(path.read_bytes() + b' ')
                with self.assertRaisesRegex(self.bench.BenchmarkError, 'hash mismatch'):
                    self.bench.replay_corpus(root, 'math')

    def test_unknown_presets_and_partitions_are_rejected(self):
        with self.assertRaises(ValueError):
            self.corpus.inputs('quick')
        with self.assertRaises(ValueError):
            self.corpus.partition([], 'best')

    def test_resource_additions_are_exact_one_shot_inputs_from_the_same_partition(self):
        resources = load('compare-bc-resources')
        for name, count in [('low', 7), ('middle', 5), ('high', 5)]:
            with tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary) / 'corpus'
                self.corpus.write_partition(self.bench, 'holdout', name, root)
                selected = resources.additional_cases(self.bench, root)
                entire = self.corpus.partition(self.corpus.cases(self.bench, 'holdout'), name)
                expected = [c for c in entire if c['parameters'].get('horner_experiment')
                            and c['parameters']['mode'] == 'one_shot']
                self.assertEqual(selected, expected)
                self.assertEqual(len(selected), count)


if __name__ == '__main__':
    unittest.main()
