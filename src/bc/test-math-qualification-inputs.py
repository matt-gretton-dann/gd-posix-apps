#!/usr/bin/env python3
"""Verify declared workload coverage and oracle identities without timing bc."""
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

PATH = Path(__file__).resolve().parent / 'scripts/math-qualification-inputs.py'
SPEC = importlib.util.spec_from_file_location('declared_math_inputs', PATH)
INPUTS = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(INPUTS)


class QualificationInputs(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.bench = INPUTS.bench_module()
        cls.validation = INPUTS.fresh(cls.bench)
        expected = json.loads((INPUTS.ROOT / 'fixtures/math-certified-validation-inputs.json').read_text(encoding='utf-8'))
        if cls.validation != expected:
            raise AssertionError('fresh declaration differs from its prospectively frozen fixture')

    def assert_partition_union(self, rows, shards):
        seen = set()
        for band in INPUTS.BANDS:
            for shard in range(shards):
                selected = INPUTS.select(rows, band, shard, shards)
                names = {r['name'] for r in selected}
                self.assertFalse(seen & names)
                seen.update(names)
                for key in {INPUTS.pair_key(r) for r in selected}:
                    self.assertEqual({r['parameters']['mode'] for r in selected
                        if INPUTS.pair_key(r) == key}, {'one_shot', 'repeated'})
        self.assertEqual(seen, {r['name'] for r in rows})

    def test_exposed_union_keeps_all_original_bytes_and_pairs(self):
        for preset in ('full', 'holdout'):
            rows = INPUTS.exposed(preset)
            self.assertEqual(len(rows), 94)
            self.assert_partition_union(rows, 3)
            for band in INPUTS.BANDS:
                for shard in range(3):
                    selected = INPUTS.select(rows, band, shard, 3)
                    with tempfile.TemporaryDirectory() as temporary:
                        directory = Path(temporary) / 'inputs'
                        INPUTS.write(selected, 'exposed', preset, band, shard, 3, directory)
                        restored, _ = self.bench.declared_corpus(directory, 'math')
                        self.assertEqual({r['name'] for r in restored}, {r['name'] for r in selected})
                        for old, new in zip(selected, restored):
                            self.assertEqual(new['program'], old['program'])
                            self.assertEqual(self.bench.oracle_payload(new).decode(), old['oracle_payload'])

    def test_validation_covers_every_declared_scale_and_precision(self):
        self.assertEqual(len(self.validation), 36)
        self.assert_partition_union(self.validation, 1)
        p = [r['parameters'] for r in self.validation if r['parameters']['mode'] == 'one_shot']
        for order in ('0', '1'):
            rows = [r for r in p if r['arguments'][0] == order]
            self.assertEqual({(r['scale'], r['input_digits']) for r in rows},
                {(0, 6), (1, 256), (20, 6), (20, 256), (100, 6), (100, 256), (1000, 6), (1000, 64)})
        for band in INPUTS.BANDS:
            rows = INPUTS.select(self.validation, band, 0, 1)
            with tempfile.TemporaryDirectory() as temporary:
                directory = Path(temporary) / 'inputs'
                INPUTS.write(rows, 'validation', 'holdout', band, 0, 1, directory)
                restored, _ = self.bench.declared_corpus(directory, 'math')
                self.assertEqual(len(restored), len(rows))

    def test_exact_forward_enclosures_contain_independent_alternating_bounds(self):
        from fractions import Fraction
        for order in (0, 1, 2):
            lo, hi = map(Fraction, INPUTS.enclosure(order, '1.25', 20))
            x, q = Fraction(5, 4), Fraction(25, 64)
            term = (x / 2)**order / INPUTS.factorial(order)
            total = term
            for k in range(1, 81):
                term *= -q / (k * (order + k))
                total += term
            following = -term * q / (81 * (order + 81))
            self.assertLess(lo, min(total, total + following))
            self.assertGreater(hi, max(total, total + following))

    def test_invalid_partition_and_domain_are_rejected(self):
        for args in [('low', -1, 3), ('high', 3, 3), ('unknown', 0, 1)]:
            with self.assertRaises(ValueError):
                INPUTS.select(self.validation, *args)
        for args in [(3, '1', 20), (0, '6', 20), (1, '1', 1001)]:
            with self.assertRaises(ValueError):
                INPUTS.enclosure(*args)


if __name__ == '__main__':
    unittest.main()
