#!/usr/bin/env python3
"""Check array experiment inputs with independent oracles and the real VM."""
import argparse
import importlib.util
from pathlib import Path
import tempfile
import unittest


def load(name):
    path = Path(__file__).resolve().parent / 'scripts' / (name + '.py')
    spec = importlib.util.spec_from_file_location(name.replace('-', '_'), str(path))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class ArrayBenchmark(unittest.TestCase):
    def test_quick_oracles_match_the_real_interpreter(self):
        generator, bench = load('prepare-array-benchmark'), load('bench-bc')
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory) / 'prepared'
            generator.prepare(root, 'quick')
            cases, metadata = bench.replay_corpus(root, 'arithmetic')
            self.assertEqual(len(cases), 8)
            self.assertEqual(metadata['original_preset'], 'quick')
            for case in cases:
                with self.subTest(workload=case['name']):
                    bench.run_checked(dict(name='actual-vm', argv=[BC]), case, 10)

    def test_presets_keep_separate_dimensions_and_exact_sparse_bounds(self):
        generator = load('prepare-array-benchmark')
        full, held = generator.corpus('full'), generator.corpus('holdout')
        self.assertEqual([c['group'] for c in full], [c['group'] for c in held])
        for first, second in zip(full, held):
            self.assertNotEqual(first['parameters'], second['parameters'])
            self.assertNotEqual(first['program'], second['program'])
        # The sparse modulus and maximum explicit key stay below the native
        # language limit; the full-width C++ storage oracle covers its boundary.
        sparse = next(c for c in full if c['group'] == 'array_sparse')
        self.assertLess(sparse['parameters']['modulus'], 1000000000)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('bc')
    args, remaining = parser.parse_known_args()
    BC = str(Path(args.bc).resolve())
    unittest.main(argv=[__file__, *remaining])
