#!/usr/bin/env python3
"""Check #707's prospectively declared frame workloads without elapsed sampling."""
import hashlib
import importlib.util
import json
from pathlib import Path
import random
import re
import sys
import unittest

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location('binding_corpus', ROOT / 'src/bc/scripts/bench-bc.py')
BENCH = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(BENCH)
GROUPS = ('bindings_wide_locals', 'bindings_dynamic_chain', 'bindings_recursion_snapshots')
LETTERS = 'abcdehjklmopqrtu'


class BindingCorpus(unittest.TestCase):
    def test_historical_identities(self):
        original = json.loads((ROOT / 'docs/utilities/bc-scalar-local-bindings/'
                               'original-arithmetic-corpus.json').read_text())
        for preset, seed in [('full', 197200), ('holdout', 197201)]:
            cases = BENCH.corpus(preset, listing=True, seed=seed)
            self.assertEqual(len(cases), 69)
            self.assertEqual(len(original[preset]), 66)
            for actual, previous in zip(cases[:66], original[preset]):
                self.assertEqual(actual['name'], previous['name'])
                self.assertEqual(actual['group'], previous['group'])
                self.assertEqual(actual['parameters'], previous['parameters'])
                self.assertEqual(hashlib.sha256(actual['program'].encode()).hexdigest(),
                                 previous['program_sha256'])

    def test_independent_integer_execution(self):
        for preset, seed, digits, width, depth, counts in [
                ('quick', 197200, 180, 17, 37, (750, 1000, 12)),
                ('full', 197200, 180, 17, 37, (75000, 100000, 1200)),
                ('holdout', 197201, 207, 18, 41, (65625, 87500, 1050))]:
            catalog = BENCH.corpus(preset, listing=True, seed=seed)
            names = [c['name'] for c in catalog if c['group'] in GROUPS]
            cases = BENCH.corpus(preset, names, seed=seed)
            self.assertEqual([c['group'] for c in cases], list(GROUPS))
            rng = random.Random(seed ^ 707197)
            def dense(n):
                return int('1' + ''.join(str(rng.randrange(10)) for _ in range(n - 1)))
            for case, count, group in zip(cases, counts, GROUPS):
                p = case['parameters']
                self.assertEqual(p['iterations'], count)
                self.assertEqual(p['operand_seed'], seed ^ 707197)
                self.assertEqual(p['checksum_modulus'], 1000000007)
                source = case['program']
                self.assertIn('for(i=0;i<{};i++)'.format(count), source)
                total = 0
                if group == GROUPS[0]:
                    global_value, initial = dense(digits), dense(width)
                    coefficients = [rng.randrange(1, 98) for _ in LETTERS]
                    self.assertEqual(p['coefficients'], coefficients)
                    self.assertEqual(p['global_value'], str(global_value))
                    self.assertEqual(p['initial'], str(initial))
                    self.assertIn('auto ' + ','.join(LETTERS), source)
                    for j, (letter, coefficient) in enumerate(zip(LETTERS, coefficients), 1):
                        self.assertIn('{}=n+{}\n'.format(letter, coefficient), source)
                        self.assertIn('{}={}\n'.format(letter, global_value + j), source)
                    for i in range(count):
                        locals_ = [initial + i + coefficient for coefficient in coefficients]
                        returned = sum(j * value for j, value in enumerate(locals_, 1))
                        total = (total + returned) % 1000000007
                    expected = [total] + [(global_value + j) % 1000000007 for j in range(1, 17)]
                elif group == GROUPS[1]:
                    global_value, initial = dense(digits), dense(width)
                    c, d, y = [rng.randrange(1, 98) for _ in range(3)]
                    self.assertEqual([p['c'], p['d'], p['y0']], [c, d, y])
                    self.assertEqual(p['initial'], str(initial))
                    self.assertEqual(p['global_value'], str(global_value))
                    for i in range(count):
                        x = initial + i
                        x += c
                        y += 1
                        t = x
                        x += d
                        y += 1
                        u = x
                        total = (total + t + u + y) % 1000000007
                    expected = [total, y] + [(global_value + j) % 1000000007 for j in range(3)] + [31]
                else:
                    initial, delta = dense(digits), rng.randrange(1, 98)
                    self.assertEqual(p['initial'], str(initial))
                    self.assertEqual((p['depth'], p['delta']), (depth, delta))
                    for i in range(count):
                        stack = [initial + i]
                        for _ in range(depth):
                            stack.append(stack[-1] + delta)
                        returned = 0
                        while stack:
                            returned += stack.pop()
                        total = (total + returned) % 1000000007
                    expected = [total, initial % 1000000007, (initial + 1) % 1000000007, 29]
                self.assertEqual(case['expected'], [str(x) for x in expected])

    def test_held_out_parameters_are_distinct_and_reproducible(self):
        full = BENCH.corpus('full', listing=True, seed=197200)[66:]
        held = BENCH.corpus('holdout', listing=True, seed=197201)[66:]
        self.assertEqual(len(full), 3)
        self.assertEqual(held, BENCH.corpus('holdout', listing=True, seed=197201)[66:])
        for left, right in zip(full, held):
            self.assertNotEqual(left['parameters'], right['parameters'])
            self.assertNotEqual(left['program'], right['program'])


if __name__ == '__main__':
    unittest.main()
