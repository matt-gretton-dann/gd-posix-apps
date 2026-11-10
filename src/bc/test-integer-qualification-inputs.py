#!/usr/bin/env python3
"""Validate fixed native-family input identities and small-count exact outputs."""
import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('integer_inputs',ROOT/'scripts/integer-qualification-inputs.py')
INPUTS=importlib.util.module_from_spec(spec)
spec.loader.exec_module(INPUTS)
BINARY=sys.argv.pop() if len(sys.argv)>1 and not sys.argv[-1].startswith('-') else None
spec=importlib.util.spec_from_file_location('bench',ROOT/'scripts/bench-bc.py')
BENCH=importlib.util.module_from_spec(spec)
spec.loader.exec_module(BENCH)


class IntegerInputs(unittest.TestCase):
    def test_fixed_production_counts_and_exact_replay_survive(self):
        rows=INPUTS.cases()
        self.assertEqual(len(rows),5)
        self.assertEqual({r['group'] for r in rows},{'calls','recursion'})
        for row in rows:
            self.assertGreaterEqual(row['parameters']['iterations'],1<<18)
            self.assertEqual(row['parameters']['duration_target_seconds'],.25)
            self.assertEqual(row['expected'][1:],['73','0','10'])
        with tempfile.TemporaryDirectory() as tmp:
            directory=Path(tmp)/'inputs'
            INPUTS.write(directory)
            restored,_=BENCH.replay_corpus(directory,'arithmetic')
            self.assertEqual(restored,rows)
            manifest=json.loads((directory/'manifest.json').read_text())
            self.assertEqual(manifest['corpus_seed'],INPUTS.SEED)

    @unittest.skipUnless(BINARY,'requires the actual test executable')
    def test_small_count_programs_preserve_exact_results_and_outer_bindings(self):
        rows=INPUTS.cases(small_counts=True)
        self.assertEqual(rows[0]['expected'][0],'98')  # sum(i*i+1), i=0..6.
        for row in rows:
            with self.subTest(name=row['name']):
                # Seven word calls or two reductions: no production benchmark
                # count or elapsed sample is executed by this functional test.
                result=subprocess.run([BINARY],input=row['program'].encode(),
                    stdout=subprocess.PIPE,stderr=subprocess.PIPE,timeout=20)
                self.assertEqual(result.returncode,0,result.stderr)
                self.assertEqual(result.stderr,b'')
                BENCH.check_output(row,BENCH.canonical_output(result.stdout),row['name'])


if __name__=='__main__':
    unittest.main()
