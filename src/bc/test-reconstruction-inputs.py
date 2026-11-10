#!/usr/bin/env python3
"""Check complete control retention, fresh enclosures and real small-count calls."""
import hashlib
import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location('inputs', ROOT/'scripts/reconstruction-qualification-inputs.py')
INPUTS = importlib.util.module_from_spec(spec)
spec.loader.exec_module(INPUTS)
BINARY = sys.argv.pop() if len(sys.argv)>1 and not sys.argv[-1].startswith('-') else None


class ReconstructionInputs(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.bench = INPUTS.OLD.bench_module()
        cls.fresh = INPUTS.validation()

    def test_all224_original_programs_oracles_and_dimensions_survive(self):
        originals = INPUTS.OLD.exposed('full')+INPUTS.OLD.exposed('holdout')+INPUTS.OLD.fresh(self.bench)
        combined = INPUTS.controls('full')+INPUTS.controls('holdout')
        self.assertEqual(len(originals), 224)
        self.assertEqual({r['name']: r for r in originals}, {r['name']: r for r in combined})

    def test_every_partition_is_disjoint_and_keeps_both_modes(self):
        for family, preset, rows in [('controls','full',INPUTS.controls('full')),
                                     ('controls','holdout',INPUTS.controls('holdout')),
                                     ('validation','holdout',self.fresh)]:
            union=[]
            for band in INPUTS.OLD.BANDS:
                for shard in range(3):
                    selected=INPUTS.OLD.select(rows,band,shard,3)
                    union+=selected
                    modes={}
                    for r in selected:
                        modes.setdefault(INPUTS.OLD.pair_key(r),set()).add(r['parameters']['mode'])
                    self.assertTrue(all(v=={'one_shot','repeated'} for v in modes.values()))
                    with tempfile.TemporaryDirectory() as tmp:
                        directory=Path(tmp)/'inputs'
                        INPUTS.write(selected,family,preset,band,shard,3,directory)
                        restored,_=self.bench.declared_corpus(directory,'math')
                        self.assertEqual([r['name'] for r in restored],[r['name'] for r in selected])
                        for before, after in zip(selected,restored):
                            self.assertEqual(before['program'],after['program'])
                            self.assertEqual(before['oracle_payload'],self.bench.oracle_payload(after).decode())
            self.assertEqual(len(union),len(rows))
            self.assertEqual({r['name'] for r in union},{r['name'] for r in rows})

    def test_new_oracles_are_pinned_independent_and_numerically_disjoint(self):
        self.assertEqual(len(self.fresh),76)
        self.assertEqual({r['parameters']['function'] for r in self.fresh},set('aelscj'))
        self.assertEqual({r['parameters']['scale'] for r in self.fresh},{0,20,100,1000})
        cases=ROOT/'fixtures/math-reconstruction-validation-cases.json'
        report=json.loads((ROOT/'fixtures/math-reconstruction-validation-oracles.json').read_text())
        self.assertEqual(report['case_file_sha256'],hashlib.sha256(cases.read_bytes()).hexdigest())
        self.assertEqual(report['endpoint_format'],'exact rational outward dyadic bounds')
        # Generator identity, source pin and old-value disjointness are also
        # checked before the production adapter returns any new case.
        self.assertEqual(len(report['cases']),38)

    @unittest.skipUnless(BINARY,'requires the actual test executable')
    def test_actual_implementation_checks_all_fresh_small_count_programs(self):
        # Counts are one/two, never the later calibrated performance loops.
        for row in self.fresh:
            with self.subTest(name=row['name']):
                process=subprocess.run([BINARY,'-l'],input=row['program'].encode(),
                                       stdout=subprocess.PIPE,stderr=subprocess.PIPE,timeout=60)
                self.assertEqual(process.returncode,0,process.stderr)
                self.assertEqual(process.stderr,b'')
                oracle=json.loads(row['oracle_payload'])
                self.bench.check_output(dict(oracle=oracle),
                                        self.bench.canonical_output(process.stdout),row['name'])


if __name__=='__main__':
    unittest.main()
