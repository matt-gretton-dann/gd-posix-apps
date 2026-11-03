#!/usr/bin/env python3
"""Guard full-grid accounting and preserve the distinction from partial runs."""
import copy
import importlib.util
import json
from pathlib import Path
import unittest
import zipfile

SPEC = importlib.util.spec_from_file_location('native_evidence', Path(__file__).with_name('verify.py'))
V = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(V)


class NativeEvidence(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        entries = json.loads((V.ROOT / 'checkpoints.json').read_text())['artifacts']
        cls.complete = next(e for e in entries if e['complete'])
        cls.partial = next(e for e in entries if not e['complete'])
        cls.data = V.payloads(V.ROOT / cls.complete['path'], cls.complete['sha256'])
        cls.partial_data = V.payloads(V.ROOT / cls.partial['path'], cls.partial['sha256'])

    def rows(self, change):
        data = dict(self.data)
        name = 'full-aa/samples.jsonl'
        rows = [json.loads(line) for line in data[name].splitlines()]
        change(rows)
        data[name] = ('\n'.join(json.dumps(row) for row in rows) + '\n').encode()
        return data

    def test_failed_job_logs_cannot_change_silently(self):
        index = json.loads((V.ROOT / 'failed-ci-index.json').read_text())
        with zipfile.ZipFile(V.ROOT / 'failed-ci.zip') as source:
            data = {name: source.read(name) for name in source.namelist()}
        V.verify_failed_builds(data, index)
        name = index['jobs'][0]['file']
        data[name] += b'changed'
        with self.assertRaisesRegex(ValueError, 'failed log identity differs'):
            V.verify_failed_builds(data, index)

    def test_complete_grid(self):
        self.assertTrue(V.verify(self.data, self.complete)['complete'])

    def test_partial_is_retained_without_acceptance(self):
        result = V.verify(self.partial_data, self.partial)
        self.assertFalse(result['complete'])
        self.assertFalse(result['performance_acceptance'])
        self.assertTrue(result['missing'])
        self.assertEqual(result['controls'], {})

    def test_duplicate_round_cannot_hide_a_missing_sample(self):
        with self.assertRaisesRegex(ValueError, 'duplicate sample rounds'):
            V.verify(self.rows(lambda rows: rows.__setitem__(-1, copy.deepcopy(rows[0]))), self.complete)

    def test_dropped_sample_fails_a_complete_gate(self):
        with self.assertRaisesRegex(ValueError, 'incomplete grid declared complete'):
            V.verify(self.rows(lambda rows: rows.pop()), self.complete)

    def test_partial_cannot_be_relabelled_complete(self):
        with self.assertRaisesRegex(ValueError, 'incomplete grid declared complete'):
            V.verify(self.partial_data, dict(self.partial, complete=True))

    def test_completed_record_cannot_discard_its_decisions(self):
        with self.assertRaisesRegex(ValueError, 'partial record unexpectedly'):
            V.verify(self.data, dict(self.complete, complete=False))

    def test_unchecked_sample_fails(self):
        with self.assertRaisesRegex(ValueError, 'unchecked observations'):
            V.verify(self.rows(lambda rows: rows[0].__setitem__('checked', False)), self.complete)

    def test_invalid_round_fails_without_changing_sample_count(self):
        with self.assertRaisesRegex(ValueError, 'invalid sample round'):
            V.verify(self.rows(lambda rows: rows[0].__setitem__('round', 999)), self.complete)

    def test_changed_binary_fails(self):
        data = dict(self.data)
        code = json.loads(data['codegen/manifest.json'])['observations'][0]
        name = 'codegen/' + code['binary']
        payload = bytearray(data[name]);payload[-1] ^= 1;data[name] = bytes(payload)
        with self.assertRaisesRegex(ValueError, 'binary identity differs'):
            V.verify(data, self.complete)

    def test_removed_decision_fails(self):
        data = dict(self.data);decisions = json.loads(data['decisions.json'])
        decisions['results'].pop();data['decisions.json'] = json.dumps(decisions).encode()
        with self.assertRaisesRegex(ValueError, 'decision inventory differs'):
            V.verify(data, self.complete)

    def test_control_qualification_cannot_be_replaced(self):
        data = dict(self.data);decisions = json.loads(data['decisions.json'])
        row = next(row for row in decisions['results'] if row['comparison'] == 'aa')
        row['status'] = 'invented pass';data['decisions.json'] = json.dumps(decisions).encode()
        with self.assertRaisesRegex(ValueError, 'control qualification differs'):
            V.verify(data, self.complete)

    def test_changed_workload_fails(self):
        data = dict(self.data)
        name = next(name for name in data if name.startswith('full-aa/workloads/') and name.endswith('.bc'))
        data[name] += b'1\n'
        with self.assertRaisesRegex(ValueError, 'workload or oracle hash differs'):
            V.verify(data, self.complete)


if __name__ == '__main__':
    unittest.main()
