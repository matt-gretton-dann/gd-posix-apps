#!/usr/bin/env python3
"""Guard retained VM matrices, losses, activation and failed-test evidence."""
import copy
import importlib.util
import json
from pathlib import Path, PureWindowsPath
from types import SimpleNamespace
from unittest import mock
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
        cls.data = V.BASE.payloads(V.ROOT / cls.complete['path'], cls.complete['sha256'])

    def test_package_transport_inventory(self):
        V.verify_transport()

    def test_transport_uses_portable_paths_for_windows_checkout(self):
        original = V.ROOT
        paths = list(original.rglob('*'))

        class WindowsCheckout:
            def __truediv__(self, name):
                return original / name

            def rglob(self, pattern):
                return [SimpleNamespace(is_file=path.is_file, parts=path.parts,
                        name=path.name,
                        relative_to=lambda root, path=path: PureWindowsPath(
                            path.relative_to(original).as_posix())) for path in paths]

        with mock.patch.object(V, 'ROOT', WindowsCheckout()):
            V.verify_transport()

    def rows(self, change):
        data = dict(self.data)
        name = 'full-aa/samples.jsonl'
        rows = [json.loads(line) for line in data[name].splitlines()]
        change(rows)
        data[name] = ('\n'.join(json.dumps(row) for row in rows) + '\n').encode()
        return data


    def test_complete_grid(self):
        self.assertTrue(V.verify(self.data, self.complete)['complete'])


    def test_duplicate_round_cannot_hide_a_missing_sample(self):
        with self.assertRaisesRegex(ValueError, 'duplicate sample rounds'):
            V.verify(self.rows(lambda rows: rows.__setitem__(-1, copy.deepcopy(rows[0]))), self.complete)

    def test_dropped_sample_fails_a_complete_gate(self):
        with self.assertRaisesRegex(ValueError, 'incomplete grid declared complete'):
            V.verify(self.rows(lambda rows: rows.pop()), self.complete)



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


    def test_original_default_corpus_contract_stays_58(self):
        with self.assertRaisesRegex(ValueError, 'base corpus dimensions differ'):
            V.BASE.verify(self.data, self.complete)

    def test_broad_corpus_cannot_be_replaced_by_subset(self):
        with self.assertRaisesRegex(ValueError, 'base corpus dimensions differ'):
            V.BASE.verify(self.data, self.complete, corpora={'full': 44, 'holdout': 44})

    def test_candidate_revision_is_pinned(self):
        entry = dict(self.complete, candidate='0' * 40)
        with self.assertRaisesRegex(ValueError, 'checkpoint contract differs'):
            V.verify(self.data, entry)

    def test_explicit_activation_cannot_be_lost(self):
        data = dict(self.data)
        name = 'candidate/implementations.json'
        recipe = json.loads(data[name])
        recipe['implementations'][0]['metadata']['configuration']['prepared_divisors_policy'] = None
        data[name] = json.dumps(recipe).encode()
        with self.assertRaisesRegex(ValueError, 'explicit candidate activation missing'):
            V.activation(data, 'candidate', True)

    def test_activation_commands_are_hashed(self):
        data = dict(self.data)
        data['candidate/compile_commands.json'] += b' '
        with self.assertRaisesRegex(ValueError, 'activation command identity differs'):
            V.activation(data, 'candidate', True)

    def test_legacy_build_cannot_silently_enable_reuse(self):
        data = dict(self.data)
        commands = json.loads(data['base/compile_commands.json'])
        row = next(row for row in commands if row['file'].endswith('/src/bc/vm.cc'))
        if 'arguments' in row:
            row['arguments'].append('-DGD_BC_PREPARED_DIVISORS=1')
        else:
            row['command'] += ' -DGD_BC_PREPARED_DIVISORS=1'
        data['base/compile_commands.json'] = json.dumps(commands).encode()
        with self.assertRaisesRegex(ValueError, 'compiled activation differs'):
            V.activation(data, 'base', False)

    def test_failed_test_log_is_preserved(self):
        entry = json.loads((V.ROOT / 'checkpoints.json').read_text())['failed_test']
        with zipfile.ZipFile(V.ROOT / 'failed-test.zip') as source:
            data = source.read(entry['file'])
        V.failed_test(data, entry)
        with self.assertRaisesRegex(ValueError, 'failed test log differs'):
            V.failed_test(data + b'changed', entry)

    def test_confirmed_loss_cannot_be_relabelled_pass(self):
        entries = json.loads((V.ROOT / 'checkpoints.json').read_text())['artifacts']
        entry = next(e for e in entries if e['checkpoint'] == '423-f2' and e['host'] == 'linux')
        data = V.BASE.payloads(V.ROOT / entry['path'], entry['sha256'])
        decisions = json.loads(data['decisions.json'])
        row = next(row for row in decisions['results'] if row['status'] == 'confirmed regression')
        row['status'] = 'pass'
        data['decisions.json'] = json.dumps(decisions).encode()
        with self.assertRaisesRegex(ValueError, 'regression qualification differs'):
            V.verify(data, entry)


if __name__ == '__main__':
    unittest.main()
