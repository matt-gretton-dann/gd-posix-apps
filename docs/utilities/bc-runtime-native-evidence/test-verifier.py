#!/usr/bin/env python3
"""Fail closed on mode, source and scope substitutions in retained evidence."""
import copy
import hashlib
import importlib.util
import json
from pathlib import Path, PureWindowsPath
from types import SimpleNamespace
import unittest
from unittest import mock

SPEC = importlib.util.spec_from_file_location('runtime_evidence', Path(__file__).with_name('verify.py'))
V = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(V)


class RuntimeEvidence(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        entries = json.loads((V.ROOT / 'checkpoints.json').read_text())['artifacts']
        cls.entries = {e['checkpoint']: e for e in entries if e['host'] == 'linux'}
        cls.entry = cls.entries['438-46']
        cls.data = V.BASE.payloads(V.ROOT / cls.entry['path'], cls.entry['sha256'])

    def commands(self, transform):
        data = dict(self.data)
        name = 'candidate/compile_commands.json'
        rows = json.loads(data[name])
        for row in rows:
            if row['file'].replace('\\', '/').endswith('/src/bc/vm.cc'):
                row['command'] = transform(row['command'])
        data[name] = json.dumps(rows).encode()
        recipe = json.loads(data['candidate/implementations.json'])
        recipe['implementations'][0]['metadata']['configuration']['prepared_divisors_policy'][
            'verification']['sha256'] = hashlib.sha256(data[name]).hexdigest()
        data['candidate/implementations.json'] = json.dumps(recipe).encode()
        return data

    def test_complete_checkpoint(self):
        self.assertEqual(V.verify(self.data, self.entry)['checked'], self.entry['checked'])

    def test_transport_inventory(self):
        V.verify_transport()

    def test_transport_windows_path_convention(self):
        original = V.ROOT
        paths = list(original.rglob('*'))

        class WindowsCheckout:
            def __truediv__(self, name):
                return original / name

            def rglob(self, pattern):
                return [SimpleNamespace(is_file=p.is_file, parts=p.parts, name=p.name,
                        relative_to=lambda root, p=p: PureWindowsPath(
                            p.relative_to(original).as_posix())) for p in paths]

        with mock.patch.object(V, 'ROOT', WindowsCheckout()):
            V.verify_transport()

    def test_legacy_controls_stay_unknown(self):
        entry = self.entries['430-5b']
        data = V.BASE.payloads(V.ROOT / entry['path'], entry['sha256'])
        V.modes(data, '430-5b', 'base')
        recipe = json.loads(data['base/implementations.json'])
        recipe['implementations'][0]['metadata']['configuration']['prepared_divisors_policy'][
            'enabled'] = False
        data['base/implementations.json'] = json.dumps(recipe).encode()
        with self.assertRaisesRegex(ValueError, 'legacy activation differs'):
            V.modes(data, '430-5b', 'base')

    def test_modern_off_cannot_be_relabelled_unknown(self):
        data = dict(self.data)
        recipe = json.loads(data['candidate/implementations.json'])
        recipe['implementations'][0]['metadata']['configuration']['prepared_divisors_policy'] = None
        data['candidate/implementations.json'] = json.dumps(recipe).encode()
        with self.assertRaisesRegex(ValueError, 'explicit activation differs'):
            V.modes(data, '438-46', 'candidate')

    def test_policy_requires_exact_command_bytes(self):
        data = dict(self.data)
        data['candidate/compile_commands.json'] += b' '
        with self.assertRaisesRegex(ValueError, 'activation command identity differs'):
            V.modes(data, '438-46', 'candidate')

    def test_removed_compact_mode(self):
        data = self.commands(lambda s: s.replace('-DGD_BC_COMPACT_EXECUTION=1', ''))
        with self.assertRaisesRegex(ValueError, 'compiled execution mode differs'):
            V.modes(data, '438-46', 'candidate')

    def test_changed_fusion_mode(self):
        data = self.commands(lambda s: s.replace('-DGD_BC_FUSED_LOADS=1', '-DGD_BC_FUSED_LOADS=0'))
        with self.assertRaisesRegex(ValueError, 'compiled execution mode differs'):
            V.modes(data, '438-46', 'candidate')

    def test_divisor_opt_in_cannot_hide_in_default_off_trial(self):
        data = self.commands(lambda s: s.replace('-DGD_BC_PREPARED_DIVISORS=0',
                                                 '-DGD_BC_PREPARED_DIVISORS=1'))
        with self.assertRaisesRegex(ValueError, 'compiled divisor mode differs'):
            V.modes(data, '438-46', 'candidate')

    def test_duplicate_define_fails(self):
        with self.assertRaisesRegex(ValueError, 'compiled execution mode differs'):
            V.modes(self.commands(lambda s: s + ' -DGD_BC_FUSED_LOADS=1'), '438-46', 'candidate')

    def test_undefine_fails(self):
        with self.assertRaisesRegex(ValueError, 'unexpected mode undefinition'):
            V.modes(self.commands(lambda s: s + ' -UGD_BC_FUSED_LOADS'), '438-46', 'candidate')

    def test_windows_option_spelling(self):
        self.assertEqual(V.definitions('/D"GD_BC_FUSED_LOADS=1" /O2', 'GD_BC_FUSED_LOADS'), ['1'])
        with self.assertRaisesRegex(ValueError, 'unexpected mode undefinition'):
            V.definitions('/DGD_BC_FUSED_LOADS=1 /UGD_BC_FUSED_LOADS', 'GD_BC_FUSED_LOADS')

    def test_source_contract_is_independent_of_index(self):
        with self.assertRaisesRegex(ValueError, 'checkpoint contract differs'):
            V.verify(self.data, dict(self.entry, candidate='0' * 40))

    def test_foundation_subset_cannot_replace_broad_scope(self):
        with self.assertRaisesRegex(ValueError, 'base corpus dimensions differ'):
            V.BASE.verify(self.data, self.entry, corpora={'full': 52})

    def test_duplicate_sample_cannot_hide_missing_round(self):
        data = dict(self.data)
        name = 'full-aa/samples.jsonl'
        rows = [json.loads(line) for line in data[name].splitlines()]
        rows[-1] = copy.deepcopy(rows[0])
        data[name] = ('\n'.join(json.dumps(row) for row in rows) + '\n').encode()
        with self.assertRaisesRegex(ValueError, 'duplicate sample rounds'):
            V.verify(data, self.entry)


    def test_failed_checkpoint_cannot_change_identity(self):
        record = json.loads((V.ROOT / 'failed-checks.json').read_text())
        record['source_revision'] = '0' * 40
        with self.assertRaisesRegex(ValueError, 'failed checkpoint identity differs'):
            V.failed_checks(record)

    def test_failed_outcome_cannot_be_relabelled_success(self):
        record = json.loads((V.ROOT / 'failed-checks.json').read_text())
        record['jobs'][0]['conclusion'] = 'success'
        with self.assertRaisesRegex(ValueError, 'failed outcome differs'):
            V.failed_checks(record)

    def test_failed_job_cannot_be_dropped(self):
        record = json.loads((V.ROOT / 'failed-checks.json').read_text())
        record['jobs'].pop()
        with self.assertRaisesRegex(ValueError, 'failed job inventory differs'):
            V.failed_checks(record)

    def test_failed_diagnostic_cannot_be_omitted(self):
        record = json.loads((V.ROOT / 'failed-checks.json').read_text())
        record['jobs'][0]['diagnostic_lines'] = []
        with self.assertRaisesRegex(ValueError, 'failed diagnostic missing'):
            V.failed_checks(record)


    def partial(self):
        entries = json.loads((V.ROOT / 'checkpoints.json').read_text())['artifacts']
        entry = next(e for e in entries if e['checkpoint'] == '440-b5' and e['host'] == 'windows')
        return entry, V.BASE.payloads(V.ROOT / entry['path'], entry['sha256'])

    def test_partial_collection_retains_six_reproduced_losses(self):
        entry, data = self.partial()
        result = V.verify(data, entry)
        self.assertFalse(result['complete'])
        self.assertEqual(len(result['partial_full_preset']['reproduced_losses']), 6)
        self.assertEqual(result['missing'], [{'directory': 'holdout-paired', 'missing_samples': 1167}])

    def test_partial_checkpoint_cannot_be_relabelled_complete(self):
        entry, data = self.partial()
        with self.assertRaisesRegex(ValueError, 'checkpoint contract differs'):
            V.verify(data, dict(entry, complete=True))

    def test_partial_confirmation_cannot_be_dropped(self):
        entry, data = self.partial()
        name = 'full-paired-confirmation/summary.json'
        summary = json.loads(data[name])
        summary['workloads'].pop()
        data[name] = json.dumps(summary).encode()
        with self.assertRaisesRegex(ValueError, 'partial confirmation inventory differs'):
            V.partial_full_preset(data)

    def test_partial_loss_cannot_be_relabelled_improvement(self):
        entry, data = self.partial()
        name = 'full-paired-confirmation/summary.json'
        summary = json.loads(data[name])
        ratio = summary['workloads'][0]['candidate_over_reference']['candidate']
        ratio['ci95'] = [0.90, 0.99]
        data[name] = json.dumps(summary).encode()
        with self.assertRaisesRegex(ValueError, 'partial reproduced losses differ'):
            V.partial_full_preset(data)


if __name__ == '__main__':
    unittest.main()
