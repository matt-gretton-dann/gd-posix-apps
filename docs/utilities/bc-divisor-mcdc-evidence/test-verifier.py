#!/usr/bin/env python3
"""Reject mode pooling, altered source, and favourable coverage replacements."""
import copy
import importlib.util
import json
from pathlib import Path
import unittest

SPEC = importlib.util.spec_from_file_location('mcdc_evidence', Path(__file__).with_name('verify.py'))
V = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(V)


class Evidence(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.configuration = V.inventory()
        cls.entries = {entry['id']: entry for entry in cls.configuration['collections']}
        cls.base = cls.entries['baseline-ext-on-reuse-on']
        cls.focused = cls.entries['focused-v3']
        cls.data = V.load(cls.base)
        cls.focused_data = V.load(cls.focused)
        cls.base_proof = V.source_proof(cls.configuration, cls.base['source_revision'])
        cls.focused_proof = V.source_proof(cls.configuration, cls.focused['source_revision'])

    def changed(self, original, name, data):
        payloads = dict(original)
        payloads[name] = data
        manifest = json.loads(payloads['_archive_manifest.json'])
        manifest['files'][name] = dict(sha256=V.sha(data), bytes=len(data))
        payloads['_archive_manifest.json'] = json.dumps(manifest).encode()
        return payloads

    def test_passing_baseline_uses_separate_scopes(self):
        result = V.verify(self.data, self.base, self.base_proof)
        self.assertNotEqual(result['broad'], result['application_only'])
        self.assertEqual(result['broad']['mcdc']['covered'], 221)

    def test_focused_outcomes_are_not_application_percentages(self):
        result = V.verify(self.focused_data, self.focused, self.focused_proof)
        self.assertFalse(result['application_coverage'])
        self.assertEqual(result['cases'], 26)
        self.assertTrue(all(row['capacity_rejections'] > 0 for row in result['widths'].values()))

    def test_duplicate_profile_cannot_hide_missing_obligations(self):
        lines = self.data['profiles.txt'].decode().splitlines()
        lines[-1] = lines[0]
        payloads = self.changed(self.data, 'profiles.txt', ('\n'.join(lines) + '\n').encode())
        with self.assertRaisesRegex(ValueError, 'profile inventory differs'):
            V.verify(payloads, self.base, self.base_proof)

    def test_missing_raw_profile_is_rejected(self):
        payloads = dict(self.data)
        name = next(name for name in payloads if name.startswith('test-profiles/'))
        del payloads[name]
        with self.assertRaisesRegex(ValueError, 'payload inventory differs'):
            V.verify(payloads, self.base, self.base_proof)

    def test_modes_cannot_be_pooled(self):
        entry = dict(self.base, prepared_divisors='OFF', id='baseline-ext-on-reuse-off')
        with self.assertRaisesRegex(ValueError, 'baseline modes differ'):
            V.verify(self.data, entry, self.base_proof)

    def test_compilation_must_match_requested_mode(self):
        data = self.data['build/compile_commands.json'].replace(b'GD_BC_PREPARED_DIVISORS=1', b'GD_BC_PREPARED_DIVISORS=0')
        payloads = self.changed(self.data, 'build/compile_commands.json', data)
        with self.assertRaisesRegex(ValueError, 'compiled activation differs'):
            V.verify(payloads, self.base, self.base_proof)

    def test_undefined_activation_is_rejected(self):
        commands = json.loads(self.data['build/compile_commands.json'])
        for row in commands:
            if row['file'].endswith('/src/bc/vm.cc'):
                row['command'] += ' -UGD_BC_PREPARED_DIVISORS'
        payloads = self.changed(self.data, 'build/compile_commands.json', json.dumps(commands).encode())
        with self.assertRaisesRegex(ValueError, 'compiled activation differs'):
            V.verify(payloads, self.base, self.base_proof)

    def test_modified_source_fails_even_with_updated_payload_hash(self):
        original = self.focused_data['source.tar']
        changed = original.replace(b'class BasicDigits', b'class BasicDigitx', 1)
        self.assertNotEqual(original, changed)
        payloads = self.changed(self.focused_data, 'source.tar', changed)
        with self.assertRaisesRegex(ValueError, 'source blob identity differs'):
            V.verify(payloads, self.focused, self.focused_proof)

    def test_forged_commit_label_is_rejected(self):
        proof = copy.deepcopy(self.focused_proof)
        proof['commit'] = proof['commit'][:-8] + 'AAAAAAAA'
        with self.assertRaisesRegex(ValueError, 'source commit identity differs'):
            V.verify_source(self.focused_data['source.tar'], proof)

    def test_changed_machine_code_is_rejected(self):
        binary = bytearray(self.focused_data['test-libbc']);binary[-1] ^= 1
        payloads = self.changed(self.focused_data, 'test-libbc', bytes(binary))
        with self.assertRaisesRegex(ValueError, 'binary identity differs'):
            V.verify(payloads, self.focused, self.focused_proof)

    def test_false_llvm_totals_are_rejected(self):
        exported = json.loads(self.data['coverage.json'])
        exported['data'][0]['totals']['mcdc']['covered'] = 280
        payloads = self.changed(self.data, 'coverage.json', json.dumps(exported).encode())
        with self.assertRaisesRegex(ValueError, 'LLVM export/summary differs'):
            V.verify(payloads, self.base, self.base_proof)

    def test_rejection_branch_cannot_be_hidden(self):
        exported = json.loads(self.focused_data['coverage.json'])
        function = next(f for f in exported['data'][0]['functions'] if 'within_budget' in f['name'])
        branch = next(b for b in function['branches'] if b[3] - b[1] > 1)
        branch[4] = 0
        payloads = self.changed(self.focused_data, 'coverage.json', json.dumps(exported).encode())
        with self.assertRaisesRegex(ValueError, 'focused branch outcome absent'):
            V.verify(payloads, self.focused, self.focused_proof)

    def test_incomplete_focused_iteration_cannot_be_promoted(self):
        entry = self.entries['focused-v1']
        payloads = V.load(entry)
        proof = V.source_proof(self.configuration, entry['source_revision'])
        with self.assertRaisesRegex(ValueError, 'focused eligibility differs'):
            V.verify(payloads, dict(entry, eligible=True), proof)

    def test_failed_baseline_cannot_be_promoted(self):
        entry = self.entries['failed-ext-on-reuse-on']
        with self.assertRaisesRegex(ValueError, 'baseline eligibility differs'):
            V.verify_baseline({}, dict(entry, eligible=True))

    def test_incomplete_mode_matrix_is_rejected(self):
        configuration = copy.deepcopy(self.configuration)
        configuration['collections'] = configuration['collections'][:-1]
        with self.assertRaisesRegex(ValueError, 'collection inventory differs'):
            V.inventory(configuration)

    def test_false_full_coverage_or_adoption_claim_is_rejected(self):
        for key in ('coverage_goal_achieved', 'performance_acceptance'):
            configuration = dict(self.configuration, **{key: True})
            with self.assertRaisesRegex(ValueError, 'do not establish full coverage or adoption'):
                V.inventory(configuration)


if __name__ == '__main__':
    unittest.main()
