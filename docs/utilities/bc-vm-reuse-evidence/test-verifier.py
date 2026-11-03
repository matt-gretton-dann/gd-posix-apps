#!/usr/bin/env python3
"""Reject incomplete, mislabelled and favourable replacement RSS evidence."""
import copy
import importlib.util
import json
from pathlib import Path
import unittest

SPEC = importlib.util.spec_from_file_location('vm_evidence', Path(__file__).with_name('verify.py'))
V = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(V)


class Evidence(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.payloads, cls.summary = V.load()

    def changed(self, mutate):
        payloads = dict(self.payloads)
        manifest = json.loads(payloads['manifest.json'])
        mutate(manifest, payloads)
        payloads['manifest.json'] = json.dumps(manifest).encode()
        return payloads

    def replace(self, manifest, payloads, name, data):
        payloads[name] = data
        manifest['files'][name] = V.digest(data)

    def test_complete_retained_grid(self):
        self.assertEqual(V.verify(self.payloads, self.summary)['checked'], 156)

    def test_duplicate_cannot_replace_missing_sample(self):
        payloads = self.changed(lambda m, p: m['observations'].__setitem__(-1, copy.deepcopy(m['observations'][-2])))
        with self.assertRaisesRegex(ValueError, 'duplicate observation'):
            V.verify(payloads)

    def test_missing_sample_is_not_a_smaller_successful_grid(self):
        payloads = self.changed(lambda m, p: m['observations'].pop())
        with self.assertRaisesRegex(ValueError, 'incomplete observation grid'):
            V.verify(payloads)

    def test_matching_forged_oracle_and_output_still_fail(self):
        def change(m, p):
            case = m['fixtures'][0]
            case['expected'] = ['0']
            for row in m['observations']:
                if row['case'] == case['name']:
                    self.replace(m, p, row['stdout'], b'0\n')
        with self.assertRaisesRegex(ValueError, 'fixture oracle differs'):
            V.verify(self.changed(change))

    def test_one_incorrect_intermediate_checksum_fails(self):
        def change(m, p):
            row = next(row for row in m['observations'] if row['case'].startswith('divisor_periodic'))
            values = p[row['stdout']].replace(b'\\\n', b'').splitlines()
            values[-1] = b'0'
            self.replace(m, p, row['stdout'], b'\n'.join(values) + b'\n')
        with self.assertRaisesRegex(ValueError, 'observed output differs'):
            V.verify(self.changed(change))

    def test_false_activation_fails_even_with_consistent_metadata_files(self):
        def change(m, p):
            role = next(role for role in m['roles'] if role['name'] == 'on')
            role['metadata']['configuration']['prepared_divisors_policy']['enabled'] = False
            self.replace(m, p, 'on/metadata.json', json.dumps(role['metadata']).encode())
        with self.assertRaisesRegex(ValueError, 'mode activation differs'):
            V.verify(self.changed(change))

    def test_changed_executable_fails_after_payload_hash_update(self):
        def change(m, p):
            data = bytearray(p['on/bc']);data[-1] ^= 1
            self.replace(m, p, 'on/bc', bytes(data))
        with self.assertRaisesRegex(ValueError, 'binary identity differs'):
            V.verify(self.changed(change))

    def test_misreported_rss_fails(self):
        payloads = self.changed(lambda m, p: m['observations'][0].__setitem__('rss_bytes', 1))
        with self.assertRaisesRegex(ValueError, 'RSS resource differs'):
            V.verify(payloads)

    def test_changed_program_cannot_hide_a_missing_owner_change(self):
        def change(m, p):
            case = next(case for case in m['fixtures'] if case['group'] == 'divisor_changing_divide')
            case['program'] = case['program'].replace('b+=1;', '')
        with self.assertRaisesRegex(ValueError, 'admission program differs'):
            V.verify(self.changed(change))

    def test_false_adoption_is_rejected(self):
        with self.assertRaisesRegex(ValueError, 'do not establish adoption'):
            V.verify(self.payloads, dict(self.summary, adoption_accepted=True))

    def test_code_growth_cannot_be_replaced_with_a_favourable_summary(self):
        summary = copy.deepcopy(self.summary)
        row = next(row for row in summary['results']['binaries'] if row['mode'] == 'on')
        row['file_bytes'] = 1
        with self.assertRaisesRegex(ValueError, 'published observations differ'):
            V.verify(self.payloads, summary)


if __name__ == '__main__':
    unittest.main()
