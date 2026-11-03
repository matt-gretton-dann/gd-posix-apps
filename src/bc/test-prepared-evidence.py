#!/usr/bin/env python3
"""Check that retained component evidence cannot silently lose adverse samples."""
import copy
import importlib.util
import json
from pathlib import Path
import unittest

HERE = Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location(
    'prepared_evidence', HERE / 'scripts/verify-prepared-components.py')
VERIFIER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(VERIFIER)


class Evidence(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.payloads, cls.summary = VERIFIER.load(
            HERE.parents[1] / 'docs/utilities/bc-prepared-divisors')

    def test_complete_rss_grid(self):
        payloads, summary = VERIFIER.load(HERE.parents[1] / 'docs/utilities/bc-prepared-divisors',
                                         'rss-v1')
        self.assertEqual(VERIFIER.verify_rss(payloads, summary, self.payloads),
                         {'rss_checked': 216, 'adoption_accepted': False})

    def test_duplicate_rss_sample(self):
        payloads, summary = VERIFIER.load(HERE.parents[1] / 'docs/utilities/bc-prepared-divisors',
                                         'rss-v1')
        manifest = json.loads(payloads['manifest.json'])
        manifest['observations'][-1] = copy.deepcopy(manifest['observations'][-2])
        payloads['manifest.json'] = json.dumps(manifest).encode()
        with self.assertRaisesRegex(ValueError, 'duplicate RSS'):
            VERIFIER.verify_rss(payloads, summary, self.payloads)

    def changed_manifest(self, change):
        payloads = dict(self.payloads)
        manifest = json.loads(payloads['manifest.json'])
        change(manifest)
        payloads['manifest.json'] = json.dumps(manifest).encode()
        return payloads

    def test_complete_grid(self):
        result = VERIFIER.verify(self.payloads, self.summary)
        self.assertEqual(result, {'profile_checked': 72, 'timing_checked': 576,
                                  'adoption_accepted': False})

    def test_duplicate_replacing_missing_sample(self):
        payloads = self.changed_manifest(lambda m: m['observations'].__setitem__(
            -1, copy.deepcopy(m['observations'][-2])))
        with self.assertRaisesRegex(ValueError, 'duplicate'):
            VERIFIER.verify(payloads, self.summary)

    def test_changed_executable(self):
        payloads = dict(self.payloads)
        binary = bytearray(payloads['timing/bc-components'])
        binary[len(binary) // 2] ^= 1
        payloads['timing/bc-components'] = bytes(binary)
        with self.assertRaisesRegex(ValueError, 'binary/recipe hash'):
            VERIFIER.verify(payloads, self.summary)

    def test_changed_oracle(self):
        payloads = self.changed_manifest(lambda m: m['cases'][0].__setitem__('quotient', '1'))
        with self.assertRaisesRegex(ValueError, 'independent integer oracle'):
            VERIFIER.verify(payloads, self.summary)

    def test_no_adoption_from_component_results(self):
        summary = dict(self.summary, adoption_accepted=True)
        with self.assertRaisesRegex(ValueError, 'do not establish adoption'):
            VERIFIER.verify(self.payloads, summary)

    def test_adverse_ratio_cannot_be_replaced(self):
        summary = copy.deepcopy(self.summary)
        # Keep the largest held-out quotient's measured slowdown in the summary.
        row = next(r for r in summary['median_ratios'] if r['case'] == 'held-out-2-leading-9')
        self.assertGreater(row['quotient_median_ratio'], 1)
        row['quotient_median_ratio'] = 0.9
        with self.assertRaisesRegex(ValueError, 'reported median ratio'):
            VERIFIER.verify(self.payloads, summary)


if __name__ == '__main__':
    unittest.main()
