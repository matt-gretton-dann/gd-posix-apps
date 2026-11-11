#!/usr/bin/env python3
"""Exercise the crossed deployment with synthetic executables."""
# SPDX-License-Identifier: Apache-2.0
import hashlib
import json
from pathlib import Path
import tempfile
import unittest
from unittest import mock

import collect


class DeploymentTests(unittest.TestCase):
    def test_balanced_assignments(self):
        schedule = list(collect.assignments())
        self.assertEqual(len(schedule), 20)
        self.assertEqual(schedule, list(collect.assignments()))
        for pair in zip(schedule[::2], schedule[1::2]):
            self.assertEqual(set(pair), {0, 1})

    def test_swap_reuses_both_paths_and_preserves_binary_identity(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            originals = {}
            startup = collect.load('startup-calibration')
            for role in collect.BUILDS:
                path = root / role
                payload = role.encode()
                path.write_bytes(payload)
                originals[role] = dict(name=role, argv=[str(path), '-q'],
                    metadata=dict(binary_sha256=hashlib.sha256(payload).hexdigest(),
                                  instrumented=False))
            first, first_sites = collect.deploy(originals, root, 0)
            second, second_sites = collect.deploy(originals, root, 1)
            self.assertEqual(first_sites['base'], second_sites['aa'])
            self.assertEqual(first_sites['aa'], second_sites['base'])
            self.assertEqual(first[0]['argv'], second[2]['argv'])
            self.assertEqual(second[0]['argv'], second[1]['argv'])
            for tool in second:
                startup.check_binary(tool)
                self.assertEqual(tool['argv'][1:], ['-q'])
            self.assertEqual(Path(originals['base']['argv'][0]).read_bytes(), b'base')

    def test_synthetic_children_keep_each_deployment_and_sample(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            output = root / 'study'
            config = root / 'config.json'
            config.write_text('{}')
            bench = collect.load('bench-bc')
            startup = collect.load('startup-calibration')
            tools = []
            for role in collect.BUILDS:
                path = root / role
                payload = role.encode()
                path.write_bytes(payload)
                metadata = dict(binary_sha256=hashlib.sha256(payload).hexdigest(),
                    instrumented=False, source_revision=collect.RUNTIME, compiler='fixture',
                    flags={'CMAKE_CXX_FLAGS': '-O2'}, version='fixture',
                    configuration={'diagnostic_profile': False})
                tools.append(dict(name=role, argv=[str(path)], metadata=metadata))
            tools.append(dict(tools[0], name='candidate'))
            with (mock.patch.object(bench, 'implementations', return_value=tools),
                  mock.patch.object(bench, 'run_checked', return_value={
                      'checked': True, 'seconds': .001}),
                  mock.patch.multiple(startup, PAIRS=3, WARMUPS=1, RESAMPLES=200)):
                status = collect.collect(config, output, bench, startup)
            result = json.loads((output / 'manifest.json').read_text())
            self.assertEqual(status, 0, result.get('error'))
            self.assertEqual(result['status'], 'collected')
            self.assertFalse(result['merge_eligible'])
            raw = (output / 'samples.jsonl').read_bytes()
            self.assertEqual(result['raw_sha256'], hashlib.sha256(raw).hexdigest())
            rows = [json.loads(line) for line in raw.splitlines()]
            self.assertEqual(len(rows), 20 * 2 * 4 * 3)
            self.assertEqual({r['implementation'] for r in rows}, {'base', 'self', 'aa'})
            for number, orientation in enumerate(collect.assignments()):
                block = result['blocks'][number]
                self.assertEqual(block['sites']['base'], collect.SITES[orientation])
                self.assertEqual(set(block['stages']), {'initial', 'confirmation'})
            for role in collect.BUILDS:
                self.assertEqual((output / 'originals' / role / 'bc.exe').read_bytes(),
                                 role.encode())

    def test_provenance_failure_keeps_manifest(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            output = root / 'study'
            config = root / 'config.json'
            config.write_text('{}')
            bench = collect.load('bench-bc')
            startup = collect.load('startup-calibration')
            with mock.patch.object(bench, 'implementations', return_value=[]):
                self.assertEqual(collect.collect(config, output, bench, startup), 1)
            result = json.loads((output / 'manifest.json').read_text())
            self.assertEqual(result['status'], 'incomplete')
            self.assertFalse(result['merge_eligible'])
            self.assertIn('exactly base', result['error'])
            self.assertFalse((output / 'samples.jsonl').exists())


if __name__ == '__main__':
    unittest.main()
