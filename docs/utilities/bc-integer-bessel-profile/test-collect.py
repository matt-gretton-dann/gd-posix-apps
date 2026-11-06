#!/usr/bin/env python3
"""Validate frozen replay and diagnostic counter checks without native sampling."""
import argparse
import copy
import importlib.util
import json
import os
from pathlib import Path
import shutil
import tempfile
import unittest
from unittest import mock

HERE = Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location('bessel_attribution', str(HERE / 'collect.py'))
COLLECT = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(COLLECT)


class Attribution(unittest.TestCase):
    def test_exact_selected_inputs_keep_their_calibrated_counts(self):
        bench = COLLECT.load(SCRIPTS, 'bench-bc')
        cases, _, provenance = COLLECT.frozen_cases(bench, HERE)
        self.assertEqual(len(cases), 2)
        self.assertEqual({case['parameters']['iterations'] for case in cases}, {16384})
        self.assertEqual({case['parameters']['scale'] for case in cases}, {20})
        self.assertEqual([case['name'] for case in cases], provenance['workloads'])

    def test_changed_frozen_oracle_is_rejected(self):
        bench = COLLECT.load(SCRIPTS, 'bench-bc')
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            shutil.copytree(str(HERE / 'frozen'), str(root / 'frozen'))
            shutil.copyfile(HERE / 'provenance.json', root / 'provenance.json')
            oracle = next((root / 'frozen/workloads').glob('*.expected'))
            oracle.write_bytes(oracle.read_bytes() + b'\n')
            with self.assertRaisesRegex(ValueError, 'frozen input changed'):
                COLLECT.frozen_cases(bench, root)

    def counter_check(self, counters, enabled=True, instrumented=True):
        bench = mock.Mock()
        bench.digest_file.return_value = 'binary-hash'
        tool = dict(argv=['unused-test-binary'], metadata=dict(
            instrumented=instrumented, binary_sha256='binary-hash'))
        with tempfile.TemporaryDirectory() as temporary:
            destination = Path(temporary) / 'counters.json'

            def checked(*args):
                Path(os.environ['GD_BC_PROFILE_OUTPUT']).write_text(json.dumps(counters))
                return dict(seconds=123, checked=True, output_sha256='checked-output')

            bench.run_checked.side_effect = checked
            with mock.patch.dict(os.environ, GD_BC_PROFILE_OUTPUT='previous-value'):
                try:
                    return COLLECT.software_profile(bench, tool, {}, destination, enabled)
                finally:
                    self.assertEqual(os.environ['GD_BC_PROFILE_OUTPUT'], 'previous-value')

    def counters(self):
        return dict(instrumented=True, total=dict(live_bytes=0),
                    allocations=dict(frames=dict(live_bytes=0)), events=dict(
                        integer_function_attempts=3, integer_function_successes=2,
                        integer_function_abandonments=1))

    def test_success_retains_counts_without_instrumented_elapsed_values(self):
        result = self.counter_check(self.counters())
        self.assertTrue(result['checked'])
        self.assertNotIn('seconds', result)

    def test_invalid_or_live_counters_are_rejected_and_environment_restored(self):
        for field in ('instrumented', 'total', 'allocations', 'events'):
            counters = copy.deepcopy(self.counters())
            if field == 'instrumented':
                counters[field] = False
            elif field == 'total':
                counters[field]['live_bytes'] = 1
            elif field == 'allocations':
                counters[field]['frames']['live_bytes'] = 1
            else:
                counters[field]['integer_function_successes'] = 0
            with self.subTest(field=field), self.assertRaises(ValueError):
                self.counter_check(counters)

    def test_off_control_rejects_integer_execution(self):
        with self.assertRaisesRegex(ValueError, 'OFF control'):
            self.counter_check(self.counters(), enabled=False)

    def test_release_binary_cannot_supply_software_counters(self):
        with self.assertRaisesRegex(ValueError, 'instrumented binary'):
            self.counter_check(self.counters(), instrumented=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--scripts', type=Path, required=True)
    ARGS, remaining = parser.parse_known_args()
    SCRIPTS = ARGS.scripts.resolve()
    unittest.main(argv=[__file__, *remaining])
