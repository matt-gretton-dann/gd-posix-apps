#!/usr/bin/env python3
"""Check integer resource diagnostics without running timed workloads.

SPDX-License-Identifier: Apache-2.0
"""
import importlib.util
import json
import os
from pathlib import Path
import tempfile
import types
import unittest
from unittest.mock import patch

SCRIPTS = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location('integer_resources', str(SCRIPTS / 'collect-integer-resources.py'))
COLLECTOR = importlib.util.module_from_spec(spec)
spec.loader.exec_module(COLLECTOR)


class ResourceTests(unittest.TestCase):
    def test_real_corpus_selects_both_target_classes_and_startup(self):
        bench = COLLECTOR.load(SCRIPTS, 'bench-bc')
        for preset in ('full', 'holdout'):
            cases = COLLECTOR.selected_cases(bench, preset)
            self.assertEqual({row['group'] for row in cases}, {'startup', 'calls', 'recursion'})
            self.assertEqual(len(cases), 3)
            self.assertTrue(all(bench.oracle_payload(row) for row in cases))

    def collect(self, counters=None, group='calls', enabled=True, instrumented=True,
                changed=False, child_error=False):
        if counters is None:
            counters = dict(instrumented=True, total=dict(live_bytes=0),
                            allocations={'frames': dict(live_bytes=0)},
                            events=dict(integer_function_attempts=3, integer_function_successes=3,
                                        integer_function_abandonments=0))
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary) / 'counters.json'
            def checked(tool, case, timeout):
                self.assertEqual(os.environ['GD_BC_PROFILE_OUTPUT'], str(output))
                if child_error:
                    raise RuntimeError('wrong output')
                output.write_text(json.dumps(counters), encoding='utf-8')
                return dict(seconds=42, checked=True)
            bench = types.SimpleNamespace(digest_file=lambda path: 'changed' if changed else 'hash',
                                          run_checked=checked)
            tool = dict(argv=['binary'], metadata=dict(instrumented=instrumented, binary_sha256='hash'))
            with patch.dict(os.environ, {'GD_BC_PROFILE_OUTPUT': 'previous'}):
                try:
                    return COLLECTOR.profile_record(bench, tool, {'group': group}, output, enabled, 1)
                finally:
                    self.assertEqual(os.environ['GD_BC_PROFILE_OUTPUT'], 'previous')

    def test_success_keeps_checked_data_and_discards_instrumented_time(self):
        self.assertEqual(self.collect(), {'counters': 'counters.json', 'checked': True})

    def test_release_or_changed_profile_binary_is_rejected(self):
        for arguments in ({'instrumented': False}, {'changed': True}):
            with self.assertRaises(ValueError):
                self.collect(**arguments)

    def test_each_live_category_is_checked_even_if_total_claims_zero(self):
        counters = dict(instrumented=True, total=dict(live_bytes=0),
                        allocations={'frames': dict(live_bytes=1)}, events={})
        with self.assertRaises(ValueError):
            self.collect(counters)

    def test_fallback_and_missing_execution_are_rejected(self):
        for attempts, successes, abandoned in ((0, 0, 0), (3, 2, 1), (3, 2, 0)):
            counters = dict(instrumented=True, total=dict(live_bytes=0), allocations={},
                            events=dict(integer_function_attempts=attempts,
                                        integer_function_successes=successes,
                                        integer_function_abandonments=abandoned))
            with self.assertRaises(ValueError):
                self.collect(counters)

    def test_control_cannot_hide_integer_execution(self):
        for arguments in ({'group': 'startup'}, {'enabled': False}):
            with self.assertRaises(ValueError):
                self.collect(**arguments)

    def test_controls_allow_zero_attempts(self):
        counters = dict(instrumented=True, total=dict(live_bytes=0), allocations={},
                        events=dict(integer_function_attempts=0))
        self.assertTrue(self.collect(counters, enabled=False)['checked'])
        self.assertTrue(self.collect(counters, group='startup')['checked'])

    def test_child_failure_restores_profile_environment(self):
        with self.assertRaisesRegex(RuntimeError, 'wrong output'):
            self.collect(child_error=True)


if __name__ == '__main__':
    unittest.main()
