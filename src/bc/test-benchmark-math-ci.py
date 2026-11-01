#!/usr/bin/env python3
"""Check native math roles, acceptance limits and process-tree deadlines."""
import copy
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

PATH = Path(__file__).resolve().parent / 'scripts/benchmark-math-ci.py'
SPEC = importlib.util.spec_from_file_location('benchmark_math_ci', str(PATH))
CI = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CI)


class NativeMath(unittest.TestCase):
    def test_every_regime_and_mode_has_a_prespecified_representative(self):
        bench = CI.load('bench-bc')
        for preset in ('full', 'holdout'):
            selected = set(CI.selected_cases(bench, preset, False))
            rows = [row for row in bench.corpus(preset, suite='math') if row['name'] in selected]
            self.assertEqual(len(rows), 20)
            self.assertEqual(len({(row['group'], row['parameters']['mode']) for row in rows}), 20)
            self.assertTrue(all(row['parameters']['scale'] == 100 for row in rows))
            self.assertEqual(len(CI.selected_cases(bench, preset, True)), 60)

    def test_roles_pin_both_host_and_library_without_mutating_recipes(self):
        with tempfile.TemporaryDirectory() as temporary:
            library = Path(temporary) / 'library.bc'
            other = Path(temporary) / 'other.bc'
            library.write_bytes(b'scale=20\n');other.write_bytes(library.read_bytes())
            revision = 'a' * 40
            gd = dict(name='gd', argv=['gd-binary'], metadata=dict(source_revision=revision))
            aa = copy.deepcopy(gd)
            howard = dict(name='howard', argv=['howard-binary'], metadata=dict(
                source_revision=CI.load('build-bc-comparators').HOWARD_REVISION))
            original = copy.deepcopy((gd, aa, howard))
            configs = CI.configurations(gd, aa, howard, library, other, revision)
            self.assertEqual((gd, aa, howard), original)
            self.assertEqual([len(configs[key]) for key in ('preparation', 'paired', 'aa')], [4, 3, 2])
            standard, candidate, portable = configs['paired']
            self.assertEqual(standard['argv'][-1], '-l')
            self.assertEqual(candidate['argv'][-1], '-l')
            self.assertEqual(portable['argv'], ['howard-binary', str(library)])
            self.assertEqual(portable['input_artifacts'][0]['sha256'],
                             candidate['metadata']['library']['source_sha256'])
            other.write_bytes(b'scale=19\n')
            with self.assertRaisesRegex(CI.CommandFailure, 'identical'):
                CI.configurations(gd, aa, howard, library, other, revision)
            other.write_bytes(library.read_bytes())
            howard['metadata']['source_revision'] = 'b' * 40
            with self.assertRaisesRegex(CI.CommandFailure, 'pinned'):
                CI.configurations(gd, aa, howard, library, other, revision)

    def test_control_bias_needs_same_direction_confirmation(self):
        high, low = dict(ci95=[1.1, 1.2]), dict(ci95=[.8, .9])
        self.assertEqual(CI.aa_status(high), 'inconclusive')
        self.assertEqual(CI.aa_status(high, low), 'inconclusive')
        self.assertEqual(CI.aa_status(high, high), 'confirmed A/A bias')
        self.assertEqual(CI.aa_status(low, low), 'confirmed A/A bias')
        self.assertEqual(CI.aa_status(dict(ci95=[.98, 1.02])), 'pass')
        self.assertEqual(CI.aa_status(dict(ci95=[.9, 1.1])), 'inconclusive')
        self.assertEqual(CI.aa_status({}), 'inconclusive')

    def test_goal_requires_checked_duration_and_preserves_inconclusive_controls(self):
        row = dict(parameters=dict(mode='repeated'), duration_target_met=dict(gd=True, howard=True),
                   candidate_over_reference={'gd-portable': dict(median_paired_ratio=.89, ci95=[.85, .95])})
        self.assertEqual(CI.goal_status(row, 'pass'), 'target observed')
        self.assertEqual(CI.goal_status(row, 'inconclusive'), 'target observed; A/A inconclusive')
        self.assertEqual(CI.goal_status(row, 'confirmed A/A bias'), 'inconclusive control bias')
        row['candidate_over_reference']['gd-portable']['median_paired_ratio'] = .91
        self.assertEqual(CI.goal_status(row, 'pass'), 'target not established')
        for duration in ({}, dict(gd=False, howard=True)):
            row['duration_target_met'] = duration
            self.assertEqual(CI.goal_status(row, 'pass'), 'insufficient measured duration')
        del row['duration_target_met']
        self.assertEqual(CI.goal_status(row, 'pass'), 'insufficient measured duration')
        row['parameters']['mode'] = 'one_shot'
        self.assertEqual(CI.goal_status(row, 'pass'), 'one-shot latency observation')

    @unittest.skipUnless(os.name in ('posix', 'nt'), 'tree termination needs a native supported API')
    def test_timeout_reaps_descendants_with_inherited_output_pipes(self):
        # Run the timeout handler under another bounded process: a surviving
        # grandchild would otherwise hold communicate() open past the deadline.
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            ready = root / 'ready'
            grandchild = 'from pathlib import Path;import time;Path(' + repr(str(ready)) + ').touch();time.sleep(30)'
            child = 'import subprocess,sys;subprocess.run([sys.executable,"-c",' + repr(grandchild) + '])'
            outer = ('import importlib.util,pathlib;'
                     's=importlib.util.spec_from_file_location("ci",' + repr(str(PATH)) + ');'
                     'm=importlib.util.module_from_spec(s);s.loader.exec_module(m);'
                     'm.Commands(pathlib.Path(' + repr(str(root)) + '),10).run(' +
                     repr([sys.executable, '-c', child]) + ',timeout=2)')
            result = subprocess.run([sys.executable, '-c', outer], stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE, timeout=12)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn(b'exceeded its deadline', result.stderr)
            self.assertTrue(ready.exists(), 'grandchild must actually start before the deadline')
            self.assertTrue((root / 'command-001.log').is_file())

    def test_missing_library_emits_unavailable_evidence_without_acceptance(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            original_load = CI.load
            # This checks unavailable-source evidence, not machine inventory.
            # Native workflow artifacts exercise the real bounded host probes;
            # a Windows CIM query can itself take longer than this unit test.
            def load(name):
                if name == 'benchmark-ci':
                    return mock.Mock(host_info=lambda: dict(test_host='isolated fixture'))
                return original_load(name)
            argv = [str(PATH), '--source', str(root), '--candidate', 'a' * 40,
                    '--output', str(root / 'results')]
            with mock.patch.object(CI, 'load', side_effect=load), mock.patch.object(sys, 'argv', argv):
                self.assertEqual(CI.main(), 0)
            evidence = root / 'results/evidence'
            self.assertFalse(json.loads((evidence / 'unavailable.json').read_text())['performance_acceptance'])
            self.assertFalse((evidence / 'decisions.json').exists())
            self.assertEqual(json.loads((evidence / 'orchestration.json').read_text())['commands'], [])


if __name__ == '__main__':
    unittest.main()
