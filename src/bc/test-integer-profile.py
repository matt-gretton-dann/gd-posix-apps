#!/usr/bin/env python3
"""Prove diagnostic builds execute and account for the selected integer tier."""
import argparse
import json
import math
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


class IntegerProfile(unittest.TestCase):
    def profile(self, text, expected):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / 'integer-profile.json'
            environment = {k: v for k, v in os.environ.items()
                           if not k.startswith(('BC_', 'DC_', 'GD_BC_PROFILE_'))}
            environment.update(LC_ALL='C', GD_BC_PROFILE_OUTPUT=str(output))
            run = subprocess.run([BC], input=text, universal_newlines=True,
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                 timeout=10, env=environment)
            self.assertEqual((run.returncode, run.stdout, run.stderr), (0, expected, ''))
            data = json.loads(output.read_text())
        self.assertTrue(data['instrumented'])
        self.assertEqual(data['total']['live_bytes'], 0)
        for allocation in data['allocations'].values():
            self.assertEqual(allocation['live_bytes'], 0)
        return data

    def test_hot_calls_execute_the_selected_tier(self):
        data = self.profile('define f(x) {\nreturn (x*x+1)\n}\n'
                            's=0\nfor(i=0;i<20;i++) s+=f(i)\ns\n',
                            str(sum(i * i + 1 for i in range(20))) + '\n')
        events = data['events']
        self.assertGreater(events.get('integer_function_attempts', 0), 0)
        self.assertEqual(events['integer_function_successes'], events['integer_function_attempts'])
        self.assertEqual(events['integer_function_abandonments'], 0)
        self.assertGreater(events['integer_function_instructions'], 0)
        self.assertEqual(events['integer_function_promotions'], 0)
        self.assertGreater(data['allocations']['frames']['allocated_bytes'], 0)
        self.assertGreater(data['allocations']['execution_plans']['allocated_bytes'], 0)

    def test_exact_large_results_report_promotions_and_self_calls(self):
        data = self.profile('define f(x) {\nif(x<=1) return (1)\nreturn (x*f(x-1))\n}\n'
                            'for(i=0;i<12;i++) a=f(50)\na\n', str(math.factorial(50)) + '\n')
        events = data['events']
        self.assertGreater(events.get('integer_function_successes', 0), 0)
        self.assertGreater(events['integer_function_self_calls'], 0)
        self.assertGreater(events['integer_function_promotions'], 0)
        self.assertEqual(events['integer_function_abandonments'], 0)

    def test_effectful_functions_remain_on_the_generic_path(self):
        data = self.profile('define f(x) {\ns=s+1\nreturn (x)\n}\n'
                            'for(i=0;i<20;i++) a=f(i)\ns\n', '20\n')
        self.assertEqual(data['events'].get('integer_function_attempts', 0), 0)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('bc')
    args, remaining = parser.parse_known_args()
    BC = args.bc
    unittest.main(argv=[__file__, *remaining])
