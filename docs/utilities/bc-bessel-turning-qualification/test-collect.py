#!/usr/bin/env python3
"""Check qualification preflight without executing a benchmark."""
# SPDX-License-Identifier: Apache-2.0
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]


class QualificationPlan(unittest.TestCase):
    def test_math_plan_includes_the_cli_preflight(self):
        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary) / 'plan'
            subprocess.run([sys.executable, str(HERE / 'collect.py'), '--suite',
                            'math-full-20', '--output', str(output), '--plan-only'],
                           cwd=ROOT, check=True)
            plan = json.loads((output / 'plan.json').read_text(encoding='utf-8'))
            self.assertTrue('src/bc/test-math-library-cli.py' in plan['harness_files'],
                            'the math CLI preflight is missing from the qualification plan')


if __name__ == '__main__':
    unittest.main()
