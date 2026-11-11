#!/usr/bin/env python3
"""Check qualification preflight without executing a benchmark."""
# SPDX-License-Identifier: Apache-2.0
import hashlib
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
            bridge = HERE / 'candidate-preflight.py'
            self.assertEqual(plan['harness_files']['src/bc/test-math-library-cli.py'],
                             hashlib.sha256(bridge.read_bytes()).hexdigest())

    def test_preflight_runs_the_candidate_tests_with_the_original_arguments(self):
        with tempfile.TemporaryDirectory() as temporary:
            source = Path(temporary) / 'candidate source'
            source.mkdir()
            library = source / 'math-library.bc'
            library.write_text('scale=20\n', encoding='utf-8')
            receipt = source / 'argv.json'
            (source / 'test-math-library-cli.py').write_text(
                "import json, sys\nfrom pathlib import Path\n"
                "Path(__file__).with_name('argv.json').write_text(json.dumps(sys.argv[1:]))\n"
                "raise SystemExit(7)\n", encoding='utf-8')
            arguments = ['candidate binary', '--source', str(library), '--other-bc', 'Howard binary']
            result = subprocess.run([sys.executable, str(HERE / 'candidate-preflight.py'),
                                     *arguments], cwd=ROOT)
            self.assertEqual(result.returncode, 7)
            self.assertEqual(json.loads(receipt.read_text(encoding='utf-8')), arguments)


if __name__ == '__main__':
    unittest.main()
