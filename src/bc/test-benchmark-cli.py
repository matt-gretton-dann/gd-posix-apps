#!/usr/bin/env python3
"""Check bounded benchmark execution, output validation and paired statistics."""

import argparse
import contextlib
import importlib.util
import io
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


class BenchmarkCli(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.driver = self.root / "fake bc's tool.py"
        self.driver.write_text(
            "import sys, time\n"
            "mode = sys.argv[1]\n"
            "source = sys.stdin.read()\n"
            "if mode == 'timeout': time.sleep(10)\n"
            "if mode == 'exit':\n"
            "    sys.stderr.write('deliberate child failure\\n')\n"
            "    sys.exit(23)\n"
            "sys.stdout.write('3\\n' if mode == 'wrong' else '2\\n')\n"
            "if mode == 'stderr': sys.stderr.write('unexpected warning\\n')\n",
            encoding="utf-8")

    def invoke(self, mode, timeout=3, duplicate=False):
        implementation = {"name": "fake", "argv": [sys.executable, str(self.driver), mode]}
        config = self.root / "implementations.json"
        config.write_text(json.dumps({"implementations":
                                     [implementation, implementation] if duplicate else
                                     [implementation]}), encoding="utf-8")
        output = self.root / "results"
        try:
            result = subprocess.run(
                [sys.executable, BENCHMARK, "--config", str(config), "--output", str(output),
                 "--workload", "startup", "--samples", "1", "--warmups", "0",
                 "--timeout", str(timeout)], stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                universal_newlines=True, timeout=8)
        except subprocess.TimeoutExpired:
            self.fail("benchmark did not terminate within the test's outer deadline")
        return result, output

    def test_checked_success_and_quoted_argv(self):
        result, output = self.invoke("ok")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stderr, "")
        summary = json.loads((output / "summary.json").read_text(encoding="utf-8"))
        self.assertEqual(summary["workloads"][0]["name"], "startup")
        self.assertEqual(summary["workloads"][0]["implementations"]["fake"]["samples"], 1)
        rows = [json.loads(line) for line in
                (output / "samples.jsonl").read_text(encoding="utf-8").splitlines()]
        self.assertEqual(len(rows), 1)
        self.assertTrue(rows[0]["checked"])
        self.assertGreater(rows[0]["seconds"], 0)
        manifest = json.loads((output / "manifest.json").read_text(encoding="utf-8"))
        self.assertEqual(len(manifest["implementations"][0]["binary_sha256"]), 64)

    def check_failure(self, mode, diagnostic, timeout=3):
        result, output = self.invoke(mode, timeout)
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertIn(diagnostic, result.stderr)
        failure = json.loads((output / "failure.json").read_text(encoding="utf-8"))
        self.assertIn(diagnostic, failure["error"])

    def test_reject_child_failure(self):
        self.check_failure("exit", "exited with status 23")

    def test_reject_incorrect_output(self):
        self.check_failure("wrong", "incorrect output")

    def test_reject_stderr(self):
        self.check_failure("stderr", "wrote to stderr")

    def test_timeout_is_enforced(self):
        self.check_failure("timeout", "timed out", timeout=0.05)

    def test_duplicate_implementations_are_rejected(self):
        result, _ = self.invoke("ok", duplicate=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("duplicate implementation", result.stderr)


class PairedStatistics(unittest.TestCase):
    def module(self):
        spec = importlib.util.spec_from_file_location("bc_benchmark", BENCHMARK)
        module = importlib.util.module_from_spec(spec)
        try:
            with contextlib.redirect_stderr(io.StringIO()):
                spec.loader.exec_module(module)
        except SystemExit:
            self.fail("benchmark module starts its CLI while being imported")
        return module

    def test_exact_paired_ratio_and_replay(self):
        module = self.module()
        result = module.paired_statistics([2, 4, 6], [1, 2, 3], seed=197200, resamples=200)
        self.assertEqual(result["median_paired_ratio"], 2)
        self.assertEqual(result["ci95"], [2, 2])
        self.assertEqual(result, module.paired_statistics(
            [2, 4, 6], [1, 2, 3], seed=197200, resamples=200))

    def test_single_pair_has_no_confidence_claim(self):
        result = self.module().paired_statistics([2], [1], seed=197200, resamples=200)
        self.assertEqual(result["median_paired_ratio"], 2)
        self.assertIsNone(result["ci95"])
        self.assertEqual(result["pairs"], 1)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("benchmark")
    args, remaining = parser.parse_known_args()
    BENCHMARK = str(Path(args.benchmark).resolve())
    unittest.main(argv=[__file__, *remaining])
