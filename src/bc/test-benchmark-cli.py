#!/usr/bin/env python3
"""Check bounded benchmark execution, output validation and paired statistics."""

import argparse
import contextlib
import importlib.util
import io
import hashlib
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
        self.invocations = 0
        self.driver = self.root / "fake bc's tool.py"
        self.driver.write_text(
            "import sys, time\n"
            "from pathlib import Path\n"
            "mode = sys.argv[1]\n"
            "source = sys.stdin.read()\n"
            "if mode == 'warmup':\n"
            "    marker = Path(sys.argv[2])\n"
            "    mode = 'ok' if marker.exists() else 'wrong'\n"
            "    marker.write_text('used')\n"
            "if mode == 'timeout': time.sleep(10)\n"
            "if mode == 'exit':\n"
            "    sys.stderr.write('deliberate child failure\\n')\n"
            "    sys.exit(23)\n"
            "sys.stdout.write('3\\n' if mode == 'wrong' else '2\\n')\n"
            "if mode == 'stderr': sys.stderr.write('unexpected warning\\n')\n",
            encoding="utf-8")

    def invoke(self, mode, timeout=3, duplicate=False, names=("fake",), samples=1,
               warmups=0, extra=(), metadata=None):
        entries = [{"name": name, "argv": [sys.executable, str(self.driver), mode,
                                           str(self.root / "marker")],
                    "metadata": metadata or {}} for name in names]
        implementation = entries[0]
        config = self.root / "implementations.json"
        config.write_text(json.dumps({"implementations":
                                     [implementation, implementation] if duplicate else
                                     entries}), encoding="utf-8")
        self.invocations += 1
        output = self.root / ("results-" + str(self.invocations))
        try:
            result = subprocess.run(
                [sys.executable, BENCHMARK, "--config", str(config), "--output", str(output),
                 "--workload", "startup", "--samples", str(samples), "--warmups", str(warmups),
                 "--timeout", str(timeout)] + list(extra),
                stdout=subprocess.PIPE, stderr=subprocess.PIPE,
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

    def test_native_quick_corpus(self):
        if BC_BINARY is None:
            self.skipTest("native executable not supplied")
        config = self.root / "native.json"
        config.write_text(json.dumps({"implementations": [{"name": "gd", "argv": [BC_BINARY]}]}),
                          encoding="utf-8")
        output = self.root / "native-results"
        try:
            result = subprocess.run(
                [sys.executable, BENCHMARK, "--config", str(config), "--output", str(output),
                 "--preset", "quick", "--samples", "1", "--warmups", "0", "--timeout", "5"],
                stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True, timeout=20)
        except subprocess.TimeoutExpired:
            self.fail("native corpus exceeded the outer test deadline")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stderr, "")
        rows = [json.loads(line) for line in
                (output / "samples.jsonl").read_text(encoding="utf-8").splitlines()]
        self.assertEqual(len(rows), 33)
        self.assertTrue(all(row["checked"] for row in rows))

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

    def test_warmup_output_is_also_checked(self):
        result, _ = self.invoke("warmup", warmups=1)
        self.assertEqual(result.returncode, 1)
        self.assertIn("incorrect output", result.stderr)

    def test_strict_provenance_and_instrumented_binary_rejection(self):
        result, _ = self.invoke("ok", extra=("--require-provenance",))
        self.assertEqual(result.returncode, 1)
        self.assertIn("lacks source/compiler/flags/version provenance", result.stderr)
        result, _ = self.invoke("ok", metadata={"instrumented": True})
        self.assertEqual(result.returncode, 1)
        self.assertIn("separate timing binary", result.stderr)

    def test_pair_schedule_is_replayable(self):
        schedules = []
        for _ in range(2):
            result, output = self.invoke("ok", names=("first", "second"), samples=3)
            self.assertEqual(result.returncode, 0, result.stderr)
            rows = [json.loads(line) for line in
                    (output / "samples.jsonl").read_text(encoding="utf-8").splitlines()]
            schedules.append([(row["round"], row["order"], row["implementation"])
                              for row in rows])
            for round_number in range(3):
                block = [row["implementation"] for row in rows if row["round"] == round_number]
                self.assertCountEqual(block, ["first", "second"])
            summary = json.loads((output / "summary.json").read_text(encoding="utf-8"))
            paired = summary["workloads"][0]["candidate_over_reference"]["second"]
            self.assertEqual(paired["pairs"], 3)
            self.assertIsNotNone(paired["ci95"])
        self.assertEqual(schedules[0], schedules[1])


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

    def test_legacy_full_corpus_retains_its_programs_and_oracles(self):
        module = self.module()
        baseline = Path(__file__).resolve().parents[2] / (
            'docs/utilities/bc-performance-baseline-20261031/competitive-manifest.json')
        recorded = json.loads(baseline.read_text(encoding='utf-8'))['corpus']
        current = {row['name']: row for row in module.corpus('full')}
        for previous in recorded:
            case = current[previous['name']]
            self.assertEqual(hashlib.sha256(case['program'].encode()).hexdigest(),
                             previous['program_sha256'])
            oracle = ('\n'.join(case['expected']) + '\n').encode()
            self.assertEqual(hashlib.sha256(oracle).hexdigest(), previous['oracle_sha256'])

    def test_holdout_shapes_are_separate_and_replayable(self):
        module = self.module()
        full = module.corpus('full', listing=True)
        held = module.corpus('holdout', listing=True, seed=197201)
        self.assertEqual([case['group'] for case in full], [case['group'] for case in held])
        self.assertEqual(held, module.corpus('holdout', listing=True, seed=197201))
        for training, holdout in zip(full, held):
            if training['name'] != 'startup':
                self.assertNotEqual(training['name'], holdout['name'])
                self.assertNotEqual(training['parameters'], holdout['parameters'])
                self.assertNotEqual(training['program'], holdout['program'])

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
    parser.add_argument("--bc", help="native executable for the checked quick corpus")
    args, remaining = parser.parse_known_args()
    BENCHMARK = str(Path(args.benchmark).resolve())
    BC_BINARY = str(Path(args.bc).resolve()) if args.bc else None
    unittest.main(argv=[__file__, *remaining])
