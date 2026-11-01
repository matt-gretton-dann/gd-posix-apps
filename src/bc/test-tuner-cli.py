#!/usr/bin/env python3
"""Fast, bounded argument/error checks for the multiplication calibration tool."""

import argparse
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


class TunerCli(unittest.TestCase):
    def invoke(self, arguments, status):
        try:
            result = subprocess.run([TUNER, *arguments], timeout=3, universal_newlines=True,
                                    stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                    env=dict(os.environ, LC_ALL="C"))
        except subprocess.TimeoutExpired:
            self.fail("argument validation started a long calibration")
        self.assertEqual(result.returncode, status, result.stderr)
        if status:
            self.assertEqual(result.stdout, "")
            self.assertIn("Calibration failed:", result.stderr)
        else:
            self.assertEqual(result.stderr, "")
            self.assertIn("Usage:", result.stdout)

    def test_help_and_valid_seeds(self):
        self.invoke(["--help"], 0)
        for seed in ("0", "197234", "18446744073709551615"):
            with self.subTest(seed=seed):
                self.invoke(["--seed", seed, "--help"], 0)

    def test_invalid_seeds(self):
        self.invoke(["--seed"], 1)
        for seed in ("", "-1", "+1", "12x", "18446744073709551616"):
            with self.subTest(seed=seed):
                self.invoke(["--seed", seed], 1)

    def test_invalid_arguments(self):
        self.invoke(["--unknown"], 1)
        self.invoke(["first.hh", "second.hh"], 1)

    def test_unwritable_output_is_rejected_before_calibration(self):
        with tempfile.TemporaryDirectory() as directory:
            self.invoke([str(Path(directory) / "missing-parent" / "output.hh")], 1)

    def test_quick_sweep_checks_every_candidate_and_shape(self):
        with tempfile.TemporaryDirectory() as directory:
            header = Path(directory) / 'dispatch.hh'
            child = subprocess.run([TUNER, '--quick', '--seed', '197203', str(header)],
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                   universal_newlines=True, timeout=10)
            self.assertEqual(child.returncode, 0, child.stderr)
            self.assertEqual(child.stderr, '')
            self.assertIn('Tuner seed: 197203', child.stdout)
            self.assertIn('Tuner clock: steady monotonic elapsed nanoseconds', child.stdout)
            rows = [line.split(',') for line in child.stdout.splitlines()
                    if line.startswith('sample,') and not line.startswith('sample,left')]
            self.assertEqual(len(rows), 3 * 4 * 3 * 8)
            seen = set()
            for _, left, right, square, round_, point, batch, elapsed, checked in rows:
                self.assertGreater(int(batch), 0)
                self.assertGreater(int(elapsed), 0)
                self.assertEqual(checked, '1')
                seen.add((int(left), int(right), int(square), int(round_), int(point)))
            expected = {(left, left * (2 if shape == 1 else 8 if shape == 2 else 1),
                         int(shape == 3), round_, point)
                        for left in (2, 12, 32) for shape in range(4)
                        for round_ in range(3)
                        for point in (16, 32, 64, 96, 126, 192, 256, 512)}
            self.assertEqual(seen, expected)
            self.assertIn('Input seed: 197203', header.read_text())
            self.assertRegex(header.read_text(),
                             r'#define BC_MULTIPLY_SPLIT_POINT (16|32|64|96|126|192|256|512)\n')


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("tuner")
    args, remaining = parser.parse_known_args()
    TUNER = str(Path(args.tuner).resolve())
    unittest.main(argv=[__file__, *remaining])
