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


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("tuner")
    args, remaining = parser.parse_known_args()
    TUNER = str(Path(args.tuner).resolve())
    unittest.main(argv=[__file__, *remaining])
