#!/usr/bin/env python3
"""Bounded offset rejection and intentional branch fall-through checks."""

import argparse
import os
import subprocess
import unittest


class OffsetTests(unittest.TestCase):
    def run_case(self, name):
        return subprocess.run([DRIVER, name], stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                              universal_newlines=True, timeout=2,
                              env=dict(os.environ, LC_ALL="C"))

    def test_reject_before_execution(self):
        for case, operand in (("op1-end", 1), ("op1-after", 1), ("op1-before", 1), ("op1-min", 1),
                              ("op2-end", 2), ("op2-after", 2), ("op2-before", 2), ("op2-min", 2),
                              ("array-index-after", 2), ("conditional-after", 2)):
            with self.subTest(case=case):
                run = self.run_case(case)
                self.assertEqual(run.returncode, 1, run.stderr)
                self.assertEqual(run.stdout, "")
                self.assertIn("Operand " + str(operand) + " of instruction", run.stderr)

    def test_valid_fallthrough_and_result(self):
        for case, output in (("branch-end", "EXECUTED\n"), ("conditional-end", "EXECUTED\n"),
                             ("last-result", "EXECUTED\n42\n")):
            with self.subTest(case=case):
                run = self.run_case(case)
                self.assertEqual(run.returncode, 0, run.stderr)
                self.assertEqual(run.stdout, output)
                self.assertEqual(run.stderr, "")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("driver")
    args, remaining = parser.parse_known_args()
    DRIVER = args.driver
    unittest.main(argv=[__file__, *remaining])
