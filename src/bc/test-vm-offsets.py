#!/usr/bin/env python3
"""Bounded offset rejection and intentional branch fall-through checks."""

import argparse
import os
import subprocess
import unittest


class OffsetTests(unittest.TestCase):
    def run_case(self, name):
        def run(mode):
            return subprocess.run([DRIVER, name, mode], stdout=subprocess.PIPE,
                                  stderr=subprocess.PIPE, universal_newlines=True,
                                  timeout=2, env=dict(os.environ, LC_ALL="C"))
        checked, verified = run('checked'), run('verified')
        self.assertEqual((checked.returncode, checked.stdout, checked.stderr),
                         (verified.returncode, verified.stdout, verified.stderr))
        return checked

    def test_reject_before_execution(self):
        for case, operand in (("op1-end", 1), ("op1-after", 1), ("op1-before", 1), ("op1-min", 1),
                              ("op2-end", 2), ("op2-after", 2), ("op2-before", 2), ("op2-min", 2),
                              ("array-index-after", 2), ("conditional-after", 2)):
            with self.subTest(case=case):
                run = self.run_case(case)
                self.assertEqual(run.returncode, 1, run.stderr)
                self.assertEqual(run.stdout, "")
                self.assertIn("Operand " + str(operand) + " of instruction", run.stderr)

    def test_reject_result_types_before_execution(self):
        for case in ("add-string", "add-empty", "add-location", "load-number", "length-string",
                     "branch-string", "index-string", "push-string", "negate-string",
                     "return-string", "store-number-array", "store-array-number",
                     "store-number-location",
                     "sqrt-string", "abs-string", "scale-string", "print-empty"):
            with self.subTest(case=case):
                run = self.run_case(case)
                self.assertEqual(run.returncode, 1, run.stderr)
                self.assertEqual(run.stdout, "")
                self.assertIn("has an incompatible result type", run.stderr)

    def test_each_binary_operand_type(self):
        for operation in ("add", "subtract", "multiply", "divide", "modulo", "power", "equals",
                          "not-equals", "less-than", "less-equal"):
            for operand in (1, 2):
                with self.subTest(operation=operation, operand=operand):
                    run = self.run_case("binary-" + operation + "-" + str(operand))
                    self.assertEqual(run.returncode, 1, run.stderr)
                    self.assertEqual(run.stdout, "")
                    self.assertIn("Operand " + str(operand) + " of instruction", run.stderr)
                    self.assertIn("has an incompatible result type", run.stderr)

    def test_reject_control_flow_before_execution(self):
        for case in ("skipped-producer", "join-producer", "self-producer", "middle-eof",
                     "missing-function-end", "mismatched-function-end", "nested-function",
                     "branch-into-function", "branch-out-of-function", "cross-function-result",
                     "orphan-function-end"):
            with self.subTest(case=case):
                run = self.run_case(case)
                self.assertEqual(run.returncode, 1, run.stderr)
                self.assertEqual(run.stdout, "")
                self.assertIn("Invalid control flow at instruction", run.stderr)

    def test_reject_parameter_marker_paths(self):
        for case in ("call-without-mark", "push-without-mark", "pop-without-function",
                     "param-without-function", "array-param-without-function", "unmatched-mark",
                     "join-marker", "function-missing-pop", "function-duplicate-pop",
                     "function-bypass-pop", "function-prepop-loop", "function-owned-loop",
                     "function-param-after-pop", "function-array-param-after-pop",
                     "function-return-open-mark", "function-call-without-mark",
                     "function-pop-owned-mark"):
            with self.subTest(case=case):
                run = self.run_case(case)
                self.assertEqual(run.returncode, 1, run.stderr)
                self.assertEqual(run.stdout, "")
                self.assertIn("Invalid control flow at instruction", run.stderr)

    def test_valid_fallthrough_and_result(self):
        for case, output in (("branch-end", "EXECUTED\n"), ("conditional-end", "EXECUTED\n"),
                             ("last-result", "EXECUTED\n42\n"),
                             ("valid-forward-result", "EXECUTED\n42\n"),
                             ("valid-location-print", "EXECUTED\na\n"),
                             ("valid-control-join", "EXECUTED\n9\n"),
                             ("valid-alternative-calls", "EXECUTED\n7\n")):
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
