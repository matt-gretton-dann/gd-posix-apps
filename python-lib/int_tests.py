"""Support library for integration tests"""
# Copyright 2020, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

import argparse
import subprocess
import sys
import os
import os.path


def parse_args():
    """Parse arguments"""
    parser = argparse.ArgumentParser()
    parser.add_argument("exe", help="Executable to test")
    return parser.parse_args()


def run_test(cmdline, expected_rc=0, expected_stdout=None, expected_stderr=None, test_name=None):
    """Run a test

    Runs the executable given in the list CMDLINE and checks for the expected
    return code EXPECTED_RC.  If EXPECTED_STDOUT or EXPECTED_STDERR are not
    None then they are also checked.

    Outputs a PASS/FAIL line to stdout.  The test name is given as TEST_NAME or
    the basename of cmdline[0]

    returns 1 on success, 0 on failure.
    """
    capture_output = expected_stdout is not None or expected_stderr is not None
    if capture_output:
        output_pipe = subprocess.PIPE
    else:
        output_pipe = None
    if test_name is None:
        test_name = os.path.basename(cmdline[0])

    rc = subprocess.run(cmdline, stderr=output_pipe,
                        stdout=output_pipe, universal_newlines=True)

    if rc.returncode != expected_rc:
        print(
            f"FAIL: {test_name} (incorrect exit code: expected {expected_rc} got {rc.returncode})")
        return False
    if expected_stdout is not None and rc.stdout != "":
        print(
            f"FAIL: {test_name} (stdout)\n---- EXPECTED: ----\n{expected_stdout}\n---- ACTUAL: ----\n{rc.stdout}\n")
        return False
    if expected_stderr is not None and rc.stderr != "":
        print(
            f"FAIL: {test_name} (stderr)\n---- EXPECTED: ----\n{expected_stdout}\n---- ACTUAL: ----\n{rc.stdout}\n")
        return False

    print(f"PASS: {test_name}")
    return True
