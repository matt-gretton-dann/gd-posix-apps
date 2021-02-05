#!/usr/bin/env python3
"""Integration tests for `cksum`"""
# Copyright 2021, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

import re
import sys
import int_tests

tester = int_tests.TestRunner()

# Test input translation via std
tester.run_test(
    [tester.exe()],
    stdin=b"",
    expected_stdout=b"4294967295 0\n",
    expected_stderr=b"")
tester.run_test(
    [tester.exe(), "-"],
    stdin=b"\0",
    expected_stdout=b"4215202376 1\n",
    expected_stderr=b"")

input1 = tester.input_file("test-input1.txt")
input2 = tester.input_file("test-input2.txt")
expected_output = f"1655064767 1355 {input1}\n1533133176 681 {input2}\n"
tester.run_test(
    [tester.exe(), input1, input2],
    expected_stdout=expected_output.encode())

# Test bad options
tester.run_test(
    [tester.exe(), "--", "a non existent file"],
    expected_stdout="",
    expected_rc=1)

tester.summarize()
