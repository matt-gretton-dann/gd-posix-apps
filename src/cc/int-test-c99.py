#!/usr/bin/env python3
"""Integration tests for `cc`"""
# Copyright 2021, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

import re
import sys
import int_tests

tester = int_tests.TestRunner()

# Test input translation via std
tester.run_test(
    [tester.exe()],
    test_name="Emply command line",
    stdin="",
    expected_stdout="",
    expected_stderr="")
tester.run_test(
    [tester.exe()],
    test_name="stdin string",
    stdin="\"Hello World!\"\n123\n",
    expected_stdout="\"Hello World!\"\n123\n",
    expected_stderr="")

input1 = tester.input_file("simple-test.c")
with open(input1, "r") as f:
    expected_output = f.read()
tester.run_test(
    [tester.exe(), input1],
    test_name="Simple input file",
    expected_stdout=expected_output)
tester.run_test(
    [tester.exe(), input1, input1],
    test_name="Simple input file twice",
    expected_stdout=expected_output+expected_output)

# Test bad options
tester.run_test(
    [tester.exe(), "a non existent file"],
    test_name="Non existent file",
    expected_stdout="",
    expected_stderr=re.compile(
        r"^\(command line\):0:0:.0002:Serious Error:.*$"),
    expected_rc=1)

# Trigraph tester
for test in ["5_2_1_1-trigraph-sequences", "6_4_9-comments"]:
    input = tester.input_file(f"{test}.c")
    expected = tester.input_file(f"{test}.expected")
    with open(expected, "r") as f:
        expected_output = f.read()
    tester.run_test(
        [tester.exe(), input],
        test_name=f"cc -E {test}.c",
        expected_stdout=expected_output,
        expected_stderr=None)

tester.summarize()
