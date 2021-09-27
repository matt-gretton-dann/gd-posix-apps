#!/usr/bin/env python3
"""Integration tests for `ar`"""
# Copyright 2021, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0


import filecmp
import int_tests
import re
import sys

tester = int_tests.TestRunner()

tester.run_test(
    [tester.exe()], test_name="Empty command line", expected_rc=1, stdin="", expected_stdout="")

test_input1 = tester.input_file('input1.txt')
test_input2 = tester.input_file('input2.txt')
test_ar1 = tester.output_file('ar1.ar')
test_ar2 = tester.output_file('ar2.ar')
test_ar3 = tester.output_file('ar3.ar')

# Create an archive twice to see if the different ways of generating produce the same results.
failed_ar1_creation = tester.run_test(
    [tester.exe(), "-rc", test_ar1, test_input1, test_input2],
    test_name="Quiet generate basic archive",
    expected_rc=0, expected_stdout="", expected_stderr="")
failed_ar2_creation = tester.run_test(
    [tester.exe(), "-r", test_ar2, test_input1, test_input2],
    test_name="generate basic archive",
    expected_rc=0, expected_stderr=re.compile(f'.*{re.escape(test_ar2)}.*'))
failed_ar3_creation = tester.run_test(
    [tester.exe(), "-rcv", test_ar3, test_input1, test_input2],
    test_name="Verbose generate basic archive",
    expected_rc=0, expected_stdout=f"a - {test_input1}\na - {test_input2}\n")

tester.compare_files(test_ar1, test_ar2,
                     failed_ar1_creation or failed_ar2_creation)
tester.compare_files(test_ar1, test_ar3,
                     failed_ar1_creation or failed_ar3_creation)

# Check the contents of the archive
tester.run_test(
    [tester.exe(), "-t", test_ar1],
    test_name="ar1 ToC",
    expected_rc=0, expected_stdout="input1.txt\ninput2.txt\n", expected_stderr="")

tester.summarize()
