#!/usr/bin/env python3
"""Integration tests for `asa`"""
# Copyright 2021, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0


import sys
import int_tests

tester = int_tests.TestRunner()

tester.run_test(
    [tester.exe()], test_name="Empty command line", expected_rc=0, stdin="", expected_stdout="", expected_stderr="")

tester.run_test(
    [tester.exe(), '-'], test_name="Standard input",
    stdin=' Hello\n',
    expected_stdout='Hello\n',
    expected_stderr='')

# Test we generate expected outputs:
test_input1 = tester.input_file('input1.txt')
test_input2 = tester.input_file('input2.txt')
with open(tester.input_file('output1.txt'), 'r') as fp:
    test_expected1 = fp.read()
with open(tester.input_file('output2.txt'), 'r') as fp:
    test_expected2 = fp.read()
with open(tester.input_file('output12.txt'), 'r') as fp:
    test_expected12 = fp.read()

tester.run_test(
    [tester.exe(), test_input1], test_name="Input 1",
    expected_stdout=test_expected1,
    expected_stderr="")
tester.run_test(
    [tester.exe(), test_input2], test_name="Input 2",
    expected_stdout=test_expected2,
    expected_stderr="")
tester.run_test(
    [tester.exe(), test_input1, test_input2], test_name="Input 1, Input 2",
    expected_stdout=test_expected12,
    expected_stderr="")

# Error tests
tester.run_test(
    [tester.exe(), tester.input_file("THIS FILE DOESN'T EXIST", file_exists=False)],
    test_name="Error on non-existent input",
    expected_rc=1,
    expected_stdout='')


tester.summarize()
