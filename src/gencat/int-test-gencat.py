#!/usr/bin/env python3
"""Integration tests for `gencat`"""
# Copyright 2020, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0


import sys
import int_tests

tester = int_tests.TestRunner()

tester.run_test(
    [tester.exe()], test_name="Empty command line", expected_rc=1, expected_stdout="")

tester.run_test(
    [tester.exe(), '-', '-'], test_name="Empty input",
    expected_rc=0,
    stdin=b'$ empty msgcat\n',
    expected_stdout=b"MSG\0\1\0\0\0\0\0\0\0\0\0\0\0\x18\0\0\0\0\0\0\0",
    expected_stderr=b'')

# Generate a basic catalogue
test_output = tester.output_file('test.msg')
test_input = tester.input_file('basic-input.cat')
skip_next = tester.run_test(
    [tester.exe(), test_output, test_input],
    test_name="Basic catalogue",
    expected_rc=0,
    expected_stdout='', expected_stderr='', files=[test_output])
tester.compare_files(
    test_output, tester.input_file('basic-input.msg'), skip=skip_next)

# Check that trying to amend it with an empty file does nothing.
skip_next2 = tester.run_test(
    [tester.exe(), test_output, '-'],
    test_name="Empty input doesn't modify",
    expected_rc=0,
    stdin='$ empty msgcat\n',
    expected_stdout='', expected_stderr='', files=[test_output], skip=skip_next)
tester.compare_files(
    test_output, tester.input_file('basic-input.msg'), skip=skip_next2)


tester.summarize()
