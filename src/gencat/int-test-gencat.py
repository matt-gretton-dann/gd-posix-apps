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
    stdin=b'$ empty msgcat\n',
    expected_stdout=b"MSG\0\1\0\0\0\0\0\0\0\0\0\0\0\x18\0\0\0\0\0\0\0",
    expected_stderr=b'')

# Generate a basic catalogue
test_output = tester.output_file('test.msg')
test_input = tester.input_file('basic-input.cat')
test_expected = tester.input_file('basic-input.msg')
skip_next = tester.run_test(
    [tester.exe(), test_output, test_input],
    test_name="Basic catalogue",
    expected_stdout='', expected_stderr='', files=[test_output])
tester.compare_files(
    test_output, test_expected, skip=skip_next)

# Check that trying to amend it with an empty file does nothing.
skip_next2 = tester.run_test(
    [tester.exe(), test_output, '-'],
    test_name="Empty input doesn't modify existing",
    stdin='$ empty msgcat\n',
    expected_stdout='', expected_stderr='', files=[test_output], skip=skip_next)
tester.compare_files(
    test_output, test_expected, skip=skip_next2)

# Similar but not quite
test_output3 = tester.output_file('test3.msg')
skip_next3 = tester.run_test(
    [tester.exe(), test_output3, test_input, '-'],
    test_name="Empty input doesn't modify existing (new)",
    stdin='$ empty msgcat\n',
    expected_stdout='', expected_stderr='', files=[test_output3], skip=skip_next)
tester.compare_files(
    test_output3, test_expected, skip=skip_next3)

# Check that trying to amend it with the same file does nothing, except produce
# a lot of warnings.
test_output4 = tester.output_file('test4.msg')
skip_next4 = tester.run_test(
    [tester.exe(), test_output4, test_input, test_input],
    test_name="Repeated input doesn't modify (new)",
    expected_stdout='', files=[test_output4], skip=skip_next)
tester.compare_files(
    test_output4, test_expected, skip=skip_next4)

# Similar but not quite
skip_next5 = tester.run_test(
    [tester.exe(), test_output, test_input],
    test_name="Repeated input doesn't modify existing (new)",
    expected_rc=0,
    expected_stdout='', files=[test_output], skip=skip_next)
tester.compare_files(
    test_output, test_expected, skip=skip_next5)


# Error tests
tester.run_test(
    [tester.exe(), test_output, tester.input_file(
        "THIS FILE DOESN'T EXIST", file_exists=False)],
    test_name="Error on non-existant input",
    expected_rc=1,
    expected_stdout='')


tester.summarize()
