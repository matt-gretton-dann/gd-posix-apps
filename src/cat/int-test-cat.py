#!/usr/bin/env python3
"""Integration tests for `cat`"""
# Copyright 2021, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

import re
import sys
import int_tests

tester = int_tests.TestRunner()

tester.run_test(
    [tester.exe(), '-'], test_name="Explicit Standard input",
    stdin='Hello\n',
    expected_stdout='Hello\n',
    expected_stderr='')

tester.run_test(
    [tester.exe()], test_name="Implicit Standard input",
    stdin='Hello\n',
    expected_stdout='Hello\n',
    expected_stderr='')

tester.run_test(
    [tester.exe(), '-u'], test_name="Accept -u",
    stdin='Hello\n',
    expected_stdout='Hello\n',
    expected_stderr='')

input_file1 = tester.input_file('jabberwocky.txt')
with open(input_file1, "rb") as fp:
    expected_output = fp.read()

tester.run_test(
    [tester.exe(), input_file1], test_name="One file",
    expected_stdout=expected_output)

tester.run_test(
    [tester.exe(), input_file1, input_file1], test_name="Repeated file",
    expected_stdout=(expected_output+expected_output))

tester.run_test(
    [tester.exe(), '-', input_file1], test_name="Stdin and input_file",
    stdin=b"Hello",
    expected_stdout=(b"Hello"+expected_output))

tester.run_test(
    [tester.exe(), '-', input_file1, '-'], test_name="Stdin twice and input_file",
    stdin=b"Hello",
    expected_stdout=(b"Hello"+expected_output))

tester.run_test(
    [tester.exe(), '-d'], test_name="Bad option",
    expected_rc=1,
    expected_stderr=re.compile(r"^(.*cat(\.exe)?): Unrecognized option: -d\.\nUsage: \1 \[-u\] \[file\.\.\.\]\n$"))

tester.run_test(
    [tester.exe(), tester.input_file("THIS FILE DOESN'T EXIST", file_exists=False)], test_name="Non existent file",
    expected_rc=1,
    expected_stderr=re.compile(r"^(.*cat(\.exe)?): Unable to open the file"))

tester.summarize()
