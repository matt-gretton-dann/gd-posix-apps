#!/usr/bin/env python3
"""Integration tests for `awk`"""
# Copyright 2022, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0


import sys
import int_tests

tester = int_tests.TestRunner()

tester.run_test(
    [tester.exe()], test_name="Empty command line", expected_rc=1, stdin="",
    expected_stdout="", expected_stderr=None)

tester.run_test(
    [tester.exe(), 'BEGIN {}\\nEND {}'], test_name="Empty actions",
    expected_rc=0, stdin="", expected_stdout="", expected_stderr='')

# tester.run_test(
#    [tester.exe(), 'BEGIN { print "Hello world!\\n"}'],
#    test_name="Hello world begin.",
#    expected_stdout='Hello world!\n',
#    expected_stderr='')

tester.summarize()
