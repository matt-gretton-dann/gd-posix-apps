#!/usr/bin/env python3
"""Integration tests for `awk`"""
# Copyright 2022, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0


import sys
import int_tests


def test_awk(program, expected_stdout, in_file=None):
    cmd_line = [tester.exe(), program]
    if in_file is not None:
        cmd_line.append(in_file)
    tester.run_test(cmd_line, test_name=program, expected_rc=0,
                    expected_stdout=expected_stdout, expected_stderr='')


tester = int_tests.TestRunner()

tester.run_test(
    [tester.exe()], test_name="Empty command line", expected_rc=1, stdin="",
    expected_stdout="", expected_stderr=None)

test_awk('BEGIN {}\\nEND {}', '')
test_awk('BEGIN { print "Hello world!"; }', "Hello world!\n")
test_awk('END { print "Goodbye world!"; }', "Goodbye world!\n")
test_awk('BEGIN { print 1, 2; }', "1 2\n")
test_awk('BEGIN { print OFS; }', ' \n')
test_awk('BEGIN { print ORS; }', '\n\n')

# The AWK Programming Language Chapter 1 Examples
emp_data = tester.input_file('emp.data')
test_awk('{ print $0; }', """Beth 4.00 0
Dan 3.75 0
Kathy 4.00 10
Mark 5.00 20
Mary 5.50 22
Susie 4.25 18
""", in_file=emp_data)
test_awk('{ print $1; }', """Beth
Dan
Kathy
Mark
Mary
Susie
""", in_file=emp_data)

tester.summarize()
