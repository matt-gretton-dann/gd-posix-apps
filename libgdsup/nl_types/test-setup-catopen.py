#!/usr/bin/env python3
"""Integration tests for `gencat`"""
# Copyright 2020, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

import os
import sys
import int_tests

tester = int_tests.TestRunner()

test_output = tester.output_file(
    os.path.join('en', 'US', 'UTF-8', 'messages.msg'))
tester.run_test(
    [tester.exe(), test_output, '-'], test_name="Generate en/US/UTF-8/messages.msg",
    stdin='1 en_US.UTF-8',
    expected_stderr=''
)

test_output = tester.output_file(os.path.join('en', 'US', 'messages.msg'))
tester.run_test(
    [tester.exe(), test_output, '-'], test_name="Generate en/US/messages.msg",
    stdin='1 en_US',
    expected_stderr=''
)

test_output = tester.output_file(os.path.join('en',  'messages.msg'))
tester.run_test(
    [tester.exe(), test_output, '-'], test_name="Generate en/messages.msg",
    stdin='1 en',
    expected_stderr=''
)

test_output = tester.output_file(os.path.join('en_US.UTF-8.messages.msg'))
tester.run_test(
    [tester.exe(), test_output, '-'], test_name="Generate en_US.UTF-8.messages.msg",
    stdin='1 en_US.UTF-8',
    expected_stderr=''
)

test_output = tester.output_file(os.path.join('en', 'large.msg'))
tester.run_test(
    [tester.exe(), test_output, '-'], test_name="Generate large.msg",
    stdin='$set 1\n1 Hello world\n2 Goodbye world\n$set 2\n1 Fred\n5 George\n',
    expected_stderr=''
)

tester.summarize()
