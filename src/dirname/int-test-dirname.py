#!/usr/bin/env python3
"""Integration tests for `dirname`"""
# Copyright 2021, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

import re
import sys
import int_tests

tester = int_tests.TestRunner()

tester.run_test(
    [tester.exe(), "usr"], test_name="usr", expected_stdout=".\n")
tester.run_test(
    [tester.exe(), "usr/"], test_name="usr/", expected_stdout=".\n")
tester.run_test(
    [tester.exe(), ""], test_name="\"\"", expected_stdout=".\n")
tester.run_test(
    [tester.exe(), "/"], test_name="/", expected_stdout="/\n")
tester.run_test(
    [tester.exe(), "//"], test_name="//", expected_stdout=re.compile(r"^//?\n$"))
tester.run_test(
    [tester.exe(), "///"], test_name="///", expected_stdout="/\n")
tester.run_test(
    [tester.exe(), "/usr/"], test_name="/usr/", expected_stdout="/\n")
tester.run_test(
    [tester.exe(), "/usr/lib"], test_name="/usr/lib", expected_stdout="/usr\n")
tester.run_test(
    [tester.exe(), "//usr//lib//"], test_name="//usr//lib//", expected_stdout="//usr\n")
tester.run_test(
    [tester.exe(), "/home//dwc//test"], test_name="/home//dwc//test", expected_stdout="/home//dwc\n")

tester.run_test(
    [tester.exe()], test_name="Empty command line", expected_rc=1, expected_stdout="", expected_stderr=re.compile(r"^(.*dirname(\.exe)?): Missing arguments, expect a path\.\nUsage: \1 string\n$"))
tester.run_test(
    [tester.exe(), "a", "b"], test_name="Long command line", expected_rc=1, expected_stdout="", expected_stderr=re.compile(r"^(.*dirname(\.exe)?): Too many arguments have been specified\.\nUsage: \1 string\n$"))

tester.summarize()
