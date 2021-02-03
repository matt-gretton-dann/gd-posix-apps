#!/usr/bin/env python3
"""Integration tests for `basename`"""
# Copyright 2021, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

import re
import sys
import int_tests

tester = int_tests.TestRunner()

tester.run_test(
    [tester.exe(), "usr"], test_name="usr", expected_stdout="usr\n")
tester.run_test(
    [tester.exe(), "usr/"], test_name="usr/", expected_stdout="usr\n")
tester.run_test(
    [tester.exe(), ""], test_name="\"\"", expected_stdout=re.compile("^.?\n$"))
tester.run_test(
    [tester.exe(), "/"], test_name="/", expected_stdout="/\n")
tester.run_test(
    [tester.exe(), "//"], test_name="//", expected_stdout=re.compile("^//?\n$"))
tester.run_test(
    [tester.exe(), "///"], test_name="///", expected_stdout="/\n")
tester.run_test(
    [tester.exe(), "/usr/"], test_name="/usr/", expected_stdout="usr\n")
tester.run_test(
    [tester.exe(), "/usr/lib"], test_name="/usr/lib", expected_stdout="lib\n")
tester.run_test(
    [tester.exe(), "//usr//lib//"], test_name="//usr//lib//", expected_stdout="lib\n")
tester.run_test(
    [tester.exe(), "/home//dwc//test"], test_name="/home//dwc//test", expected_stdout="test\n")

tester.run_test(
    [tester.exe(), "/home/fred/file.txt", ".txt"], test_name="/home/fred/file.txt .txt", expected_stdout="file\n")
tester.run_test(
    [tester.exe(), "/home/fred/file.txt", ".sh"], test_name="/home/fred/file.txt .sh", expected_stdout="file.txt\n")

tester.run_test(
    [tester.exe()], test_name="Empty command line", expected_rc=1, expected_stdout="", expected_stderr=re.compile(r"^(.*basename(\.exe)?): Missing arguments, expect at least a path\.\nUsage: \1 string \[suffix\]\n$"))
tester.run_test(
    [tester.exe(), "a", "b", "c"], test_name="Long command line", expected_rc=1, expected_stdout="", expected_stderr=re.compile(r"^(.*basename(\.exe)?): Too many arguments have been specified\.\nUsage: \1 string \[suffix\]\n$"))

tester.summarize()
