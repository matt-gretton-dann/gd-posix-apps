#!/usr/bin/env python3
"""Integration tests for `false`"""
# Copyright 2020, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0


import sys
import int_tests

tester = int_tests.TestRunner()

tester.run_test(
    [tester.exe()], expected_rc=1, expected_stdout="", expected_stderr="")
tester.summarize()
