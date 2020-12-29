#!/usr/bin/env python3
"""Integration tests for `true`"""
# Copyright 2020, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

import sys
import int_tests

args = int_tests.parse_args()

success = True
success = success and int_tests.run_test(
    [args.exe], expected_stdout="", expected_stderr="")
sys.exit(0 if success else 1)
