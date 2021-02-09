#!/usr/bin/env python3
"""Integration tests for `echo`"""
# Copyright 2021, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

import re
import sys
import int_tests

tester = int_tests.TestRunner()

tests = [
    {"cmdline": ['Hello', 'world'], "output": 'Hello world\n'},
    {"cmdline": [r'\a'], "output": '\a\n'},
    {"cmdline": [r'\b'], "output": '\b\n'},
    {"cmdline": [r'Hello\c', 'world'], "output": 'Hello'},
    {"cmdline": [r'\f'], "output": '\f\n'},
    {"cmdline": [r'\n'], "output": '\n\n'},
    {"cmdline": [r'\t'], "output": '\t\n'},
    {"cmdline": [r'\v'], "output": '\v\n'},
    {"cmdline": [r'\\'], "output": '\\\n'},
    {"cmdline": [r'\041'], "output": '!\n'},
    {"cmdline": [r'\0041'], "output": '!\n'},
    {"cmdline": [r'\0041a'], "output": '!a\n'},
    {"cmdline": [r'\00410'], "output": '!0\n'},
    {"cmdline": [r'\0377\c'], "output": b'\xff'}
]

for test in tests:
    cmdline = test["cmdline"]
    output = test["output"]
    err = "" if isinstance(output, str) else b""
    tester.run_test(
        [tester.exe()] + cmdline, test_name=" ".join(cmdline), expected_stdout=output, expected_stderr=err
    )

err_tests = [
    {"cmdline": ['Hello\\'], "output": 'Hello\\\n', "err": re.compile(".+")},
    {"cmdline": ['\\0401'], "output": '\n', "err": re.compile(".+")},
    {"cmdline": ['\\T'], "output": '\\T\n', "err": re.compile(".+")},
]

for test in err_tests:
    cmdline = test["cmdline"]
    output = test["output"]
    err = test["err"]
    tester.run_test(
        [tester.exe()] + cmdline, test_name=" ".join(cmdline), expected_stdout=output, expected_stderr=err
    )

tester.summarize()
