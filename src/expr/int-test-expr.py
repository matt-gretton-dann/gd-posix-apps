#!/usr/bin/env python3
"""Integration tests for `basename`"""
# Copyright 2021, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

import re
import sys
import int_tests

tester = int_tests.TestRunner()

tests = [
    {"cmdline": ['1'], "output": '1', "result": 0},
    {"cmdline": ['=', '=', '='], "output": '1', "result": 0},
    {"cmdline": ['34', '+', '12'], "output": '46', "result": 0},
    {"cmdline": ['34', '*', '0'], "output": '0', "result": 1},
    {"cmdline": ['34', '-', '-12'], "output": '46', "result": 0},
    {"cmdline": ['(', '34', ')'], "output": '34', "result": 0},
    {"cmdline": ['34', '/', '12'], "output": '2', "result": 0},
    {"cmdline": ['34', '%', '12'], "output": '10', "result": 0},
    {"cmdline": ['34', '=', '34'], "output": '1', "result": 0},
    {"cmdline": ['34', '!=', '34'], "output": '0', "result": 1},
    {"cmdline": ['34', '<', '34'], "output": '0', "result": 1},
    {"cmdline": ['34', '<=', '34'], "output": '1', "result": 0},
    {"cmdline": ['34', '>', '34'], "output": '0', "result": 1},
    {"cmdline": ['34', '>=', '34'], "output": '1', "result": 0},
    {"cmdline": ['34', '=', '35'], "output": '0', "result": 1},
    {"cmdline": ['34', '!=', '35'], "output": '1', "result": 0},
    {"cmdline": ['34', '<', '35'], "output": '1', "result": 0},
    {"cmdline": ['34', '<=', '35'], "output": '1', "result": 0},
    {"cmdline": ['34', '>=', '35'], "output": '0', "result": 1},
    {"cmdline": ['34', '>=', '35'], "output": '0', "result": 1},
    {"cmdline": ['36', '=', '3'], "output": '0', "result": 1},
    {"cmdline": ['36', '!=', '3'], "output": '1', "result": 0},
    {"cmdline": ['36', '<', '3'], "output": '0', "result": 1},
    {"cmdline": ['36', '<=', '3'], "output": '0', "result": 1},
    {"cmdline": ['36', '>', '35'], "output": '1', "result": 0},
    {"cmdline": ['36', '>=', '35'], "output": '1', "result": 0},
    {"cmdline": ['36', '&', '35'], "output": '36', "result": 0},
    {"cmdline": ['36', '&', '0'], "output": '0', "result": 1},
    {"cmdline": ['0', '&', '0'], "output": '0', "result": 1},
    {"cmdline": ['0', '&', '36'], "output": '0', "result": 1},
    {"cmdline": ['36', '|', '35'], "output": '36', "result": 0},
    {"cmdline": ['36', '|', '0'], "output": '36', "result": 0},
    {"cmdline": ['0', '|', '35'], "output": '35', "result": 0},
    {"cmdline": ['0', '|', '0'], "output": '0', "result": 1},
    {"cmdline": ['a', '=', '34'], "output": '0', "result": 1},
    {"cmdline": ['a', '!=', '34'], "output": '1', "result": 0},
    {"cmdline": ['111112', ':', '1*2'], "output": '6', "result": 0},
    {"cmdline": ['111112', ':', r'\(1*\)2'], "output": '11111', "result": 0},
    {"cmdline": ['3111112', ':', r'\(1*\)2'], "output": '', "result": 1},
    {"cmdline": ['3111112', ':', '1*2'], "output": '0', "result": 1},
]

for test in tests:
    cmdline = test["cmdline"]
    output = test["output"] + '\n'
    result = test["result"]
    tester.run_test(
        [tester.exe()] + cmdline, test_name=" ".join(cmdline), expected_stdout=output, expected_rc=result
    )

tester.summarize()
