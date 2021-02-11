#!/usr/bin/env python3
"""Integration tests for `printf`"""
# Copyright 2021, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

import re
import sys
import int_tests

tester = int_tests.TestRunner()

tests = [
    {"cmdline": ['Hello world'], "output": 'Hello world'},
    {"cmdline": [r'\a'], "output": '\a'},
    {"cmdline": [r'\b'], "output": '\b'},
    {"cmdline": [r'\f'], "output": '\f'},
    {"cmdline": [r'\n'], "output": '\n'},
    {"cmdline": [r'\t'], "output": '\t'},
    {"cmdline": [r'\v'], "output": '\v'},
    {"cmdline": [r'\\'], "output": '\\'},
    {"cmdline": [r'\041'], "output": '!'},
    {"cmdline": [r'\41'], "output": '!'},
    {"cmdline": [r'\41a'], "output": '!a'},
    {"cmdline": [r'\0410'], "output": '!0'},
    {"cmdline": [r'%b\n', r'Hello\c', 'world'], "output": 'Hello'},
    {"cmdline": [r'%.4b\n', r'Hello\c', 'world'], "output": 'Hell'},
    {"cmdline": [r'%b\n', r'\041'], "output": '!\n'},
    {"cmdline": [r'%b\n', r'\0041'], "output": '!\n'},
    {"cmdline": [r'%b\n', r'\0041a'], "output": '!a\n'},
    {"cmdline": [r'%b\n', r'\00410'], "output": '!0\n'},
    {"cmdline": [r'%b\n', r'\0377\c'], "output": b'\xff'},
    {"cmdline": [r'%s\n', "Hello", "World"], "output": "Hello\nWorld\n"},
    {"cmdline": [r'%5s\n', "Hello", "World!", "a", ""],
        "output": "Hello\nWorld!\n    a\n     \n"},
    {"cmdline": [r'%-5s\n', "Hello", "World!", "a", ""],
        "output": "Hello\nWorld!\na    \n     \n"},
    {"cmdline": [r'%5.4s\n', "Hello", "World!", "a", ""],
        "output": " Hell\n Worl\n    a\n     \n"},
    {"cmdline": [r'%d %i\n', '10', '20', '-30', '\' ', '+4'],
        "output": "10 20\n-30 32\n4 0\n"},
    {"cmdline": [r'%4d', '10'], "output": "  10"},
    {"cmdline": [r'%04d', '10'], "output": "0010"},
    {"cmdline": [r'%+04d', '10'], "output": "+010"},
    {"cmdline": [r'% 04d', '10'], "output": " 010"},
    {"cmdline": [r'%04d', '-10'], "output": "-010"},
    {"cmdline": [r'%4d', '-10'], "output": " -10"},
    {"cmdline": [r'%-4d', '-10'], "output": "-10 "},
    {"cmdline": [r'%4.3d', '10'], "output": " 010"},
    {"cmdline": [r'%-4.3d', '10'], "output": "010 "},
    {"cmdline": [r'%.d', '0'], "output": ""},
    {"cmdline": [r'%d', '0'], "output": "0"},
    {"cmdline": [r'%d', '0x123'], "output": "291"},
    {"cmdline": [r'%d', '0x456789AB'], "output": "1164413355"},
    {"cmdline": [r'%d', '0xcdef'], "output": "52719"},
    {"cmdline": [r'%d', '01234567'], "output": "342391"},
    {"cmdline": [r'%x', '10'], "output": "a"},
    {"cmdline": [r'%4.3x', '10'], "output": " 00a"},
    {"cmdline": [r'%2x', '16'], "output": "10"},
    {"cmdline": [r'%#4x', '16'], "output": "0x10"},
    {"cmdline": [r'%#5x', '16'], "output": " 0x10"},
    {"cmdline": [r'%05x', '16'], "output": "00010"},
    {"cmdline": [r'%0#5x', '16'], "output": "0x010"},
    {"cmdline": [r'%0#5X', '16'], "output": "0X010"},
    {"cmdline": [r'%3.2x', '16'], "output": " 10"},
    {"cmdline": [r'%-3.2x', '16'], "output": "10 "},
    {"cmdline": [r'%o', '16'], "output": "20"},
    {"cmdline": [r'%#o', '16'], "output": "020"},
    {"cmdline": [r'%#.2o', '16'], "output": "020"},
    {"cmdline": [r'%#.3o', '16'], "output": "020"},
    {"cmdline": [r'%#.4o', '16'], "output": "0020"},
    {"cmdline": [r'%#2o', '16'], "output": "020"},
    {"cmdline": [r'%2o', '16'], "output": "20"},
    {"cmdline": [r'%c', '100'], "output": "1"},
    {"cmdline": [r'%%'], "output": "%"}
]

for test in tests:
    cmdline = test["cmdline"]
    output = test["output"]
    err = "" if isinstance(output, str) else b""
    tester.run_test(
        [tester.exe()] + cmdline, test_name=" ".join(cmdline), expected_stdout=output, expected_stderr=err
    )

warn_tests = [
    {"cmdline": ['%d', '5a'], "output": '5'},
    {"cmdline": ['%d', '9999999999'], "output": '2147483647'},
    {"cmdline": ['%d', '-9999999999'], "output": '-2147483647'},
    {"cmdline": ['%d', 'ABC'], "output": '0'},
    {"cmdline": ['%d', '\''], "output": '0'},
]

err_tests = [
    {"cmdline": ['100', '1', '2'], "output": '100', "rc": 1},
    {"cmdline": ['-100'], "output": "", "rc": 1},
    {"cmdline": ['100', '200'], "output": "100", "rc": 1},
    {"cmdline": ['%P'], "output": "", "rc": 1},
]

for test in warn_tests + err_tests:
    cmdline = test["cmdline"]
    output = test["output"]
    err = re.compile(test.get("err", ".+"))
    rc = test.get("rc", 0)
    tester.run_test(
        [tester.exe()] + cmdline, test_name=" ".join(cmdline),
        expected_stdout=output, expected_stderr=err, expected_rc=rc
    )


tester.summarize()
