#!/usr/bin/env python3
"""Integration tests for `ar`"""
# Copyright 2021, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0


import filecmp
import int_tests
import os.path
import re
import sys

tester = int_tests.TestRunner()

tester.run_test(
    [tester.exe()], test_name="Empty command line", expected_rc=1, stdin="", expected_stdout="")

inputs = []
for i in range(0, 10):
    fname = tester.output_file(f"input{i}.txt")
    contents = f"File number {i}"
    with open(fname, "w") as f:
        print(f"{contents}", file=f)
    data = dict()
    data["fname"] = fname
    data["base"] = os.path.basename(fname)
    data["contents"] = contents
    inputs.append(data)

test_ar1 = tester.output_file('ar1.ar')
test_ar2 = tester.output_file('ar2.ar')
test_ar3 = tester.output_file('ar3.ar')

# Create an archive twice to see if the different ways of generating produce the same results.
failed_ar1_creation = tester.run_test(
    [tester.exe(), "-rc", test_ar1, inputs[0]["fname"], inputs[1]["fname"]],
    test_name="Quiet generate basic archive",
    expected_rc=0, expected_stdout="", expected_stderr="")
failed_ar2_creation = tester.run_test(
    [tester.exe(), "-r", test_ar2, inputs[0]["fname"], inputs[1]["fname"]],
    test_name="generate basic archive",
    expected_rc=0, expected_stderr=re.compile(f'.*{re.escape(test_ar2)}.*'))
failed_ar3_creation = tester.run_test(
    [tester.exe(), "-rcv", test_ar3, inputs[0]["fname"], inputs[1]["fname"]],
    test_name="Verbose generate basic archive",
    expected_rc=0, expected_stdout=f"a - {inputs[0]['fname']}\na - {inputs[1]['fname']}\n")

tester.compare_files(test_ar1, test_ar2,
                     failed_ar1_creation or failed_ar2_creation)
tester.compare_files(test_ar1, test_ar3,
                     failed_ar1_creation or failed_ar3_creation)

# Check the contents of the archive
tester.run_test(
    [tester.exe(), "-t", test_ar1],
    test_name="ar1 ToC",
    expected_rc=0, expected_stdout=f"{inputs[0]['base']}\n{inputs[1]['base']}\n", expected_stderr="",
    skip=failed_ar1_creation)
tester.run_test(
    [tester.exe(), "-p", test_ar1, inputs[0]["base"]],
    test_name=f"ar1 print {inputs[0]['base']}",
    expected_rc=0, expected_stdout=f"{inputs[0]['contents']}\n",
    skip=failed_ar1_creation)
tester.run_test(
    [tester.exe(), "-p", test_ar1, inputs[1]["fname"]],
    test_name=f"ar1 print {inputs[1]['base']}",
    expected_rc=0, expected_stdout=f"{inputs[1]['contents']}\n",
    skip=failed_ar1_creation)
tester.run_test(
    [tester.exe(), "-pv", test_ar1, inputs[0]["base"]],
    test_name=f"ar1 print {inputs[0]['base']} verbose",
    expected_rc=0, expected_stdout=f"\n<{inputs[0]['base']}>\n\n{inputs[0]['contents']}\n",
    skip=failed_ar1_creation)
tester.run_test(
    [tester.exe(), "-pv", test_ar1, inputs[1]['fname']],
    test_name=f"ar1 print {inputs[1]['base']} verbose",
    expected_rc=0, expected_stdout=f"\n<{inputs[1]['fname']}>\n\n{inputs[1]['contents']}\n",
    skip=failed_ar1_creation)
tester.run_test(
    [tester.exe(), "-p", test_ar1],
    test_name="ar1 print",
    expected_rc=0, expected_stdout=f"{inputs[0]['contents']}\n{inputs[1]['contents']}\n",
    skip=failed_ar1_creation)
tester.run_test(
    [tester.exe(), "-pv", test_ar1],
    test_name="ar1 print verbose",
    expected_rc=0, expected_stdout=f"\n<{inputs[0]['base']}>\n\n{inputs[0]['contents']}\n\n<{inputs[1]['base']}>\n\n{inputs[1]['contents']}\n",
    skip=failed_ar1_creation)


skip_ar4 = False


def test_manipulate(opts, files, toc, pos=None):
    global skip_ar4
    global inputs
    global tester
    test_ar4 = tester.output_file('ar4.ar')
    cmdline = [tester.exe(), opts]
    if pos is not None:
        cmdline.append(inputs[pos]['base'])
    cmdline.append(test_ar4)
    name = f"ar {opts} ar4.ar"
    artoc = ""
    contents = ""
    for i in files:
        cmdline.append(inputs[i]['fname'])
        name += f" {inputs[i]['base']}"
    for i in toc:
        artoc += f"{inputs[i]['base']}\n"
        contents += f"{inputs[i]['contents']}\n"
    skip_ar4 = tester.run_test(
        cmdline, test_name=name, expected_rc=0, expected_stdout="", skip=skip_ar4)
    skip_ar4 = tester.run_test(
        [tester.exe(), "-t", test_ar4],
        test_name=f"{name} (ToC)",
        expected_rc=0, expected_stdout=artoc, skip=skip_ar4)
    skip_ar4 = tester.run_test(
        [tester.exe(), "-p", test_ar4],
        test_name=f"{name} (Contents)",
        expected_rc=0, expected_stdout=contents, skip=skip_ar4)


# Test various manipulations
test_manipulate("-qc", [0, 1], [0, 1])
test_manipulate("-r", [2, 3, 4], [0, 1, 2, 3, 4])
test_manipulate("-m", [0, 3, 4], [1, 2, 0, 3, 4])
test_manipulate("-ma", [1, 4], [2, 0, 1, 4, 3], pos=0)
test_manipulate("-mb", [4, 1, 3], [1, 4, 3, 2, 0], pos=2)
test_manipulate("-mb", [3, 2], [1, 4, 3, 2, 0], pos=2)
test_manipulate("-mi", [4, 3], [1, 4, 3, 2, 0], pos=3)

# Change file contents so we know we have really changed stuff
for i in [1, 5]:
    inputs[i]['contents'] = f"Updated contents for input {i}"
    with open(inputs[i]['fname'], "w") as f:
        print(f"{inputs[i]['contents']}", file=f)

test_manipulate("-r", [1, 5], [1, 4, 3, 2, 0, 5])
test_manipulate("-ra", [4, 3, 6], [1, 4, 3, 2, 6, 0, 5], pos=2)
test_manipulate("-m", [0], [1, 4, 3, 2, 6, 5, 0])
test_manipulate("-rb", [3, 4, 7], [7, 1, 4, 3, 2, 6, 5, 0], pos=1)
test_manipulate("-ri", [1, 2], [7, 1, 4, 3, 2, 6, 5, 0], pos=1)

tester.summarize()
