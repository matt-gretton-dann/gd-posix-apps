#!/usr/bin/env python3
"""Integration tests for `cksum`"""
# Copyright 2021, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0

import int_tests
import re
import shutil
import sys

tester = int_tests.TestRunner()

# First see if we fail to compress the uncompressible file :-).
input1 = tester.input_file("uncompressible.bin")
output1 = tester.output_file("uncompressible.bin")
output2 = tester.output_file("uncompressible.bin.Z")
expected2 = tester.input_file("uncompressible.bin.16.Z")

shutil.copyfile(input1, output1)

skip1 = tester.run_test(
    [tester.exe(), "-b", "16", "-f", output1], files=[output2])
tester.compare_files(output2, expected2, skip1)

# Second see if we compress a repeated string correctly
input3 = tester.input_file("repeated.bin")
output3 = tester.output_file("repeated.bin")
output4 = tester.output_file("repeated.bin.Z")
expected4 = tester.input_file("repeated.bin.9.Z")

shutil.copyfile(input3, output3)

skip3 = tester.run_test(
    [tester.exe(), "-b9", output3], files=[output4])
tester.compare_files(output4, expected4, skip3)

tester.summarize()
