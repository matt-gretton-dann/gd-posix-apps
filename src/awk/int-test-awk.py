#!/usr/bin/env python3
"""Integration tests for `awk`"""
# Copyright 2022, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0


import sys
import int_tests
import os.path


def test_awk(program, expected_stdout, in_file=None, expected_rc=0):
    cmd_line = [tester.exe(), program]
    test_name = repr(program)
    if in_file is not None:
        cmd_line.append(in_file)
        test_name += f" {os.path.basename(in_file)}"

    expected_stderr = ''
    if expected_rc != 0:
        expected_stderr = None
        expected_stdout = None
        test_name += " (error)"

    tester.run_test(cmd_line, test_name=test_name, expected_rc=expected_rc,
                    expected_stdout=expected_stdout,
                    expected_stderr=expected_stderr)


tester = int_tests.TestRunner()

tester.run_test(
    [tester.exe()], test_name="Empty command line", expected_rc=1, stdin="",
    expected_stdout="", expected_stderr=None)

emp_data = tester.input_file('emp.data')

test_awk('BEGIN {}\nEND {}', '')
test_awk('BEGIN { print "Hello world!" }', "Hello world!\n")
test_awk('END { print "Goodbye world!" }', "Goodbye world!\n")
test_awk('BEGIN { print 1, 2 }', "1 2\n")
test_awk('BEGIN { print OFS }', ' \n')
test_awk('BEGIN { print ORS }', '\n\n')
test_awk('BEGIN { print (1) }', '1\n')
test_awk('BEGIN { print (1, 2) }', '1 2\n')
test_awk('BEGIN { print (1), 2 }', '1 2\n')
test_awk("{ print COUNT++ }\nEND { print COUNT }", '\n1\n2\n3\n4\n5\n6\n',
         in_file=emp_data)
test_awk("{ COUNT-- }\nEND { print COUNT }", '-6\n', in_file=emp_data)
test_awk("{ print ++COUNT }\nEND { print COUNT }", '1\n2\n3\n4\n5\n6\n6\n',
         in_file=emp_data)
test_awk("{ --COUNT }\nEND { print COUNT }", '-6\n', in_file=emp_data)
test_awk("BEGIN { print 2 ^ 5 }", "32\n")
test_awk("BEGIN { print 2 ^ 3 ^ 2, (2 ^ 3) ^ 2, 2 ^ (3 ^ 2) }", "512 64 512\n")
test_awk("BEGIN { print + 1 }", "1\n")
test_awk("BEGIN { print - 1 }", "-1\n")
test_awk("BEGIN { print ! 301 }", "0\n")
test_awk("BEGIN { print ! ! 301 }", "1\n")
test_awk('BEGIN { print ! "" }', "1\n")
test_awk('BEGIN { print ! "1" }', "0\n")
test_awk('BEGIN { print "23" * 2 }', "46\n")
test_awk('BEGIN { print "23" / 2 }', "11.5\n")
test_awk('BEGIN { print "23" % 2 }', "1\n")
test_awk('BEGIN { print 24 / 4 / 2, (24 / 4) / 2, 24 / (4 / 2) }', "3 3 12\n")
test_awk('BEGIN { print 24 % 11 % 3, (24 % 11) % 3, 24 % (11 % 3) }',
         "2 2 0\n")
test_awk('BEGIN { print 1 + 2, 1 - 2 }', '3 -1\n')
test_awk('BEGIN { print 1 + 2 * 3, (1 + 2) * 3, 1 + (2 * 3) }', '7 9 7\n')
test_awk('BEGIN { print 1 - 2 - 3, (1 - 2) - 3, 1 - (2 - 3) }', '-4 -4 2\n')
test_awk('BEGIN { print "Hello" " " "world!" }', 'Hello world!\n')
test_awk('BEGIN { print "Hello " 2.3 " world!" }', 'Hello 2.3 world!\n')
test_awk(
    'BEGIN { print (1 == 2), (2 == 1), (1 == 1), ("abc" == "def"), ("def" == "abc"), ("abc" == "abc") }',
    '0 0 1 0 0 1\n')
test_awk(
    'BEGIN { print (1 != 2), (2 != 1), (1 != 1), ("abc" != "def"), ("def" != "abc"), ("abc" != "abc") }',
    '1 1 0 1 1 0\n')
test_awk(
    'BEGIN { print (1 < 2), (2 < 1), (1 < 1), ("abc" < "def"), ("def" < "abc"), ("abc" < "abc") }',
    '1 0 0 1 0 0\n')
test_awk(
    'BEGIN { print (1 <= 2), (2 <= 1), (1 <= 1), ("abc" <= "def"), ("def" <= "abc"), ("abc" <= "abc") }',
    '1 0 1 1 0 1\n')
test_awk(
    'BEGIN { print (1 > 2), (2 > 1), (1 > 1), ("abc" > "def"), ("def" > "abc"), ("abc" > "abc") }',
    '0 1 0 0 1 0\n')
test_awk(
    'BEGIN { print (1 >= 2), (2 >= 1), (1 >= 1), ("abc" >= "def"), ("def" >= "abc"), ("abc" >= "abc") }',
    '0 1 1 0 1 1\n')

# Some error tests
test_awk('BEGIN { print (1 }', None, expected_rc=1)
test_awk('BEGIN { print (1, }', None, expected_rc=1)
test_awk('BEGIN { print 2 ^ }', None, expected_rc=1)
test_awk('BEGIN { print ++ 2 }', None, expected_rc=1)
test_awk('BEGIN { print -- 2 }', None, expected_rc=1)
test_awk('BEGIN { print ++ FUNC ++ }', None, expected_rc=1)
test_awk('BEGIN { print -- FUNC -- }', None, expected_rc=1)
test_awk('BEGIN { print 3 ++ }', None, expected_rc=1)
test_awk('BEGIN { print 3 -- }', None, expected_rc=1)
test_awk('BEGIN { print 1 < 2}', None, expected_rc=1)

# The AWK Programming Language Chapter 1 Examples
emp_data = tester.input_file('emp.data')

# Page 1, 1.1 Getting started
test_awk('$3 > 0 { print $1, $2 * $3 }', """Kathy 40
Mark 100
Mary 121
Susie 76.5
""", in_file=emp_data)
# Page 2
test_awk('$3 == 0 { print $1 }', """Beth
Dan
""", in_file=emp_data)
# Page 3
test_awk('$3 == 0', """Beth 4.00 0
Dan 3.75 0
""", in_file=emp_data)
test_awk('{ print $1 }', """Beth
Dan
Kathy
Mark
Mary
Susie
""", in_file=emp_data)
# Page 5 Section 1.2
test_awk('{ print }', """Beth 4.00 0
Dan 3.75 0
Kathy 4.00 10
Mark 5.00 20
Mary 5.50 22
Susie 4.25 18
""", in_file=emp_data)
test_awk('{ print $0 }', """Beth 4.00 0
Dan 3.75 0
Kathy 4.00 10
Mark 5.00 20
Mary 5.50 22
Susie 4.25 18
""", in_file=emp_data)
test_awk('{ print $1, $3 }', """Beth 0
Dan 0
Kathy 10
Mark 20
Mary 22
Susie 18
""", in_file=emp_data)
# Page 6
test_awk('{ print NF, $1, $NF }', """3 Beth 0
3 Dan 0
3 Kathy 10
3 Mark 20
3 Mary 22
3 Susie 18
""", in_file=emp_data)
test_awk('{ print $1, $2 * $3 }', """Beth 0
Dan 0
Kathy 40
Mark 100
Mary 121
Susie 76.5
""", in_file=emp_data)
# Page 7
test_awk('{ print NR, $0 }', """1 Beth 4.00 0
2 Dan 3.75 0
3 Kathy 4.00 10
4 Mark 5.00 20
5 Mary 5.50 22
6 Susie 4.25 18
""", in_file=emp_data)
test_awk('{ print "Total pay for", $1, "is", $2 * $3 }', """Total pay for Beth is 0
Total pay for Dan is 0
Total pay for Kathy is 40
Total pay for Mark is 100
Total pay for Mary is 121
Total pay for Susie is 76.5
""", in_file=emp_data)

tester.summarize()
