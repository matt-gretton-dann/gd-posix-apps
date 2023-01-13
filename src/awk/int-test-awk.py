#!/usr/bin/env python3
"""Integration tests for `awk`"""
# Copyright 2022, Matthew Gretton-Dann
# SPDX-License-Identifier: Apache-2.0


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
test_awk('BEGIN { print ("ns1234sa" ~ /[0-9]+/) }', "1\n")
test_awk('BEGIN { print ("ns1234sa" !~ /[0-9]+/) }', "0\n")
test_awk('BEGIN { print (0 && 0) }', "0\n")
test_awk('BEGIN { print (0 && 1) }', "0\n")
test_awk('BEGIN { print (1 && 0) }', "0\n")
test_awk('BEGIN { print (1 && 1) }', "1\n")
test_awk('BEGIN { print (0 || 0) }', "0\n")
test_awk('BEGIN { print (0 || 1) }', "1\n")
test_awk('BEGIN { print (1 || 0) }', "1\n")
test_awk('BEGIN { print (1 || 1) }', "1\n")
test_awk('BEGIN { print (1 ? 2 : 3) }', "2\n")
test_awk('BEGIN { print (0 ? 2 : 3) }', "3\n")
test_awk('BEGIN { a=1; print a }', "1\n")
test_awk('BEGIN { b=a=1; print a, b }', "1 1\n")
test_awk('BEGIN { a=1; a+=1; print a }', "2\n")
test_awk('BEGIN { a=1; a-=1; print a }', "0\n")
test_awk('BEGIN { a=2; a*=3; print a }', "6\n")
test_awk('BEGIN { a=100; a/=5; print a }', "20\n")
test_awk('BEGIN { a=100; a%=33; print a }', "1\n")
test_awk('BEGIN { a=2; a^=10; print a }', "1024\n")
test_awk('BEGIN { printf "%s-%d-%g", "Hello", 10, 372.2 }', "Hello-10-372.2")
test_awk('BEGIN { print length "100" }', "3\n")
test_awk('BEGIN { print 1; { print 2 } print 3 }', "1\n2\n3\n")
test_awk('BEGIN { if (1) { print 2 } else { print 3 } print 4 }', "2\n4\n")
test_awk('BEGIN { if (0) { print 2 } else { print 3 } print 4 }', "3\n4\n")
test_awk('BEGIN { if (1)  print 2 }', "2\n")
test_awk('BEGIN { if (0)  print 2 }', "")
test_awk('BEGIN { while (i < 4) print i++ }', '\n1\n2\n3\n')
test_awk('BEGIN { for (i = 0; i < 4; ++i) print i }', "0\n1\n2\n3\n")
test_awk('BEGIN { a[1] = 20; print a[1] }', "20\n")
test_awk('BEGIN { a[1,2] = 20; print a[1,2] }', "20\n")
test_awk('BEGIN { a[1 SUBSEP 2] = 20; print a[1, 2] }', "20\n")
test_awk('BEGIN { SUBSEP=","; a["1,2"] = 20; print a[1, 2] }', "20\n")
test_awk('BEGIN { for(i = 0; i < ARGC; ++i) print i, ARGV[i] }',
         f"0 {emp_data}\n", in_file=emp_data)
test_awk('BEGIN { print sin(0) }', '0\n')
test_awk('BEGIN { print cos(0) }', '1\n')
test_awk('''BEGIN { piby4 = atan2(1, 1)
                    print ((cos(piby4) - sin(piby4)) < 1e-8)
                    print ((cos(piby4) - sin(piby4)) > -1e-8) }''',
         '1\n1\n')
test_awk('BEGIN { print log(exp(10)), exp(log(10)) }', '10 10\n')
test_awk('''BEGIN { piby4 = atan2(1, 1); sqrt1_2 = 1 / sqrt(2)
                    print ((cos(piby4) - sqrt1_2) < 1e-8)
                    print ((cos(piby4) - sqrt1_2) > -1e-8) }''',
         '1\n1\n')
test_awk('BEGIN { print sqrt(100) }', '10\n')
test_awk('BEGIN { print int(10.382), int(10) }', '10 10\n')
test_awk('''BEGIN { x = srand(20); a = rand(); y = srand(20); b = rand()
                    print (a == b), y }''',
         '1 20\n')
test_awk(
    'BEGIN { a="Hello world!"; print gsub(/[aeiou]/, "*", a), a }',
    "3 H*ll* w*rld!\n")
test_awk(
    'BEGIN { a="Hello world!"; print sub(/[aeiou]/, "*", a), a }',
    "1 H*llo world!\n")
test_awk(
    'BEGIN { a="Rhythm!"; print sub(/[aeiou]/, "*", a), a }',
    "0 Rhythm!\n")
test_awk(
    'BEGIN { a="Hello world!"; print gsub(/[aeiou]/, "[&]", a), a }',
    "3 H[e]ll[o] w[o]rld!\n")
test_awk(
    'BEGIN { a="Hello world!"; print gsub(/[aeiou]/, "\\\\&", a), a }',
    "3 H&ll& w&rld!\n")
test_awk(
    'BEGIN { print index("Hello world!", "zzz"), index("Hello world!", "o ") }',
    '0 5\n')
test_awk(
    'BEGIN { print match("Hello world!", /zzz/), RSTART, RLENGTH }',
    "0 0 -1\n")
test_awk('BEGIN { print match("Hello world!", /o.*o/), RSTART, RLENGTH }',
         "5 5 4\n")
test_awk('BEGIN { print substr("Hello world!", 5, 4) }', "o wo\n")
test_awk('BEGIN { print tolower("Hello world!") }', "hello world!\n")
test_awk('BEGIN { print toupper("Hello world!") }', "HELLO WORLD!\n")
test_awk('BEGIN { a=sprintf( "%s-%d-%g", "Hello", 10, 372.2); print a }',
         "Hello-10-372.2\n")
test_awk('BEGIN { print split("Hello world", a); print a[1]; print a[2] }',
         "2\nHello\nworld\n")
test_awk(
    'BEGIN { print split("Hello world", a, /[aeiou]/); print a[1]; print a[2]; print a[3]; print a[4] }',
    "4\nH\nll\n w\nrld\n")
test_awk('BEGIN { exit; print "nothing to see here." }', "")

# Have exit codes will travel...
test_awk('BEGIN { exit 2 }', None, expected_rc=2)
test_awk('BEGIN { exit 2 }\nEND { exit 3 }', None, expected_rc=3)

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
interest_data = tester.input_file('interest.data')
countries_data = tester.input_file('countries.data')

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
# Page 8 Section 1.3
test_awk('{ printf("total pay for %s is $%.2f\\n", $1, $2 * $3) }', """total pay for Beth is $0.00
total pay for Dan is $0.00
total pay for Kathy is $40.00
total pay for Mark is $100.00
total pay for Mary is $121.00
total pay for Susie is $76.50
""", in_file=emp_data)
test_awk('{ printf("%-8s $%6.2f\\n", $1, $2 * $3) }', """Beth     $  0.00
Dan      $  0.00
Kathy    $ 40.00
Mark     $100.00
Mary     $121.00
Susie    $ 76.50
""", in_file=emp_data)
test_awk('{ printf("%6.2f  %s\\n", $2 * $3, $0) }', """  0.00  Beth 4.00 0
  0.00  Dan 3.75 0
 40.00  Kathy 4.00 10
100.00  Mark 5.00 20
121.00  Mary 5.50 22
 76.50  Susie 4.25 18
""", in_file=emp_data)
# Page 9 Section 1.4
test_awk('$2 >= 5', """Mark 5.00 20
Mary 5.50 22
""", in_file=emp_data)
test_awk('$2 * $3 >= 50 { printf("$%.2f for %s\\n", $2 * $3, $1) }', """$100.00 for Mark
$121.00 for Mary
$76.50 for Susie
""", in_file=emp_data)
test_awk('$1 == "Susie"', """Susie 4.25 18
""", in_file=emp_data)
# Page 10
test_awk('/Susie/', """Susie 4.25 18
""", in_file=emp_data)
test_awk('$2 >= 4 || $3 >= 20', """Beth 4.00 0
Kathy 4.00 10
Mark 5.00 20
Mary 5.50 22
Susie 4.25 18
""", in_file=emp_data)
test_awk('$2 >= 4\n$3 >= 20', """Beth 4.00 0
Kathy 4.00 10
Mark 5.00 20
Mark 5.00 20
Mary 5.50 22
Mary 5.50 22
Susie 4.25 18
""", in_file=emp_data)
test_awk('!($2 < 4 && $3 < 20)', """Beth 4.00 0
Kathy 4.00 10
Mark 5.00 20
Mary 5.50 22
Susie 4.25 18
""", in_file=emp_data)
# Page 11
test_awk('''NF != 3 { print $0, "number of fields is not equal to 3" }
$2 < 3.35 { print $O, "rate is below minimum wage" }
$2 > 10 { print $0, "rate exceeds $10 per hour" }
$3 < 0 { print $0, "negative hours worked" }
$3 > 60{ print $0, "too many hours worked" }''', '', in_file=emp_data)
test_awk('BEGIN { print "NAME RATE HOURS"; print "" }\n{ print }', """NAME RATE HOURS

Beth 4.00 0
Dan 3.75 0
Kathy 4.00 10
Mark 5.00 20
Mary 5.50 22
Susie 4.25 18
""", in_file=emp_data)
# Page 12 Section 1.5
test_awk('''$3> 15{emp=emp+1}
END { print emp, "employees worked more than 15 hours" }''',
         "3 employees worked more than 15 hours\n", in_file=emp_data)
test_awk('END { print NR, "employees" }', '6 employees\n', in_file=emp_data)
test_awk('''
    { pay =pay + $2 * $3 }
END { print NR, "employees"
      print "total pay is", pay
      print "average pay is", pay/NR
    }''', '''6 employees
total pay is 337.5
average pay is 56.25
''', in_file=emp_data)
test_awk('''$2 > maxrate { maxrate =$2; maxemp =$1 }
END          { print "highest hourly rate:", maxrate, "for", maxemp }''',
         'highest hourly rate: 5.50 for Mary\n', in_file=emp_data)
# Page 13
test_awk('''
    { names = names $1 " " }
END { print names }
''', "Beth Dan Kathy Mark Mary Susie \n", in_file=emp_data)
# Page 14
test_awk('''{ last = $0 }; END { print last }''', "Susie 4.25 18\n",
         in_file=emp_data)
# Page 14
test_awk('{ print $1, length($1) }', '''Beth 4
Dan 3
Kathy 5
Mark 4
Mary 4
Susie 5
''', in_file=emp_data)
test_awk('''
    { nc = nc + length($0) + 1; nw = nw + NF; }
END { print NR, "lines,", nw, "words,", nc, "characters" }''',
         '6 lines, 18 words, 77 characters\n', in_file=emp_data)
# Section 1.6
test_awk('''
$2 > 6 { n = n + 1; pay = pay + $2 * $3 }
   END { if (n > 0)
            print n, "employees, total pay is", pay,
                  "average pay is", pay/n
         else
            print "no employees are paid more than $6/hour"
       }
''', 'no employees are paid more than $6/hour\n', in_file=emp_data)
# Page 15
test_awk('''
# interest1 - compute compound interest
# input: amount rate years
# output: compounded value at the end of each year
{ print $0
  i=1
  while (i <= $3) {
    printf("\\t%.2f\\n", $1 * (1 + $2) ^ i)
    i=i+1
  }
}''',
         '1000 .06 5\n\t1060.00\n\t1123.60\n\t1191.02\n\t1262.48\n\t1338.23\n1000 .12 5\n\t1120.00\n\t1254.40\n\t1404.93\n\t1573.52\n\t1762.34\n',
         in_file=interest_data)
# Page 16
test_awk('''
# interest2 - compute compound interest
# input: amount rate years
# output: compounded value at the end of each year
{ print $0
  for (i = 1; i <= $3; i = i + 1)
    printf("\\t%.2f\\n", $1 * (1 + $2) ^ i)
}
''',
         '1000 .06 5\n\t1060.00\n\t1123.60\n\t1191.02\n\t1262.48\n\t1338.23\n1000 .12 5\n\t1120.00\n\t1254.40\n\t1404.93\n\t1573.52\n\t1762.34\n',
         in_file=interest_data)
# Section 1.7
test_awk('''
# reverse - print input in reverse order by line
    { line[NR] = $0 } # remember each input line
END { i = NR
      while (i > 0) { # print lines in reverse order
        print line[i]
        i = i-1
      }
    }
''', '''Susie 4.25 18
Mary 5.50 22
Mark 5.00 20
Kathy 4.00 10
Dan 3.75 0
Beth 4.00 0
''', in_file=emp_data)
# Page 17
test_awk('''
# reverse - print input in reverse order by line
    { line[NR] = $0 } # remember each input line
END { for (i = NR; i > 0; i = i - 1)
        print line[i]
    }
''', '''Susie 4.25 18
Mary 5.50 22
Mark 5.00 20
Kathy 4.00 10
Dan 3.75 0
Beth 4.00 0
''', in_file=emp_data)

# Chapter 2
# Page 22
test_awk('{ print $1, $3 }', '''USSR 275
Canada 25
China 1032
USA 237
Brazil 134
India 746
Mexico 78
France 55
Japan 120
Germany 61
England 56
''', in_file=countries_data)

test_awk('''{ print \\
    $1,
    $2,
    $3 }''', '''USSR 8649 275
Canada 3852 25
China 3705 1032
USA 3615 237
Brazil 3286 134
India 1267 746
Mexico 762 78
France 211 55
Japan 144 120
Germany 96 61
England 94 56
''', in_file=countries_data)

# Page 24
test_awk('''
BEGIN { FS ="\\t" # make tab the field separator
        printf("%10s %6s %5s   %s\\n\\n", "COUNTRY", "AREA", " POP", "CONTINENT")
      }
      { printf("%10s %6d %5d   %s\\n", $1, $2, $3, $4)
        area = area + $2
        pop = pop + $3
      }
END   { printf("\\n%10s %6d %5d\\n", "TOTAL", area, pop) }''', '''   COUNTRY   AREA   POP   CONTINENT

      USSR   8649   275   Asia
    Canada   3852    25   North America
     China   3705  1032   Asia
       USA   3615   237   North America
    Brazil   3286   134   South America
     India   1267   746   Asia
    Mexico    762    78   North America
    France    211    55   Europe
     Japan    144   120   Asia
   Germany     96    61   Europe
   England     94    56   Europe

     TOTAL  25681  2819
''', in_file=countries_data)

# Page 25
test_awk('$3/$2 >= 0.5', '''India\t1267\t746\tAsia
Japan\t144\t120\tAsia
Germany\t96\t61\tEurope
England\t94\t56\tEurope
''', in_file=countries_data)

# Page 26
test_awk('$0 >= "M"', '''USSR\t8649\t275\tAsia
USA\t3615\t237\tNorth America
Mexico\t762\t78\tNorth America
''', in_file=countries_data)
test_awk('$1 < $4', '''Canada\t3852\t25\tNorth America
Brazil\t3286\t134\tSouth America
Mexico\t762\t78\tNorth America
England\t94\t56\tEurope
''', in_file=countries_data)
test_awk('$2 < $3', '', in_file=countries_data)
test_awk('/Asia/', '''USSR\t8649\t275\tAsia
China\t3705\t1032\tAsia
India\t1267\t746\tAsia
Japan\t144\t120\tAsia
''', in_file=countries_data)

# Page 27
test_awk('$4 ~ /Asia/', '''USSR\t8649\t275\tAsia
China\t3705\t1032\tAsia
India\t1267\t746\tAsia
Japan\t144\t120\tAsia
''', in_file=countries_data)
test_awk('$4 !~ /Asia/', '''Canada\t3852\t25\tNorth America
USA\t3615\t237\tNorth America
Brazil\t3286\t134\tSouth America
Mexico\t762\t78\tNorth America
France\t211\t55\tEurope
Germany\t96\t61\tEurope
England\t94\t56\tEurope
''', in_file=countries_data)
test_awk('$0 ~ /Asia/', '''USSR\t8649\t275\tAsia
China\t3705\t1032\tAsia
India\t1267\t746\tAsia
Japan\t144\t120\tAsia
''', in_file=countries_data)

# Page 31
test_awk('$4 == "Asia" && $3 > 500', '''China\t3705\t1032\tAsia
India\t1267\t746\tAsia
''', in_file=countries_data)
test_awk('$4 == "Asia" || $4 == "Europe"', '''USSR\t8649\t275\tAsia
China\t3705\t1032\tAsia
India\t1267\t746\tAsia
France\t211\t55\tEurope
Japan\t144\t120\tAsia
Germany\t96\t61\tEurope
England\t94\t56\tEurope
''', in_file=countries_data)

# Page 32
test_awk('$4 ~ /^(Asia|Europe)$/', '''USSR\t8649\t275\tAsia
China\t3705\t1032\tAsia
India\t1267\t746\tAsia
France\t211\t55\tEurope
Japan\t144\t120\tAsia
Germany\t96\t61\tEurope
England\t94\t56\tEurope
''', in_file=countries_data)
test_awk('/Asia/ || /Europe/', '''USSR\t8649\t275\tAsia
China\t3705\t1032\tAsia
India\t1267\t746\tAsia
France\t211\t55\tEurope
Japan\t144\t120\tAsia
Germany\t96\t61\tEurope
England\t94\t56\tEurope
''', in_file=countries_data)
test_awk('/Asia|Europe/', '''USSR\t8649\t275\tAsia
China\t3705\t1032\tAsia
India\t1267\t746\tAsia
France\t211\t55\tEurope
Japan\t144\t120\tAsia
Germany\t96\t61\tEurope
England\t94\t56\tEurope
''', in_file=countries_data)
test_awk('/Canada/, /USA/', '''Canada\t3852\t25\tNorth America
China\t3705\t1032\tAsia
USA\t3615\t237\tNorth America
''', in_file=countries_data)
test_awk('/Europe/, /Africa/', '''France\t211\t55\tEurope
Japan\t144\t120\tAsia
Germany\t96\t61\tEurope
England\t94\t56\tEurope
''', in_file=countries_data)
test_awk('FNR == 1, FNR == 5 { print FILENAME ": " $0 }', f'''{countries_data}: USSR\t8649\t275\tAsia
{countries_data}: Canada\t3852\t25\tNorth America
{countries_data}: China\t3705\t1032\tAsia
{countries_data}: USA\t3615\t237\tNorth America
{countries_data}: Brazil\t3286\t134\tSouth America
''', in_file=countries_data)
test_awk('FNR <= 5 { print FILENAME ": " $0 }', f'''{countries_data}: USSR\t8649\t275\tAsia
{countries_data}: Canada\t3852\t25\tNorth America
{countries_data}: China\t3705\t1032\tAsia
{countries_data}: USA\t3615\t237\tNorth America
{countries_data}: Brazil\t3286\t134\tSouth America
''', in_file=countries_data)

tester.summarize()
