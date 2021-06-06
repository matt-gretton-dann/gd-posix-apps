#! /usr/bin/env python3
"""Benchmark bc implementations

This is a very simple benchmarking app that tries to benchmark the performance
of `bc` in a variety of situations.

Usage is as follows:

   bench-bc.py <bc> <script>

Where script should define a function f(a).

The benchmarking script will call <bc> passing in <script> along with a call
to f().  The parameter passed to f will increase until each individual
execution takes longer than the timeout.

The script outputs a CSV format table given the following fields:
 * number passed to f()
 * Time taken on average for a call to f(number)
 * Number of times bc run to get the result.
"""

import argparse
import atexit
import os.path
import os
import shutil
import subprocess
import sys
import timeit
import tempfile

ap = argparse.ArgumentParser()
ap.add_argument("bc", help="BC executable to benchmark")
ap.add_argument("script", help="Base script to pass to benchmark")
args = ap.parse_args()

# Timeout in seconds
timeout = 20

tempdir = tempfile.mkdtemp()
atexit.register(shutil.rmtree, tempdir)

run_file = os.path.join(tempdir, "in.bc")

num=5
mul_by_5 = False

print('"Number", "Time taken", "Reps"')
while True:
    with open(run_file, 'w') as of:
        of.write("f({})\nquit\n".format(num))

    call_arguments = """['{}', '{}', '{}'], stdout=subprocess.DEVNULL""".format(args.bc, args.script, run_file)

    reps = 1
    time_taken = 1
    min_time_taken = 3
    while time_taken < min_time_taken:
        reps = (min_time_taken + 1) / time_taken * reps
        reps = int(reps)
        if reps < 1:
            reps = 1
        time_taken = timeit.timeit(
            stmt="subprocess.run({})".format(call_arguments),
            setup="""import subprocess;""", number=reps)

    time_taken /= reps

    print("{},{},{}".format(num, time_taken, reps))
    if (time_taken > timeout):
        sys.exit(0)

    if mul_by_5:
        num *= 5
        mul_by_5 = False
    else:
        num *= 2
        mul_by_5 = True
