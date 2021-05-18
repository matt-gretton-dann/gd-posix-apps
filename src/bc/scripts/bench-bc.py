#! /usr/bin/env python3

import sys
import subprocess
import argparse
import timeit
import tempfile
import os.path
import os

ap = argparse.ArgumentParser()
ap.add_argument("bc", help="BC executable to benchmark")
ap.add_argument("script", help="Base script to pass to benchmark")
args = ap.parse_args()

# Timeout in seconds
timeout = 20

tempdir = tempfile.mkdtemp()
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
