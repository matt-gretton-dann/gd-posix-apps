# SPDX-License-Identifier: Apache-2.0
"""Check identical bc input via argv and redirected stdin in paired rounds."""

import csv
import hashlib
import random
import subprocess
import sys
import time
from pathlib import Path

base, candidate, program = map(Path, sys.argv[1:])
expected = b"20000\n"
rng = random.Random(200)
payload = program.read_bytes()
writer = csv.writer(sys.stdout)
writer.writerow(("phase", "round", "order", "mode", "role", "seconds", "input_sha256"))
digest = hashlib.sha256(payload).hexdigest()
for phase, rounds in (("warmup", 3), ("measure", 31)):
    for index in range(rounds):
        modes = ["named", "redirected"]
        rng.shuffle(modes)
        order = 0
        for mode in modes:
            roles = [("base", base), ("candidate", candidate)]
            rng.shuffle(roles)
            for role, binary in roles:
                argv = [str(binary.resolve())]
                if mode == "named":
                    argv.append(str(program.resolve()))
                with program.open("rb") as source:
                    stdin = source if mode == "redirected" else subprocess.DEVNULL
                    start = time.perf_counter()
                    child = subprocess.run(argv, stdin=stdin, capture_output=True, timeout=30)
                    elapsed = time.perf_counter() - start
                if child.returncode or child.stderr or child.stdout != expected:
                    raise SystemExit(f"incorrect output for {role} via {mode}")
                writer.writerow((phase, index, order, mode, role, elapsed, digest))
                order += 1
