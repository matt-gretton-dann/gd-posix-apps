# SPDX-License-Identifier: Apache-2.0
"""Output-checked end-to-end timing of the exact transform experiment."""
import argparse
import hashlib
import json
import os
import random
import statistics
import subprocess
import sys
import time
from pathlib import Path

if hasattr(sys, 'set_int_max_str_digits'):
    sys.set_int_max_str_digits(1000000)
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('base')
parser.add_argument('ntt')
parser.add_argument('output', type=Path)
parser.add_argument('--howard')
parser.add_argument('--aa', help='second baseline executable for a timing control')
parser.add_argument('--padding-boundaries', action='store_true',
                    help='replay a fresh grid around the padding-aware cutoff')
parser.add_argument('--rss', action='store_true', help='collect macOS peak RSS separately')
args = parser.parse_args()
binaries = {role: str(Path(getattr(args, role)).resolve())
            for role in ('base', 'ntt', 'howard', 'aa') if getattr(args, role)}
rng = random.Random(2042027 if args.padding_boundaries else 2042026)
schedule = random.Random(2026204)
environment = dict(os.environ, LC_ALL='C', BC_LINE_LENGTH='70', BC_ENV_ARGS='')
sizes = ((100008, 147447, 147448, 196605, 196606, 294903, 294904, 300011)
         if args.padding_boundaries else (99990, 100008, 145003, 210007, 300011))
cases = [(size, shape) for size in sizes
         for shape in ('square', 'dense', 'sparse-square')]
records = []
for size, shape in cases:
    left = '9' + ''.join(rng.choices('0123456789', k=size - 1))
    right = '8' + ''.join(rng.choices('0123456789', k=size - 2))
    if shape == 'sparse-square':
        left = '9' + '0' * (size - 2) + '3'
    if shape.endswith('square'):
        right = left
    program = (f'a={left}\na*a\n' if shape.endswith('square')
               else f'a={left}\nb={right}\na*b\n').encode()
    expected = (str(int(left) * int(right)) + '\n').encode()
    samples = {role: [] for role in binaries}
    for trial in range(8):
        order = list(binaries)
        schedule.shuffle(order)
        for role in order:
            before = time.perf_counter()
            child = subprocess.run([binaries[role]], input=program, capture_output=True,
                                   timeout=120, env=environment)
            elapsed = time.perf_counter() - before
            assert child.returncode == 0 and not child.stderr, (size, shape, role, child.stderr)
            assert child.stdout.replace(b'\r\n', b'\n').replace(b'\\\n', b'') == expected, (size, shape, role)
            samples[role].append(elapsed)
    ratios = {
        role: round(statistics.median(
            measured / reference for measured, reference in zip(samples['ntt'][1:], times[1:])), 3)
        for role, times in samples.items() if role != 'ntt'
    }
    print(size, shape, ratios, flush=True)
    rss = {}
    if args.rss:
        for role, binary in binaries.items():
            child = subprocess.run(['/usr/bin/time', '-l', binary], input=program,
                                   capture_output=True, timeout=120, env=environment)
            output = child.stdout.replace(b'\\\n', b'')
            assert child.returncode == 0 and output == expected
            rss[role] = next(int(line.split()[0]) for line in child.stderr.splitlines()
                             if b'maximum resident set size' in line)
    records.append(dict(rss_bytes=rss, ratios=ratios, size=size, shape=shape, checked=True, samples=samples,
                        program_sha256=hashlib.sha256(program).hexdigest(),
                        output_sha256=hashlib.sha256(expected).hexdigest()))
    args.output.write_text(json.dumps(records, indent=2) + '\n')
