#!/usr/bin/env python3
"""Exact large-product checks for the optional Toom tier, without elapsed gates.

Python's integer multiplication is independent of bc's decimal limb kernels.
SPDX-License-Identifier: Apache-2.0
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import random
import subprocess
import sys


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('bc')
    parser.add_argument('--large', action='store_true', help='include 100k/200k decimal digits')
    parser.add_argument('--report', type=Path)
    args = parser.parse_args()
    if hasattr(sys, 'set_int_max_str_digits'):
        sys.set_int_max_str_digits(1000000)
    rng = random.Random(197204)
    observations = []
    sizes = [53, 54, 55, 1071, 1152, 4608, 9216, 10000]
    if args.large:
        sizes += [100000, 200003]
    for size in sizes:
        dense = ''.join(str(rng.randrange(1 if i == 0 else 0, 10)) for i in range(size))
        for shape in ('balanced', 'unequal', 'repeated', 'sparse', 'square'):
            if shape == 'balanced':
                right = ''.join(str(rng.randrange(1 if i == 0 else 0, 10))
                                for i in range(size - 1))
            elif shape == 'unequal':
                right = '7' * max(1, size // 7)
            elif shape == 'repeated':
                right = '9' * size
            elif shape == 'sparse':
                right = '1' + '0' * (size - 2) + '1'
            else:
                right = dense
            # Copies of the operands remain observable after destination reuse.
            operation = 'a*=a' if shape == 'square' else 'a*=b'
            program = 'a={}\nb={}\nc=a\n{}\na\nb\nc\n'.format(dense, right, operation)
            expected = [str(int(dense) * int(right)), right, dense]
            child = subprocess.run([args.bc], input=program.encode(), stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE, timeout=120,
                                   env=dict(os.environ, LC_ALL='C', BC_LINE_LENGTH='70',
                                            BC_ENV_ARGS=''))
            output = child.stdout.replace(b'\r\n', b'\n').replace(b'\\\n', b'').decode('ascii').splitlines()
            if child.returncode or child.stderr or output != expected:
                raise RuntimeError('exact product/alias check failed: {} {}'.format(size, shape))
            observations.append(dict(decimal_digits=size, shape=shape, checked=True,
                program_sha256=hashlib.sha256(program.encode()).hexdigest(),
                oracle_sha256=hashlib.sha256(('\n'.join(expected) + '\n').encode()).hexdigest(),
                output_sha256=hashlib.sha256(child.stdout).hexdigest()))
        print(size, 'five exact product/alias checks', flush=True)
    if args.report:
        args.report.write_text(json.dumps(dict(seed=197204, checked=observations,
            binary_sha256=hashlib.sha256(Path(args.bc).read_bytes()).hexdigest(),
            checker_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
            scope='Correctness only; no speed or peak-memory acceptance claim'), indent=2) + '\n')
    return 0


if __name__ == '__main__':
    sys.exit(main())
