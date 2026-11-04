#!/usr/bin/env python3
"""Run a checked cohort of short bc processes for software CPU profiling."""

import argparse
import errno
import os
from pathlib import Path
import pty
import select
import subprocess
import time
import tty


def read_line(terminal, timeout):
    deadline = time.monotonic() + timeout
    data = bytearray()
    while not data.endswith(b'\n'):
        remaining = deadline - time.monotonic()
        if remaining <= 0 or not select.select([terminal], [], [], remaining)[0]:
            raise RuntimeError('interactive output deadline exceeded')
        data.extend(os.read(terminal, 64))
    return bytes(data)


def interactive(binary, environment):
    terminal, slave = pty.openpty()
    try:
        tty.setraw(slave)
        with subprocess.Popen([binary], stdin=slave, stdout=slave, stderr=subprocess.PIPE,
                              env=environment) as child:
            os.close(slave)
            slave = None
            try:
                os.write(terminal, b'a=19\n6*7\n')
                if read_line(terminal, 3) != b'42\n':
                    raise RuntimeError('unexpected output from first interactive input')
                os.write(terminal, b'a+23\nquit\n')
                if read_line(terminal, 3) != b'42\n':
                    raise RuntimeError('unexpected output from second interactive input')
                if child.wait(timeout=3) != 0 or child.stderr.read():
                    raise RuntimeError('interactive bc did not quit cleanly')
                # Linux reports EIO when the PTY slave has closed.
                try:
                    trailing = os.read(terminal, 64)
                except OSError as error:
                    if error.errno != errno.EIO:
                        raise
                    trailing = b''
                if trailing:
                    raise RuntimeError('unexpected trailing interactive output')
            finally:
                if child.poll() is None:
                    child.kill()
                    child.wait(timeout=3)
    finally:
        if slave is not None:
            os.close(slave)
        os.close(terminal)


def check(binary, scenario, environment):
    if scenario == 'interactive':
        interactive(binary, environment)
        return
    program = b'6*7\n' if scenario == 'startup' else b'1/0\n'
    child = subprocess.run([binary], input=program, capture_output=True,
                           timeout=3, env=environment)
    if scenario == 'startup':
        if (child.returncode, child.stdout, child.stderr) != (0, b'42\n', b''):
            raise RuntimeError('startup did not produce the expected output')
    elif child.returncode != 1 or child.stdout or b'divide by zero' not in child.stderr:
        raise RuntimeError('arithmetic error did not produce the expected diagnostic')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('binary', type=Path)
    parser.add_argument('scenario', choices=('startup', 'error', 'interactive'))
    parser.add_argument('--repetitions', type=int, default=256)
    args = parser.parse_args()
    if not 1 <= args.repetitions <= 4096:
        parser.error('--repetitions must be between 1 and 4096')
    binary = args.binary.resolve(strict=True)
    environment = {key: value for key, value in os.environ.items()
                   if not key.startswith(('BC_', 'DC_', 'GD_BC_PROFILE_'))}
    environment.update(LC_ALL='C', BC_ENV_ARGS='', BC_LINE_LENGTH='70')
    for _ in range(args.repetitions):
        check(binary, args.scenario, environment)
    print(f'{args.scenario}: {args.repetitions} checked children')


if __name__ == '__main__':
    main()
