#!/usr/bin/env python3
"""Run the math CLI tests from the same checkout as the candidate library."""
# SPDX-License-Identifier: Apache-2.0
from pathlib import Path
import subprocess
import sys


def main():
    arguments = sys.argv[1:]
    library = Path(arguments[arguments.index('--source') + 1]).resolve()
    cli = library.with_name('test-math-library-cli.py')
    return subprocess.run([sys.executable, str(cli), *arguments]).returncode


if __name__ == '__main__':
    raise SystemExit(main())
