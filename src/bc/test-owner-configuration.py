#!/usr/bin/env python3
"""Reject a thread-confined count request without its required compact owner."""
from pathlib import Path
import subprocess
import sys
import tempfile

with tempfile.TemporaryDirectory(prefix='bc-owner-config-') as directory:
    result = subprocess.run([
        sys.argv[1], '-S', str(Path(sys.argv[2]).resolve()), '-B', directory,
        '-DBUILD_TESTING=OFF', '-DENABLE_BC_COMPACT_DIGIT_OWNER=OFF',
        '-DENABLE_BC_THREAD_CONFINED_DIGIT_OWNER=ON'],
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT, universal_newlines=True,
        timeout=90)
    if result.returncode == 0 or 'Thread-confined ownership requires' not in result.stdout:
        raise RuntimeError('inconsistent ownership options were not rejected: ' + result.stdout)
print('Inconsistent thread-confined/shared_ptr ownership options rejected')
