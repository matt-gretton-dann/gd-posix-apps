#!/usr/bin/env python3
"""Verify archived SHA-256 payloads and Windows Git checkout byte stability.

Run from any directory; --index checks staged blobs before committing a policy
repair. This changes neither the worktree nor persistent Git configuration.
Python 3.9+; SPDX-License-Identifier: Apache-2.0
"""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess
import sys


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--index', action='store_true')
    args = parser.parse_args()
    repo = Path(__file__).resolve().parents[2]
    tracked = subprocess.check_output(
        ['git', 'ls-files', '-z', '--', 'docs/utilities'], cwd=repo).decode().split('\0')
    manifests = [name for name in tracked if name.endswith('/sha256.json')]
    failures, checked = [], 0
    for name in manifests:
        directory = (repo / name).parent
        entries = json.loads((repo / name).read_text(encoding='utf-8'))
        for relative, expected in entries.items():
            path = directory / relative
            if hashlib.sha256(path.read_bytes()).hexdigest() != expected:
                failures.append(str(path.relative_to(repo)) + ': local payload hash differs')
            checked += 1
        # Exercise Git's actual checkout conversion on recorded JSON metadata.
        # Verifying only local bytes would miss core.autocrlf conversion elsewhere.
        sample = next((key for key in entries if key.endswith('manifest.json')),
                      next((key for key in entries if key.endswith('.json')),
                           next(iter(entries), None)))
        if sample is None:
            failures.append(name + ': empty hash manifest')
            continue
        path = (directory / sample).relative_to(repo).as_posix()
        revision = ':' + path if args.index else 'HEAD:' + path
        raw = subprocess.check_output(
            ['git', '-c', 'core.autocrlf=true', 'cat-file', '--filters', revision], cwd=repo)
        if hashlib.sha256(raw).hexdigest() != entries[sample]:
            failures.append(path + ': Windows checkout changes recorded bytes')
    for failure in failures:
        print(failure, file=sys.stderr)
    print('{} payload hashes, {} checkout simulations, {} failures'.format(
        checked, len(manifests), len(failures)))
    return int(bool(failures))


if __name__ == '__main__':
    sys.exit(main())
