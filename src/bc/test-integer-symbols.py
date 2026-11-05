#!/usr/bin/env python3
"""Check that the native integer object does not define Number ownership/math."""
import argparse
import hashlib
import json
from pathlib import Path
import re
import shutil
import subprocess
import tempfile


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--object', type=Path, required=True)
    parser.add_argument('--nm', default='nm')
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    root = Path(tempfile.mkdtemp(prefix='run-', dir=str(args.output)))
    report = dict(schema=1, collector_sha256=digest(Path(__file__)),
                  scope='native engine object only; not runtime performance or complete symbol coverage')

    def finish(status, code, **fields):
        report.update(status=status, **fields)
        (root / 'report.json').write_text(json.dumps(report, indent=2) + '\n', encoding='utf-8')
        print(str(root / 'report.json'))
        return code

    if not args.object.is_file():
        return finish('failure', 1, reason='engine object is missing')
    report.update(object=str(args.object), object_sha256=digest(args.object))
    tool = shutil.which(args.nm) if args.nm else None
    if not tool:
        return finish('unavailable', 77, reason='no compatible native symbol adapter')
    report.update(tool=tool, tool_sha256=digest(Path(tool)))
    command = [tool, '--defined-only', '--demangle', str(args.object)]
    report['argv'] = command
    try:
        run = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=30)
    except (OSError, subprocess.TimeoutExpired) as error:
        return finish('unavailable', 77, reason=str(error))
    (root / 'symbols.txt').write_bytes(run.stdout)
    (root / 'stderr.txt').write_bytes(run.stderr)
    if run.returncode:
        return finish('unavailable', 77, reason='symbol adapter rejected this object/options',
                      returncode=run.returncode)
    symbols = run.stdout.decode('utf-8', errors='replace')
    if not all(name + '(' in symbols for name in ('plan_integer_function', 'run_integer_function')):
        return finish('failure', 1, reason='selected object does not define the integer engine')
    arithmetic = re.compile(r'Basic(?:Digits|Number)<.*>::(?:add|sub|multiply(?:_[A-Za-z0-9]+)?)\(')
    # Ownership/conversion definitions can also escape a non-IPO object. Keep
    # every generic Number/Digits instantiation in the ordinary library context.
    number = re.compile(r'\bBasic(?:Digits|Number)<')
    forbidden = [line for line in symbols.splitlines() if number.search(line)]
    return finish('failure' if forbidden else 'pass', 1 if forbidden else 0,
                  arithmetic_definitions=[line for line in forbidden if arithmetic.search(line)],
                  number_definitions=forbidden)


if __name__ == '__main__':
    raise SystemExit(main())
