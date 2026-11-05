#!/usr/bin/env python3
"""Compare actual Clang VM layouts with the integer feature ON and OFF.

This checks a structural invariant, not runtime performance. Other compilers
retain portable semantic/ownership tests; unavailable record evidence skips.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re
import shlex
import subprocess
import sys
import tempfile


class UnavailableEvidence(Exception):
    pass


def records(text):
    result, active = {}, None
    for line in text.splitlines():
        start = re.fullmatch(r'\s*0 \| (?:class|struct) GD::Bc::Details::(FunctionCode|VMState)', line)
        if start:
            active = start.group(1)
            result[active] = dict(fields={})
            continue
        if active is None:
            continue
        field = re.fullmatch(r'\s*(\d+) \|   \S.*? ([A-Za-z_][A-Za-z_0-9]*)', line)
        if field:
            result[active]['fields'][field.group(2)] = int(field.group(1))
        size = re.search(r'\[sizeof=(\d+)', line)
        if size:
            result[active]['size'] = int(size.group(1))
            active = None
    if set(result) != {'FunctionCode', 'VMState'}:
        raise ValueError('missing actual VM/function records')
    for item in result.values():
        if not item.get('size') or not item['fields']:
            raise ValueError('incomplete compiler layout record')
    if not {'functions_', 'variables_', 'parameters_', 'saved_locals_', 'result_buffers_'}.issubset(result['VMState']['fields']):
        raise ValueError('missing common VM fields')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--compile-commands', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    output = Path(tempfile.mkdtemp(prefix='run-', dir=str(args.output)))
    receipt = dict(scope='compiler layout evidence only; not runtime acceptance', status='running')
    status = 1
    try:
        if not args.compile_commands.is_file():
            raise UnavailableEvidence('generator supplied no compilation database')
        rows = json.loads(args.compile_commands.read_text())
        row = next(row for row in rows if str(row.get('file', '')).replace('\\', '/').endswith('/src/bc/vm.cc'))
        receipt['source_vm_sha256'] = hashlib.sha256(Path(row['file']).read_bytes()).hexdigest()
        receipt['compile_commands_sha256'] = hashlib.sha256(args.compile_commands.read_bytes()).hexdigest()
        command = row.get('arguments') or shlex.split(row['command'])
        version = subprocess.run([command[0], '--version'], stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE, universal_newlines=True, timeout=10)
        if version.returncode or 'clang' not in version.stdout.lower() or any(x.startswith('/D') for x in command):
            receipt.update(status='unavailable', reason='requires direct Clang GNU-style compiler commands')
            status = 77
        else:
            receipt['compiler'] = version.stdout.splitlines()[0]
            clean, i = [], 0
            while i < len(command):
                arg = command[i]
                if arg in ('-o', '-MF', '-MT', '-MQ'):
                    i += 2
                    continue
                if arg not in ('-c', '-MD', '-MMD', '-MP') and not re.match(r'^-DGD_BC_INTEGER_FUNCTIONS(?:=|$)', arg):
                    clean.append(arg)
                i += 1
            modes = {}
            for enabled in (False, True):
                argv = clean[:]
                if enabled:
                    argv.insert(1, '-DGD_BC_INTEGER_FUNCTIONS=1')
                argv += ['-Xclang', '-fdump-record-layouts', '-fsyntax-only']
                run = subprocess.run(argv, cwd=row['directory'], stdout=subprocess.PIPE,
                                     stderr=subprocess.PIPE, universal_newlines=True, timeout=60)
                label = 'ON' if enabled else 'OFF'
                (output / (label + '-stderr.txt')).write_text(run.stderr)
                if run.returncode:
                    raise ValueError('compiler record collection failed for ' + label)
                modes[label] = dict(argv=argv, cwd=row['directory'], records=records(run.stdout))
            receipt['modes'] = modes
            off, on = modes['OFF']['records'], modes['ON']['records']
            has_integer = lambda data: any(name.startswith('integer_')
                                           for item in data.values() for name in item['fields'])
            if has_integer(off) or not has_integer(on):
                raise ValueError('compiler records do not prove the requested feature modes')
            differences = []
            if off['FunctionCode'] != on['FunctionCode']:
                differences.append('common FunctionCode layout changes')
            for field, offset in off['VMState']['fields'].items():
                if on['VMState']['fields'].get(field) != offset:
                    differences.append('common VM offset changes: ' + field)
            receipt.update(differences=differences, status='fail' if differences else 'pass')
            status = 1 if differences else 0
    except UnavailableEvidence as error:
        receipt.update(status='unavailable', reason=str(error))
        status = 77
    except (OSError, ValueError, KeyError, StopIteration, subprocess.TimeoutExpired) as error:
        receipt.update(status='error', error=str(error))
    (output / 'report.json').write_text(json.dumps(receipt, indent=2, sort_keys=True) + '\n')
    print(str(output / 'report.json'))
    return status


if __name__ == '__main__':
    sys.exit(main())
