#!/usr/bin/env python3
"""Collect CPU samples for exact archived Windows binaries and checked inputs."""
# SPDX-License-Identifier: Apache-2.0
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
from zipfile import ZipFile

ARCHIVE = '4319536abb8119393378600923191cdbd9318aad03359af54ce40378d7e5b5aa'
BASE = '19ef80161fd2bdb24bf9ba7e93942caa97ea6ea1'
CANDIDATE = 'a0ca39e2ecafd95d1ee67280673bc329a2a817ec'
PREFIX = 'qualification/performance/evidence/'
ROLES = ('base', 'candidate')
LAUNCHES = 64
WORKLOADS = ('file_statements_20000', 'streaming_statements_20000')
PLAN = (('file', 0, False), ('same-input-stdin', 0, True), ('stream', 1, True))


def digest(data):
    return hashlib.sha256(data).hexdigest()


def inputs(path):
    if digest(path.read_bytes()) != ARCHIVE:
        raise ValueError('unexpected input archive digest')
    with ZipFile(path) as archive:
        codegen = json.loads(archive.read(PREFIX + 'codegen/manifest.json'))
        manifest = json.loads(archive.read(PREFIX + 'full-paired/manifest.json'))
        programs = {case['name']: case for case in manifest['corpus']}
        binaries = {}
        for role, revision in zip(ROLES, (BASE, CANDIDATE)):
            row, = [row for row in codegen['observations'] if row['name'] == role]
            tool = row['implementation']
            if tool['metadata']['source_revision'] != revision or tool['metadata']['instrumented']:
                raise ValueError('unexpected binary provenance')
            argv = tool['argv']
            if len(argv) != 1:
                raise ValueError('unexpected archived invocation arguments')
            member = PREFIX + 'codegen/' + row['binary'].replace(chr(92), '/')
            binary = archive.read(member)
            if digest(binary) != row['binary_sha256']:
                raise ValueError('archived binary hash mismatch')
            binaries[role] = binary
        cases = []
        for name in WORKLOADS:
            stem = PREFIX + 'full-paired/workloads/' + name
            program, expected = archive.read(stem + '.bc'), archive.read(stem + '.expected')
            if (digest(program), digest(expected)) != (
                    programs[name]['program_sha256'], programs[name]['oracle_sha256']):
                raise ValueError('archived program or oracle hash mismatch')
            cases.append((program, expected))
    return binaries, cases


def checked_run(exe, path, program, expected, use_stdin):
    args = [str(exe)] if use_stdin else [str(exe), str(path)]
    environment = {key: value for key, value in os.environ.items()
                   if not key.startswith(('BC_', 'DC_'))}
    environment.update(LC_ALL='C', LANG='C', BC_LINE_LENGTH='70', BC_ENV_ARGS='')
    options = dict(stdin=subprocess.PIPE if use_stdin else subprocess.DEVNULL,
                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=environment)
    with subprocess.Popen(args, **options) as child:
        try:
            stdout, stderr = child.communicate(program if use_stdin else None, timeout=10)
        except subprocess.TimeoutExpired:
            child.kill()
            child.communicate()
            raise
        if child.returncode or stderr or stdout.replace(b'\r\n', b'\n') != expected:
            raise ValueError('bc output or exit did not match the frozen oracle')
        return child.pid


def invoke(*command, timeout=120):
    subprocess.run(command, check=True, timeout=timeout)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('archive', type=Path)
    parser.add_argument('decoder', type=Path)
    parser.add_argument('output', type=Path)
    parser.add_argument('--verify-only', action='store_true')
    args = parser.parse_args()
    binaries, cases = inputs(args.archive)
    if args.verify_only:
        print('Verified two original binaries and two checked programs')
        return
    if os.name != 'nt' or os.environ.get('GITHUB_RUN_ATTEMPT') != '1':
        parser.error('the fixed CPU diagnostic runs once on a Windows hosted runner')

    output = args.output.resolve()
    output.mkdir()
    report = dict(scope='sampled CPU attribution; no elapsed or acceptance claim',
                  archive_sha256=ARCHIVE, base=BASE, candidate=CANDIDATE,
                  launches_per_stage=LAUNCHES, status='incomplete', stages=[],
                  binaries={role:digest(raw) for role,raw in binaries.items()},
                  inputs={name:dict(program_sha256=digest(program), oracle_sha256=digest(expected))
                          for name,(program,expected) in zip(WORKLOADS,cases)})
    wpr = shutil.which('wpr.exe')
    try:
        if wpr is None:
            raise FileNotFoundError('Windows Performance Recorder is unavailable')
        with tempfile.TemporaryDirectory(prefix='bc-checked-etw-') as temporary:
            work = Path(temporary)
            executable = work / 'bc.exe'
            path = work / 'input.bc'
            trace = work / 'profile.etl'
            pid_file = work / 'pids.json'
            for block, roles in enumerate((ROLES, tuple(reversed(ROLES)))):
                for mode, index, use_stdin in PLAN:
                    program, expected = cases[index]
                    path.write_bytes(program)
                    for role in roles:
                        executable.write_bytes(binaries[role])
                        if digest(executable.read_bytes()) != report['binaries'][role]:
                            raise ValueError('deployed binary hash mismatch')
                        stage = dict(block=block, role=role, mode=mode, status='collecting')
                        report['stages'].append(stage)
                        invoke(wpr, '-start', 'CPU.light', '-filemode', '-instancename', 'bc-profile')
                        try:
                            pids = [checked_run(executable, path, program, expected, use_stdin)
                                    for _ in range(LAUNCHES)]
                        finally:
                            invoke(wpr, '-stop', str(trace), '-instancename', 'bc-profile')
                        if len(set(pids)) != LAUNCHES:
                            raise ValueError('a child PID was reused within the trace')
                        pid_file.write_text(json.dumps(pids), encoding='utf-8')
                        name = f'{block}-{mode}-{role}.json'
                        invoke('dotnet', str(args.decoder.resolve()), str(trace), str(pid_file),
                               str(output / name))
                        trace.unlink()
                        stage.update(status='collected', report=name, checked_launches=len(pids))
        report['status'] = 'collected'
    except (OSError, ValueError, subprocess.SubprocessError) as error:
        report['error'] = type(error).__name__
        raise
    finally:
        (output / 'manifest.json').write_text(json.dumps(report, indent=2)+'\n', encoding='utf-8')


if __name__ == '__main__':
    main()
