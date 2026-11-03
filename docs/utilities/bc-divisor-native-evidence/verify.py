#!/usr/bin/env python3
"""Verify complete and partial native records without pooling acceptance.

Replay the recorded nominal intervals through the frozen symmetric policy.
Do not regenerate bootstrap intervals, execute archived binaries or infer
performance acceptance from either retained counts or successful verification.
"""
from collections import Counter
import hashlib
import importlib.util
import json
import math
from pathlib import Path, PureWindowsPath
import re
import zipfile

ROOT = Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location('divisor_policy', ROOT / 'policy.py')
POLICY = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(POLICY)


def digest(data):
    return hashlib.sha256(data).hexdigest()


def require(condition, message):
    if not condition:
        raise ValueError(message)


def payloads(path, expected):
    require(digest(path.read_bytes()) == expected, 'archive identity differs')
    with zipfile.ZipFile(path) as source:
        names = source.namelist()
        require(len(names) == len(set(names)), 'duplicate ZIP members')
        return {name: source.read(name) for name in names}


def verify(data, entry, corpora=None):
    # Existing divisor checkpoints retain their original 58-case full/holdout
    # contract. Other archives must supply their own explicit preset inventory.
    corpora = {'full': 58, 'holdout': 58} if corpora is None else corpora
    require(corpora and set(corpora) <= {'full', 'holdout'} and
            all(type(count) is int and count > 0 for count in corpora.values()),
            'invalid corpus contract')
    base_grids = {preset + '-' + comparison for preset in corpora
                  for comparison in ('aa', 'paired')}
    recipes = {}
    for role in ('base', 'candidate', 'aa'):
        metadata = json.loads(data[role + '/implementations.json'])['implementations'][0]['metadata']
        require(metadata['source_revision'] == entry['candidate' if role == 'candidate' else 'base'],
                'source revision differs')
        require(metadata['instrumented'] is False and metadata['configuration']['build_type'] == 'Release' and
                metadata['configuration']['multiply_split_point'] == 126, 'instrumented or unexpected build')
        header = data[role + '/multiply-threshold.hh']
        require(digest(header) == metadata['configuration']['threshold_header_sha256'] and
                b'#define BC_MULTIPLY_SPLIT_POINT 126' in header, 'threshold evidence differs')
        flags = json.dumps(metadata['flags'])
        if role + '/compile_commands.json' in data:
            commands = data[role + '/compile_commands.json']
            require(digest(commands) == metadata['flags']['compile_commands_sha256'], 'compile commands differ')
            flags += commands.decode()
        require(not re.search(r'-fprofile(?:-instr)?-generate|-fprofile-arcs|-ftest-coverage|-fcoverage|--coverage|-fsanitize|/fsanitize|GD_BC_PROFILE=1', flags, re.I),
                'instrumented command')
        recipes[role] = metadata
    grids, missing, decision_keys = {}, [], set()
    for name in sorted(data):
        if not name.endswith('/samples.jsonl'):
            continue
        directory = name.rsplit('/', 1)[0]
        manifest = json.loads(data[directory + '/manifest.json'])
        if directory in base_grids:
            preset = directory.split('-')[0]
            require(len(manifest['corpus']) == len({c['name'] for c in manifest['corpus']}) == corpora[preset] and
                    manifest['samples'] == 9 and manifest['warmups'] == 1, 'base corpus dimensions differ')
            preset, comparison = directory.split('-')
            decision_keys.update((preset, c['name'], comparison) for c in manifest['corpus'])
        for tool in manifest['implementations']:
            if tool['name'] in recipes:
                require(tool['metadata']['source_revision'] == recipes[tool['name']]['source_revision'] and
                        tool['binary_sha256'] == recipes[tool['name']]['binary_sha256'], 'sample role provenance differs')
        rows = [json.loads(line) for line in data[name].splitlines()]
        require(rows and all(row['checked'] is True for row in rows), 'unchecked observations')
        keys = [(r['workload'], r['implementation'], r['phase'], r['round']) for r in rows]
        require(len(keys) == len(set(keys)), 'duplicate sample rounds')
        actual = Counter((r['workload'], r['implementation'], r['phase']) for r in rows)
        expected = Counter()
        for case in manifest['corpus']:
            for suffix, field in (('.bc', 'program_sha256'), ('.expected', 'oracle_sha256')):
                require(digest(data[directory + '/workloads/' + case['name'] + suffix]) == case[field],
                        'workload or oracle hash differs')
            for tool in manifest['implementations']:
                for phase, count in (('measurement', manifest['samples']), ('warmup', manifest['warmups'])):
                    expected[case['name'], tool['name'], phase] = count
        require(not (actual - expected), 'unexpected observations')
        for row in rows:
            require(type(row['round']) is int and 0 <= row['round'] < expected[row['workload'], row['implementation'], row['phase']],
                    'invalid sample round')
            require(isinstance(row['seconds'], (int, float)) and not isinstance(row['seconds'], bool) and
                    math.isfinite(row['seconds']) and row['seconds'] > 0, 'invalid measured duration')
        absent = sum((expected - actual).values())
        if absent:
            missing.append(dict(directory=directory,missing_samples=absent))
        if entry['complete']:
            require(actual == expected, 'incomplete grid declared complete')
        grids[directory] = len(rows)
    require(grids == entry['grid_counts'] and sum(grids.values()) == entry['checked'], 'published raw counts differ')
    require(base_grids <= set(grids), 'base grid missing')
    controls, losses = Counter(), []
    if entry['complete']:
        require('decisions.json' in data and not missing, 'completed decisions missing')
        decisions = json.loads(data['decisions.json'])
        recorded_keys = [(r['preset'], r['name'], r['comparison']) for r in decisions['results']]
        require(len(recorded_keys) == len(set(recorded_keys)) and set(recorded_keys) == decision_keys,
                'decision inventory differs')
        policy = (ROOT / 'policy.py').read_bytes()
        if entry['host'] == 'windows':
            policy = policy.replace(b'\n', b'\r\n')
        require(decisions['aa_policy_sha256'] == digest(policy), 'frozen policy identity differs')
        qualifiers = {}
        for row in decisions['results']:
            if row['comparison'] == 'aa':
                status = 'inconclusive-noisy' if row['name'] == 'startup' else POLICY.aa_status(row['ratio'], row['confirmation']).replace('inconclusive', 'inconclusive-noisy')
                require(row['status'] == row['aa_control_status'] == status, 'control qualification differs')
                qualifiers[row['preset'], row['name']] = status
        require(len(qualifiers) == sum(corpora.values()), 'control inventory differs')
        controls.update(qualifiers.values())
        for row in decisions['results']:
            require(row['aa_control_status'] == qualifiers[row['preset'], row['name']], 'lost control qualification')
            if row['comparison'] == 'paired':
                first = POLICY.bounds(row['ratio'])
                second = POLICY.bounds(row['confirmation']) if row['confirmation'] else None
                if row['name'] == 'startup' or first is None:
                    status = 'inconclusive-noisy'
                elif first[0] > 1.05:
                    status = ('confirmed regression' if row['name'] != 'startup' and second is not None and
                              second[0] > 1.05 else 'inconclusive-noisy')
                else:
                    status = 'pass' if first[1] <= 1.05 else 'inconclusive-noisy'
                require(status == row['status'], 'regression qualification differs')
                if status == 'confirmed regression':
                    losses.append(row)
    else:
        require('decisions.json' not in data and missing, 'partial record unexpectedly has complete decisions')
    code = json.loads(data['codegen/manifest.json'])
    require(code['status'] == 'preserved' and len(code['observations']) == 3 and
            {row['name'] for row in code['observations']} == set(recipes), 'binary inventory differs')
    for row in code['observations']:
        relative = PureWindowsPath(row['binary']).as_posix()
        binary = data['codegen/' + relative]
        require(digest(binary) == row['binary_sha256'] == recipes[row['name']]['binary_sha256'] and
                len(binary) == row['file_bytes'] and row['source_revision'] == recipes[row['name']]['source_revision'],
                'binary identity differs')
        native = row['disassembly']
        require(native['status'] == 'collected' and len(native['commands']) == 2, 'inspection inventory differs')
        for command in native['commands']:
            output = data['codegen/' + row['name'] + '/' + command['log']]
            require(command['returncode'] == 0 and digest(output) == command['log_sha256'] and
                    len(output) == command['log_bytes'], 'inspection log differs')
    return dict(checkpoint=entry['checkpoint'],host=entry['host'],complete=entry['complete'],checked=sum(grids.values()),
                missing=missing,controls=dict(controls),confirmed_regressions=losses,binaries=3,inspections=6,
                performance_acceptance=False)


def verify_failed_builds(data, index):
    expected = {125655598485, 125655598581, 125655598821, 125655599428}
    require(index['run_id'] == 41556208140 and len(index['jobs']) == 4 and
            {row['job_id'] for row in index['jobs']} == expected, 'failed job inventory differs')
    require(set(data) == {row['file'] for row in index['jobs']}, 'failed log inventory differs')
    for row in index['jobs']:
        require(row['conclusion'] == 'failure' and digest(data[row['file']]) == row['sha256'] and
                len(data[row['file']]) == row['bytes'], 'failed log identity differs')


def main():
    index = json.loads((ROOT / 'failed-ci-index.json').read_text())
    with zipfile.ZipFile(ROOT / 'failed-ci.zip') as source:
        verify_failed_builds({name: source.read(name) for name in source.namelist()}, index)
    configuration = json.loads((ROOT / 'checkpoints.json').read_text())
    entries = configuration['artifacts']
    require(len(entries) == 12 and len({(e['checkpoint'], e['host']) for e in entries}) == 12,
            'checkpoint inventory differs')
    reports = [verify(payloads(ROOT / entry['path'], entry['sha256']), entry) for entry in entries]
    result = dict(artifacts=12,checked=sum(row['checked'] for row in reports),
                  complete_artifacts=sum(row['complete'] for row in reports),
                  partial_artifacts=sum(not row['complete'] for row in reports),
                  binaries=36,inspections=72,reports=reports,performance_acceptance=False)
    require(result == json.loads((ROOT / 'audit-summary.json').read_text()), 'published audit summary differs')
    print('{} checked observations; {} complete and {} partial archives; no pooled acceptance'.format(
        result['checked'],result['complete_artifacts'],result['partial_artifacts']))
    return result


if __name__ == '__main__':
    main()
