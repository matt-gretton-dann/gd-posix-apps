#!/usr/bin/env python3
"""Read archived native evidence; preserve losses and requalify old controls.

This verifies raw provenance/counts/hashes and replays the frozen policy. It
uses the recorded nominal intervals, without regenerating bootstrap estimates
or turning a passing batch into universal performance acceptance.
SPDX-License-Identifier: Apache-2.0
"""
from collections import Counter
import hashlib
import importlib.machinery
import importlib.util
import json
from pathlib import Path, PureWindowsPath
import re
import sys
import zipfile

ROOT = Path(__file__).resolve().parent


def digest(data):
    return hashlib.sha256(data).hexdigest()


def main():
    policy_path = ROOT / 'sources/benchmark-policy.py.txt'
    loader = importlib.machinery.SourceFileLoader('archived_policy', str(policy_path))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    policy = importlib.util.module_from_spec(spec)
    sys.dont_write_bytecode = True
    loader.exec_module(policy)
    configuration = json.loads((ROOT / 'checkpoints.json').read_text())
    reports = []
    binaries = inspections = children = 0
    for checkpoint in configuration['checkpoints']:
        for host, archive in checkpoint['hosts'].items():
            path = ROOT / archive['path']
            assert digest(path.read_bytes()) == archive['sha256'], path
            checked = 0
            with zipfile.ZipFile(path) as z:
                recipes = {}
                for role in ('base', 'candidate', 'aa'):
                    m = json.loads(z.read(role + '/implementations.json'))['implementations'][0]['metadata']
                    expected = checkpoint['candidate' if role == 'candidate' else 'base']
                    assert m['source_revision'] == expected and m['instrumented'] is False
                    assert m['configuration']['build_type'] == 'Release'
                    assert m['configuration']['multiply_split_point'] == 126
                    header = z.read(role + '/multiply-threshold.hh')
                    assert digest(header) == m['configuration']['threshold_header_sha256']
                    assert b'#define BC_MULTIPLY_SPLIT_POINT 126' in header
                    flags = json.dumps(m['flags'])
                    commands = role + '/compile_commands.json'
                    if commands in z.namelist():
                        payload = z.read(commands)
                        assert digest(payload) == m['flags']['compile_commands_sha256']
                        flags += payload.decode()
                    assert not re.search(r'-fprofile(?:-instr)?-generate|-fprofile-arcs|-ftest-coverage|-fcoverage|--coverage|-fsanitize|/fsanitize|GD_BC_PROFILE=1', flags, re.I)
                    recipes[role] = m
                for name in z.namelist():
                    if not name.endswith('/samples.jsonl'):
                        continue
                    directory = name.rsplit('/', 1)[0]
                    m = json.loads(z.read(directory + '/manifest.json'))
                    rows = [json.loads(line) for line in z.read(name).decode().splitlines()]
                    assert rows and all(row['checked'] is True for row in rows)
                    actual = Counter((r['workload'], r['implementation'], r['phase']) for r in rows)
                    expected = Counter()
                    for case in m['corpus']:
                        for suffix, key in (('.bc', 'program_sha256'), ('.expected', 'oracle_sha256')):
                            assert digest(z.read(directory + '/workloads/' + case['name'] + suffix)) == case[key]
                        for tool in m['implementations']:
                            for phase, count in (('measurement', m['samples']), ('warmup', m['warmups'])):
                                expected[case['name'], tool['name'], phase] = count
                    assert actual == expected
                    checked += len(rows)
                assert checked >= checkpoint['minimum_checked']
                decisions = json.loads(z.read('decisions.json'))
                policy_bytes = policy_path.read_bytes()
                if checkpoint['recorded_symmetric_policy']:
                    if host == 'windows':
                        policy_bytes = policy_bytes.replace(b'\n', b'\r\n')
                    assert decisions['aa_policy_sha256'] == digest(policy_bytes)
                qualifiers = {}
                changed = 0
                for row in decisions['results']:
                    if row['comparison'] != 'aa':
                        continue
                    status = 'inconclusive-noisy' if row['name'] == 'startup' else policy.aa_status(row['ratio'], row['confirmation']).replace('inconclusive', 'inconclusive-noisy')
                    qualifiers[row['preset'], row['name']] = status
                    changed += status != row['status']
                    if checkpoint['recorded_symmetric_policy']:
                        assert row['status'] == row['aa_control_status'] == status
                if checkpoint['recorded_symmetric_policy']:
                    for row in decisions['results']:
                        assert row['aa_control_status'] == qualifiers[row['preset'], row['name']]
                losses = [r for r in decisions['results'] if r['comparison'] == 'paired' and 'confirmed regression' in r['status'].lower().replace('-', ' ')]
                for row in decisions['results']:
                    if row['comparison'] != 'paired':
                        continue
                    first = policy.bounds(row['ratio'])
                    second = policy.bounds(row['confirmation']) if row['confirmation'] else None
                    confirmed = row['name'] != 'startup' and first is not None and first[0] > 1.05 and second is not None and second[0] > 1.05
                    recorded = 'confirmed regression' in row['status'].lower().replace('-', ' ')
                    assert bool(confirmed) == recorded
                if checkpoint['candidate_toom3_threshold']:
                    threshold = checkpoint['candidate_toom3_threshold']
                    assert decisions['candidate_toom3_threshold'] == threshold
                    for role, m in recipes.items():
                        assert m['configuration']['toom3_threshold'] == (threshold if role == 'candidate' else 0)
                    exact = json.loads(z.read('toom-exact-oracles.json'))
                    assert len(exact['checked']) == 50 and all(r['checked'] for r in exact['checked'])
                    assert exact['binary_sha256'] == recipes['candidate']['binary_sha256']
                if checkpoint.get('codegen_available', True):
                    codegen = json.loads(z.read('codegen/manifest.json'))
                    assert codegen['status'] == 'preserved'
                    assert {r['name'] for r in codegen['observations']} == set(recipes)
                    for row in codegen['observations']:
                        role = row['name']
                        relative = PureWindowsPath(row['binary']).as_posix() if host == 'windows' else row['binary']
                        payload = z.read('codegen/' + relative)
                        assert digest(payload) == row['binary_sha256'] == recipes[role]['binary_sha256']
                        assert len(payload) == row['file_bytes'] and row['source_revision'] == recipes[role]['source_revision']
                        binaries += 1
                        native = row['disassembly']
                        assert native['status'] == 'collected' and len(native['commands']) == 2
                        for command in native['commands']:
                            payload = z.read('codegen/' + role + '/' + command['log'])
                            assert digest(payload) == command['log_sha256'] and len(payload) == command['log_bytes']
                            assert command['returncode'] == 0 and 0 < command['timeout_seconds'] <= 60
                            inspections += 1
                else:
                    assert 'codegen/manifest.json' not in z.namelist()
                host_info = json.loads(z.read('host.json'))
                cpu = None
                if host == 'windows':
                    detail = next(r for r in host_info['details'] if 'Win32_Processor' in ' '.join(r['argv']))
                    cpu = json.loads(detail['stdout'])['Name'].strip()
                reports.append(dict(checkpoint=checkpoint['name'], host=host, cpu=cpu,
                    checked=checked, controls=dict(Counter(qualifiers.values())),
                    changed_legacy_qualifiers=changed, confirmed_losses=losses,
                    checkpoint_status=checkpoint['status'], universal_acceptance=False))
                children += checked
    local_checked = 0
    with zipfile.ZipFile(ROOT / 'producer-paired.zip') as z:
        local = json.loads(z.read('audit.json'))
        for name in z.namelist():
            if not name.endswith('/samples.jsonl'):
                continue
            directory = name.rsplit('/', 1)[0]
            m = json.loads(z.read(directory + '/manifest.json'))
            rows = [json.loads(line) for line in z.read(name).decode().splitlines()]
            assert rows and all(r['checked'] is True for r in rows)
            actual = Counter((r['workload'], r['implementation'], r['phase']) for r in rows)
            expected = Counter()
            for case in m['corpus']:
                for suffix, key in (('.bc', 'program_sha256'), ('.expected', 'oracle_sha256')):
                    assert digest(z.read(directory + '/workloads/' + case['name'] + suffix)) == case[key]
                for tool in m['implementations']:
                    for phase, count in (('measurement', 15), ('warmup', 3)):
                        expected[case['name'], tool['name'], phase] = count
            assert m['samples'] == 15 and m['warmups'] == 3 and actual == expected
            for tool in m['implementations']:
                assert tool['metadata']['instrumented'] is False
                if tool['name'] in ('base', 'aa', 'candidate'):
                    expected_revision = '9e2c5d40a66ec13d8f87c1fd9ed9f3c1ca958b9d' if tool['name'] == 'candidate' else 'a5f83342a11d5044f48f41470178e55f99dd4d79'
                    assert tool['metadata']['source_revision'] == expected_revision
                    if m['suite'] == 'math':
                        assert len(tool['argv']) == 2 and '-l' not in tool['argv']
                        with zipfile.ZipFile(ROOT / 'producer-preparation.zip') as prepared:
                            assert digest(prepared.read('math-library.bc')) == tool['metadata']['library']['source_sha256']
                for artifact in tool.get('input_artifacts', []):
                    assert digest(z.read(directory + '/' + artifact['archived_as'])) == artifact['sha256']
            local_checked += len(rows)
        controls = json.loads(z.read('controls.json'))['results']
        regressions = json.loads(z.read('regressions.json'))['results']
        assert len(controls) == len(regressions) == 240
        for row in controls:
            assert row['status'] == ('inconclusive' if row['name'] == 'startup' else policy.aa_status(row['first'], row['confirmation']))
        assert not any(r['confirmed_regression'] for r in regressions)
        assert local_checked == local['checked'] == 19440
    with zipfile.ZipFile(ROOT / 'producer-codegen.zip') as z:
        m = json.loads(z.read('manifest.json'))
        assert m['status'] == 'preserved' and len(m['observations']) == 3
        for row in m['observations']:
            assert digest(z.read(row['binary'])) == row['binary_sha256']
            assert len(z.read(row['binary'])) == row['file_bytes']
            assert row['disassembly']['status'] == 'collected'
            for command in row['disassembly']['commands']:
                payload = z.read(row['name'] + '/' + command['log'])
                assert digest(payload) == command['log_sha256'] and command['returncode'] == 0
    with zipfile.ZipFile(ROOT / 'producer-resources.zip') as z:
        resource = json.loads(z.read('manifest.json'))
        assert len(resource['samples']) == 48 and all(r['checked'] for r in resource['samples'])
    summary = dict(native_checked=children, native_binaries=binaries,
                   native_inspection_outputs=inspections, local_producer_checked=local_checked, local_exact_binaries=3, local_native_outputs=6,
                   local_resource_observations=48, reports=reports,
                   limitations=['recorded nominal intervals are not recomputed',
                       'local producer raw counts/hashes/controls are replayed; original collector/auditor remain alongside them',
                       'different CPU models and batches are not pooled',
                       'successful batches do not waive retained losses',
                       'native math was unavailable on these main-based sources; not a math performance pass'])
    (ROOT / 'audit-summary.json').write_bytes((json.dumps(summary, indent=2, sort_keys=True) + '\n').encode('utf-8'))
    print('{} native checked children, {} exact binaries, {} native outputs; losses retained'.format(children, binaries, inspections))
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
