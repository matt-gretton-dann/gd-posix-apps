#!/usr/bin/env python3
"""Replay immutable native runtime experiments without executing archived code."""
import hashlib
import importlib.util
import json
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location(
    'divisor_evidence', ROOT.parent / 'bc-divisor-native-evidence/verify.py')
BASE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(BASE)
require = BASE.require

# Source and corpus contracts are independent of the mutable artifact index.
CHECKPOINTS = {
    "430-5b": {
        "base": "97c0c7bcb90f8b7853e360bfc9d1420e3369e865",
        "candidate": "5b0e2569269d0d3d99a1da334ad883105fdef2e3",
        "corpora": {
            "full": 66,
            "holdout": 66
        }
    },
    "430-e2": {
        "base": "fd0ba3503c8a68520f149d73f4d973c17ae0dc14",
        "candidate": "e2ecdb2907ab899ec3bc9529d4d967fa6316b205",
        "corpora": {
            "full": 66,
            "holdout": 66
        }
    },
    "433-8e": {
        "base": "e2eb862d1294505cb993edc41081e783ce11d55f",
        "candidate": "8e996fc32f5e0c3e9e2936de56ea1de8c113e7ff",
        "corpora": {
            "full": 66,
            "holdout": 66
        }
    },
    "435-182": {
        "base": "e2eb862d1294505cb993edc41081e783ce11d55f",
        "candidate": "182d5cdf19fd54ce3cebbe2a884ef1f2559acaa7",
        "corpora": {
            "full": 52
        }
    },
    "433-5d": {
        "base": "86e153f6e2371882e9c0f470bb70aaad282848d8",
        "candidate": "5d0e056711f1a9744893c74fccf4e2a6d1abb798",
        "corpora": {
            "full": 66,
            "holdout": 66
        }
    },
    "436-5f": {
        "base": "609d1e08943a0e6e019157f4e1dbf0b57c9efb9f",
        "candidate": "5f7c73166687c58e8e340fd550d38cf9c7da6874",
        "corpora": {
            "full": 66,
            "holdout": 66
        }
    },
    "437-b6": {
        "base": "609d1e08943a0e6e019157f4e1dbf0b57c9efb9f",
        "candidate": "b628199eec24e1a673f294c5c9361e87d0e562c3",
        "corpora": {
            "full": 66,
            "holdout": 66
        }
    },
    "438-d8": {
        "base": "ced17fb74622b5c0b7cb2b43de8fa445e97f8e8e",
        "candidate": "d8f5312f29d81153033dcad052cae44b833d0ffe",
        "corpora": {
            "full": 66,
            "holdout": 66
        }
    },
    "440-b5": {
        "base": "ced17fb74622b5c0b7cb2b43de8fa445e97f8e8e",
        "candidate": "b54f0c3b714a948f8769227def16fd3f97bb1732",
        "corpora": {"full": 66, "holdout": 66},
        "partial_hosts": ["windows"]
    },
    "438-46": {
        "base": "ced17fb74622b5c0b7cb2b43de8fa445e97f8e8e",
        "candidate": "46bf0ff00e3e0df56a2ce3a6c322bec3dd205d49",
        "corpora": {
            "full": 66,
            "holdout": 66
        }
    }
}


def definitions(command, name):
    require(not re.search(r'''(?:^|[\s"'])[-/]U\s*["']?''' + name + r'\b', command),
            'unexpected mode undefinition: ' + name)
    return re.findall(r'''(?:^|[\s"'])[-/]D\s*["']?''' + name +
                      r'''\b(?:=([^\s"';]+))?''', command)


def modes(data, checkpoint, role):
    """Bind policy metadata to actual VM/CLI commands, including legacy unknowns."""
    metadata = json.loads(data[role + '/implementations.json'])['implementations'][0]['metadata']
    commands = data[role + '/compile_commands.json']
    policy = metadata['configuration'].get('prepared_divisors_policy')
    legacy = checkpoint.startswith('430-') and role != 'candidate'
    enabled = checkpoint.startswith('430-') and role == 'candidate'
    if legacy:
        require(policy and policy.get('status') == 'legacy-auto' and
                policy.get('enabled') is None and policy.get('mode') == 'AUTO',
                'legacy activation differs')
    else:
        require(policy and policy.get('status') == 'recorded' and
                policy.get('enabled') is enabled and
                policy.get('mode') == ('ON' if enabled else 'AUTO'),
                'explicit activation differs')
        require(policy.get('verification') == dict(kind='compile-commands',
                sha256=hashlib.sha256(commands).hexdigest(), translation_units=2),
                'activation command identity differs')
    compact = checkpoint.startswith(('438-', '440-')) or (checkpoint == '437-b6' and role == 'candidate')
    live = checkpoint == '436-5f' and role == 'candidate'
    fused = checkpoint.startswith('438-') and role == 'candidate'
    selected = []
    for row in json.loads(commands):
        filename = row['file'].replace('\\', '/').rsplit('/', 1)[-1]
        if not row['file'].replace('\\', '/').endswith(('/src/bc/vm.cc', '/src/bc/bc.cc')):
            continue
        command = ' '.join(row['arguments']) if 'arguments' in row else row['command']
        expected = [] if legacy else ['1' if enabled else '0']
        require(definitions(command, 'GD_BC_PREPARED_DIVISORS') == expected,
                'compiled divisor mode differs')
        require(definitions(command, 'ENABLE_EXTENSIONS') == ['1'],
                'compiled language mode differs')
        # These experimental VM-private switches do not propagate to the CLI.
        for name, active in (('GD_BC_COMPACT_EXECUTION', compact),
                             ('GD_BC_LIVE_REGISTERS', live), ('GD_BC_FUSED_LOADS', fused)):
            require(definitions(command, name) == (['1'] if active and filename == 'vm.cc' else []),
                    'compiled execution mode differs: ' + name)
        selected.append(filename)
    require(sorted(selected) == ['bc.cc', 'vm.cc'], 'mode target inventory differs')


def verify(data, entry):
    contract = CHECKPOINTS[entry['checkpoint']]
    complete = entry['host'] not in contract.get('partial_hosts', [])
    require(entry['base'] == contract['base'] and entry['candidate'] == contract['candidate'] and
            entry['complete'] is complete, 'checkpoint contract differs')
    result = BASE.verify(data, entry, corpora=contract['corpora'])
    for role in ('base', 'candidate', 'aa'):
        modes(data, entry['checkpoint'], role)
    if not complete:
        require(result['missing'] == [{'directory': 'holdout-paired', 'missing_samples': 1167}],
                'partial scope differs')
        result['partial_full_preset'] = partial_full_preset(data)
    return result


def partial_full_preset(data):
    """Retain reproduced full-preset losses even without final decisions.json."""
    expected = {'multiply_1000d_x10000', 'multiply_10000d_x1000',
                'multiply_dense_1000d_x5000', 'multiply_dense_10000d_x500',
                'multiply_unbalanced_10000d_100d_x2000', 'multiply_sparse_10000d_x1000'}
    initial = {r['name']: r['candidate_over_reference']['candidate']
               for r in json.loads(data['full-paired/summary.json'])['workloads']}
    repeats = json.loads(data['full-paired-confirmation/summary.json'])['workloads']
    require({r['name'] for r in repeats} == expected | {'hex_input_100000d_x3'} and len(repeats) == 7,
            'partial confirmation inventory differs')
    losses = []
    for row in repeats:
        first, second = initial[row['name']], row['candidate_over_reference']['candidate']
        if BASE.POLICY.bounds(first)[0] > 1.05 and BASE.POLICY.bounds(second)[0] > 1.05:
            losses.append(dict(name=row['name'], ratio=first, confirmation=second))
    require({r['name'] for r in losses} == expected, 'partial reproduced losses differ')
    return dict(reproduced_losses=losses,
                scope='Complete full-preset initial/confirmation intervals only; no complete decisions or held-out gate')


def verify_transport():
    hashes = json.loads((ROOT / 'sha256.json').read_text())
    actual = {p.relative_to(ROOT).as_posix() for p in ROOT.rglob('*')
              if p.is_file() and '__pycache__' not in p.parts and p.name != 'sha256.json'}
    require(actual == set(hashes), 'package payload inventory differs')
    for name, expected in hashes.items():
        require(hashlib.sha256((ROOT / name).read_bytes()).hexdigest() == expected,
                'package payload identity differs: ' + name)


def failed_checks(record):
    require(record.get('source_revision') == 'b54f0c3b714a948f8769227def16fd3f97bb1732' and
            record.get('run_id') == 41628715797, 'failed checkpoint identity differs')
    jobs = record.get('jobs', [])
    require(len(jobs) == 2 and {j['id'] for j in jobs} == {125892513572, 125892513595},
            'failed job inventory differs')
    for job in jobs:
        require(job.get('conclusion') == 'failure', 'failed outcome differs')
        diagnostic = '\n'.join(job.get('diagnostic_lines', []))
        expected = ('bc-368-cancel-division/README.md: local payload hash differs'
                    if job['id'] == 125892513572 else '[-Werror=mismatched-new-delete]')
        require(expected in diagnostic, 'failed diagnostic missing')


def collect():
    entries = json.loads((ROOT / 'checkpoints.json').read_text())['artifacts']
    inventory = {(e['checkpoint'], e['host']) for e in entries}
    require(len(entries) == 3 * len(CHECKPOINTS) and inventory ==
            {(c, h) for c in CHECKPOINTS for h in ('linux', 'macos', 'windows')},
            'checkpoint inventory differs')
    reports = [verify(BASE.payloads(ROOT / e['path'], e['sha256']), e) for e in entries]
    failed_checks(json.loads((ROOT / 'failed-checks.json').read_text()))
    return dict(artifacts=len(entries), checked=sum(r['checked'] for r in reports),
                binaries=3 * len(entries), inspections=6 * len(entries), reports=reports,
                failed_job_excerpts=2,
                performance_acceptance=False,
                scope='Distinct immutable native checkpoints; no pooled performance or adoption claim')


def main():
    verify_transport()
    result = collect()
    require(result == json.loads((ROOT / 'audit-summary.json').read_text()),
            'published audit summary differs')
    print('{} checked observations; all losses and control qualifications retained'.format(result['checked']))
    return result


if __name__ == '__main__':
    main()
