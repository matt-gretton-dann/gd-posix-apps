#!/usr/bin/env python3
"""Replay retained VM experiment records without converting losses to acceptance."""
import hashlib
import importlib.util
import json
from pathlib import Path
import re
import zipfile

ROOT = Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location(
    'divisor_native_evidence', ROOT.parent / 'bc-divisor-native-evidence/verify.py')
BASE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(BASE)
require = BASE.require
# Keep each matrix tied to its immutable checkpoint; a smaller subset cannot
# replace either experimental full/holdout collection.
CHECKPOINTS = {
    '423-d89': ('611982305419b7a5f38f1300d6ee256256b0e905',
                'd89d17c51ea3e9792df80256ee9553a192c54760', {'full': 66, 'holdout': 66}, True),
    '423-f2': ('611982305419b7a5f38f1300d6ee256256b0e905',
               'f2bfdff902c49ed35ebb8f84a81f6b657499af0f', {'full': 66, 'holdout': 66}, True),
    '427-fa': ('2c58811161227c5ec9fc8753b57f639faae5ea17',
               'fa2420cc57d51b6e578fd7379282b02cff3ae266', {'full': 44}, False),
}


def activation(data, role, expected):
    metadata = json.loads(data[role + '/implementations.json'])['implementations'][0]['metadata']
    policy = metadata['configuration'].get('prepared_divisors_policy')
    if expected:
        require(policy and policy['status'] == 'recorded' and policy['enabled'] is True and
                policy['mode'] == 'ON', 'explicit candidate activation missing')
    else:
        # These historical sources predate the VM option. Preserve unknown
        # activation metadata rather than relabelling missing evidence OFF.
        require(policy is None or (policy['status'] == 'legacy-auto' and
                policy['enabled'] is None), 'legacy activation differs')
    commands = data[role + '/compile_commands.json']
    if expected:
        require(policy['verification'] == dict(kind='compile-commands',
                sha256=hashlib.sha256(commands).hexdigest(), translation_units=2),
                'activation command identity differs')
    selected = []
    define = re.compile(r'''(?:^|[\s"'])[-/]D\s*["']?GD_BC_PREPARED_DIVISORS\b(?:=([^\s"';]+))?''')
    undefine = re.compile(r'''(?:^|[\s"'])[-/]U\s*["']?GD_BC_PREPARED_DIVISORS\b''')
    for row in json.loads(commands):
        filename = '/' + row['file'].replace('\\', '/').lstrip('/')
        if not filename.endswith(('/src/bc/vm.cc', '/src/bc/bc.cc')):
            continue
        command = ' '.join(row['arguments']) if 'arguments' in row else row['command']
        values = define.findall(command)
        require(not undefine.search(command) and (values == ['1'] if expected else not values),
                'compiled activation differs')
        selected.append(filename.rsplit('/', 1)[1])
    require(sorted(selected) == ['bc.cc', 'vm.cc'], 'activation target inventory differs')


def verify(data, entry):
    base, candidate, corpora, enabled = CHECKPOINTS[entry['checkpoint']]
    require(entry['base'] == base and entry['candidate'] == candidate and entry['complete'] is True,
            'checkpoint contract differs')
    result = BASE.verify(data, entry, corpora=corpora)
    for role in ('base', 'candidate', 'aa'):
        activation(data, role, enabled and role == 'candidate')
    return result


def failed_test(data, entry):
    require(entry['source_revision'] == CHECKPOINTS['423-d89'][1] and
            entry['run_id'] == 41570060439 and entry['conclusion'] == 'failure',
            'failed test identity differs')
    require(hashlib.sha256(data).hexdigest() == entry['sha256'] and len(data) == entry['bytes'],
            'failed test log differs')


def verify_transport():
    hashes = json.loads((ROOT / 'sha256.json').read_text())
    actual = {str(p.relative_to(ROOT)) for p in ROOT.rglob('*')
              if p.is_file() and '__pycache__' not in p.parts and p.name != 'sha256.json'}
    require(actual == set(hashes), 'package payload inventory differs')
    for name, expected in hashes.items():
        require(hashlib.sha256((ROOT / name).read_bytes()).hexdigest() == expected,
                'package payload identity differs: ' + name)


def main():
    verify_transport()
    configuration = json.loads((ROOT / 'checkpoints.json').read_text())
    entries = configuration['artifacts']
    require(len(entries) == 9 and {(e['checkpoint'], e['host']) for e in entries} ==
            {(checkpoint, host) for checkpoint in CHECKPOINTS
             for host in ('linux', 'macos', 'windows')}, 'checkpoint inventory differs')
    reports = [verify(BASE.payloads(ROOT / e['path'], e['sha256']), e) for e in entries]
    entry = configuration['failed_test']
    with zipfile.ZipFile(ROOT / 'failed-test.zip') as source:
        require(source.namelist() == [entry['file']], 'failed test inventory differs')
        failed_test(source.read(entry['file']), entry)
    result = dict(artifacts=9, checked=sum(r['checked'] for r in reports), binaries=27,
                  inspections=54, reports=reports, performance_acceptance=False,
                  scope='Two broad experimental checkpoints and one distinct 44-case foundation subset')
    require(result == json.loads((ROOT / 'audit-summary.json').read_text()),
            'published audit summary differs')
    print('{} checked observations; rejected results retained; no pooled acceptance'.format(result['checked']))
    return result


if __name__ == '__main__':
    main()
