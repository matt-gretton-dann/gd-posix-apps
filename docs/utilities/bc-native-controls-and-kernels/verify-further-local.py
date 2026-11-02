#!/usr/bin/env python3
"""Verify retained local raw counts, fixed inputs and executable identities.

Recorded intervals and original audit decisions are retained; this verifier
neither recomputes their bootstrap estimates nor overrides native losses.
SPDX-License-Identifier: Apache-2.0
"""
from collections import Counter
import hashlib
import json
from pathlib import Path
import zipfile


def digest(data):
    return hashlib.sha256(data).hexdigest()


def verify(root):
    directory = root / 'further-local'
    counts = {}
    for name, expected_count in (('typed-paired', 20880), ('toom-training', 9414),
                                 ('toom-evaluation', 6660)):
        count = 0
        with zipfile.ZipFile(directory / (name + '.zip')) as z:
            for path in z.namelist():
                if not path.endswith('/samples.jsonl'):
                    continue
                prefix = path.rsplit('/', 1)[0] + '/'
                manifest = json.loads(z.read(prefix + 'manifest.json'))
                rows = [json.loads(line) for line in z.read(path).splitlines()]
                assert rows and all(row['checked'] is True for row in rows)
                actual = Counter((r['workload'], r['implementation'], r['phase']) for r in rows)
                expected = Counter()
                for case in manifest['corpus']:
                    for suffix, field in (('.bc', 'program_sha256'), ('.expected', 'oracle_sha256')):
                        assert digest(z.read(prefix + 'workloads/' + case['name'] + suffix)) == case[field]
                    for tool in manifest['implementations']:
                        assert tool['metadata']['instrumented'] is False
                        for phase, number in (('measurement', manifest['samples']),
                                              ('warmup', manifest['warmups'])):
                            expected[case['name'], tool['name'], phase] = number
                assert actual == expected
                count += len(rows)
            audit = json.loads(z.read('audit.json'))
            assert audit['checked'] == count == expected_count
        counts[name] = count
    # The 2048 selection was frozen from training before evaluation. Keep its
    # exact selection hash in evaluation rather than deriving it from holdout.
    with zipfile.ZipFile(directory / 'toom-training.zip') as training:
        selection = training.read('selection.json')
        value = json.loads(selection)
        assert value['threshold'] == 2048 and value['performance_acceptance'] is False
        assert value['training_audit_sha256'] == digest(training.read('audit.json'))
    with zipfile.ZipFile(directory / 'toom-evaluation.zip') as evaluation:
        assert json.loads(evaluation.read('protocol.json'))['selection_sha256'] == digest(selection)
    observations = {}
    for name, field, expected_count, hash_field in (
            ('typed-resources', 'samples', 216, None),
            ('typed-logical', 'results', 96, 'report_sha256'),
            ('toom-diagnostics', 'logical', 288, 'sha256'),
            ('toom-diagnostics', 'resources', 540, 'report_sha256')):
        with zipfile.ZipFile(directory / (name + '.zip')) as z:
            rows = json.loads(z.read('manifest.json'))[field]
            assert len(rows) == expected_count and all(r['checked'] is True for r in rows)
            for row in rows:
                payload = z.read(row['report'])
                if hash_field:
                    assert digest(payload) == row[hash_field]
                if field in ('results', 'logical'):
                    report = json.loads(payload)
                    assert report['instrumented'] is True and report['total']['live_bytes'] == 0
            observations[name + '/' + field] = len(rows)
    binaries = inspections = 0
    for experiment, bundle, expected_count in (('207-typed', 'typed-codegen', 3),
                                                ('204-toom', 'toom-codegen', 5)):
        with zipfile.ZipFile(directory / (bundle + '.zip')) as z:
            report = json.loads(z.read('manifest.json'))
            assert report['status'] == 'preserved' and len(report['observations']) == expected_count
            for row in report['observations']:
                role = row['name']
                recipe = directory / 'recipes' / experiment / role
                metadata = json.loads((recipe / 'implementations.json').read_text())['implementations'][0]['metadata']
                assert metadata['instrumented'] is False
                assert row['source_revision'] == metadata['source_revision']
                payload = z.read(row['binary'])
                assert digest(payload) == row['binary_sha256'] == metadata['binary_sha256']
                assert len(payload) == row['file_bytes']
                assert digest((recipe / 'compile_commands.json').read_bytes()) == metadata['flags']['compile_commands_sha256']
                assert digest((recipe / 'multiply-threshold.hh').read_bytes()) == metadata['configuration']['threshold_header_sha256']
                native = row['disassembly']
                assert native['status'] == 'collected' and len(native['commands']) == 2
                for command in native['commands']:
                    payload = z.read(role + '/' + command['log'])
                    assert digest(payload) == command['log_sha256'] and len(payload) == command['log_bytes']
                    assert command['returncode'] == 0 and 0 < command['timeout_seconds'] <= 60
                    inspections += 1
                binaries += 1
    return dict(timing_counts=counts, observations=observations,
                exact_binaries=binaries, native_inspection_outputs=inspections,
                performance_acceptance=False)


if __name__ == '__main__':
    print(json.dumps(verify(Path(__file__).resolve().parent), indent=2, sort_keys=True))
