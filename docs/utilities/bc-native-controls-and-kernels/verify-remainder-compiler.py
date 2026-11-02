#!/usr/bin/env python3
"""Verify remainder work/RSS grids and native compiler-policy provenance.

Retain checked output records without rerunning host-specific executables.
Logical allocation peaks, local RSS and native timings remain separate evidence.
SPDX-License-Identifier: Apache-2.0
"""
from collections import Counter
import hashlib
import json
from pathlib import Path
import re
import zipfile

NAMES = {
    'full-modulo_1000d_350d_x3000',
    'full-modulo_scaled_numerator_s3_b7_t43_x1000',
    'full-modulo_scaled_divisor_s97_b37_t43_x1000',
    'holdout-modulo_1125d_393d_x2625',
    'holdout-modulo_scaled_numerator_s4_b8_t48_x875',
    'holdout-modulo_scaled_divisor_s109_b41_t48_x875',
}


def digest(data):
    return hashlib.sha256(data).hexdigest()


def verify(root):
    directory = root / 'remainder-compiler'
    logical_bytes = None
    for name, candidate in (
            ('initial-logical', '8cbb65a5f56099fde1fbdef2896fe8142c19f844'),
            ('final-logical', '46a7d68b2022b386f0e2f54e95aa1a81d5b87772')):
        with zipfile.ZipFile(directory / (name + '.zip')) as z:
            payload = z.read('manifest.json')
            manifest = json.loads(payload)
            assert manifest['performance_acceptance'] is False
            assert set(manifest['roles']) == {'base', 'candidate'}
            assert len(manifest['observations']) == 12
            observed = Counter()
            for role, metadata in manifest['roles'].items():
                expected = candidate if role == 'candidate' else 'a9a2d5915ff45c85d0ca92f42931c62f4ac9430e'
                assert metadata['source_revision'] == expected
                assert digest(z.read(role + '/binary')) == metadata['binary_sha256']
                for snapshot in metadata['snapshots'].values():
                    if snapshot['status'] == 'retained':
                        assert digest(z.read(snapshot['path'])) == snapshot['sha256']
                    else:
                        assert snapshot['status'] == 'unavailable'
            for row in manifest['observations']:
                assert row['checked'] is True and row['name'] in NAMES
                observed[row['name'], row['role']] += 1
                for suffix, field in (('.bc', 'program_sha256'), ('.expected', 'oracle_sha256')):
                    assert digest(z.read(row['name'] + suffix)) == row[field]
                data = z.read(row['report'])
                assert digest(data) == row['report_sha256']
                report = json.loads(data)
                assert report['instrumented'] is True
                assert report['total'] == row['total'] and report['events'] == row['events']
                assert report['allocations']['digits'] == row['digits']
                assert report['total']['live_bytes'] == 0
                assert report['total']['allocation_calls'] == report['total']['deallocation_calls']
                assert all(r['live_bytes'] == 0 for r in report['allocations'].values())
                count = int(re.search(r'_x(\d+)$', row['name'])[1])
                assert report['events']['division_requests'] == count
                # Blocked multiplication is a specialization of a basic call;
                # these inclusive counters must not be added as disjoint work.
                expected = 0 if row['role'] == 'candidate' else count
                for key in ('basic_multiply_calls', 'blocked_multiply_calls'):
                    assert report['events'][key] == expected
                for key in ('karatsuba_multiply_calls', 'square_multiply_calls'):
                    assert report['events'][key] == 0
            assert observed == Counter((name, role) for name in NAMES for role in ('base', 'candidate'))
            if name == 'final-logical':
                logical_bytes = payload
    assert logical_bytes is not None
    with zipfile.ZipFile(directory / 'final-rss.zip') as z, zipfile.ZipFile(
            root / 'native/remainder-final-46a7-417/macos.zip') as native:
        manifest = json.loads(z.read('manifest.json'))
        assert manifest['performance_acceptance'] is False and manifest['trials'] == 3
        assert manifest['logical_manifest_sha256'] == digest(logical_bytes)
        assert set(manifest['roles']) == {'base', 'candidate', 'aa'}
        assert len(manifest['samples']) == 54
        for role, value in manifest['roles'].items():
            metadata = json.loads(native.read(role + '/implementations.json'))['implementations'][0]['metadata']
            assert value['metadata'] == metadata and metadata['instrumented'] is False
        observed = Counter()
        for row in manifest['samples']:
            assert row['checked'] is True and row['resources']['status'] == 'collected'
            payload = z.read(row['report'])
            assert digest(payload) == row['report_sha256']
            maximum = re.search(rb'^\s*(\d+)\s+maximum resident set size$', payload, re.M)
            assert maximum and int(maximum[1]) == row['resources']['peak_rss_native']
            observed[row['name'], row['role'], row['trial']] += 1
        assert observed == Counter((name, role, trial) for name in NAMES
                                   for role in manifest['roles'] for trial in range(3))
    policies = 0
    for host in ('linux', 'macos', 'windows'):
        with zipfile.ZipFile(root / 'native/compiler-policy-d08-418' / (host + '.zip')) as z:
            for role in ('base', 'candidate', 'aa'):
                metadata = json.loads(z.read(role + '/implementations.json'))['implementations'][0]['metadata']
                policy = metadata['configuration']['ipo_policy']
                assert policy['mode'] == 'AUTO'
                if role == 'candidate':
                    assert policy['status'] == 'recorded' and policy['automatic_ipo'] is True
                    data = z.read(role + '/gd-ipo-policy.json')
                    assert digest(data) == policy['report_sha256']
                    assert json.loads(data) == {key: policy[key] for key in
                                               ('schema', 'mode', 'automatic_ipo')}
                else:
                    assert policy['status'] == 'legacy-auto' and policy['automatic_ipo'] is None
                    assert role + '/gd-ipo-policy.json' not in z.namelist()
                policies += 1
    return dict(logical_observations=24, local_rss_observations=54,
                profiling_binaries=4, native_policy_records=policies,
                performance_acceptance=False,
                limitations=['checked output records retained, not re-executed',
                             'local RSS is not hosted-runner RSS',
                             'allocation traffic and peak storage are separate',
                             'no pooled timing or universal memory claim'])


if __name__ == '__main__':
    print(json.dumps(verify(Path(__file__).resolve().parent), indent=2, sort_keys=True))
