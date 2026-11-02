#!/usr/bin/env python3
"""Verify retained root and numeric-storage diagnostics without rerunning them.

Logical allocation counters and local RSS observations are separate evidence.
The original failed RSS attempt remains failed; the retry fills only that gap.
SPDX-License-Identifier: Apache-2.0
"""
from collections import Counter
import hashlib
import json
from pathlib import Path
import re
import zipfile


def digest(data):
    return hashlib.sha256(data).hexdigest()


def report(archive, row):
    payload = archive.read(row['report'])
    assert digest(payload) == row['report_sha256']
    return json.loads(payload)


def verify(root):
    directory = root / 'root-storage'
    with zipfile.ZipFile(directory / 'root-logical.zip') as z:
        manifest_bytes = z.read('manifest.json')
        logical = json.loads(manifest_bytes)
        assert logical['status'] == 'failed' and not logical['resources']
        assert 'sysctl kern.clockrate: Operation not permitted' in logical['failure']
        assert logical['performance_acceptance'] is False
        assert len(logical['cases']) == 28 and len(logical['logical']) == 56
        cases = {}
        for case in logical['cases']:
            key = case['name'], case['mode']
            assert key not in cases
            cases[key] = case
            for suffix, field in (('.bc', 'program_sha256'), ('.expected', 'oracle_sha256')):
                assert digest(z.read('-'.join(key) + suffix)) == case[field]
        assert Counter(mode for _, mode in cases) == Counter(single=14, batch=14)
        observed = Counter()
        for row in logical['logical']:
            assert row['checked'] is True
            assert (row['name'], row['mode']) in cases
            observed[row['name'], row['mode'], row['role']] += 1
            data = report(z, row)
            assert data['instrumented'] is True and data['total']['live_bytes'] == 0
            for key in ('total', 'events'):
                assert data[key] == row[key]
            assert data['allocations']['digits'] == row['digits']
        assert observed == Counter((name, mode, role) for name, mode in cases
                                   for role in ('base', 'candidate'))
        for role, metadata in logical['profile_roles'].items():
            assert metadata['instrumented'] is True
            assert digest(z.read(role + '-profile-binary')) == metadata['binary_sha256']
            for snapshot in metadata['snapshots'].values():
                assert digest(z.read(snapshot['path'])) == snapshot['sha256']
    with zipfile.ZipFile(directory / 'root-rss.zip') as z, zipfile.ZipFile(
            root / 'native/root-535-405/macos.zip') as native:
        rss = json.loads(z.read('manifest.json'))
        assert rss['logical_manifest_sha256'] == digest(manifest_bytes)
        assert rss['original_failure_retained'] is True
        assert rss['performance_acceptance'] is False and rss['trials'] == 3
        assert len(rss['samples']) == 126
        observed = Counter()
        names = {name for name, mode in cases if mode == 'batch'}
        for role, tool in rss['roles'].items():
            metadata = json.loads(native.read(role + '/implementations.json'))['implementations'][0]['metadata']
            assert tool['metadata'] == metadata and metadata['instrumented'] is False
        assert set(rss['roles']) == {'base', 'candidate', 'aa'}
        for row in rss['samples']:
            assert row['checked'] is True and row['resources']['status'] == 'collected'
            payload = z.read(row['report'])
            assert digest(payload) == row['report_sha256']
            maximum = re.search(rb'^\s*(\d+)\s+maximum resident set size$', payload, re.M)
            assert maximum and int(maximum[1]) == row['resources']['peak_rss_native']
            observed[row['name'], row['role'], row['trial']] += 1
        assert observed == Counter((name, role, trial) for name in names
                                   for role in rss['roles'] for trial in range(3))
    for name in ('numeric-optional', 'numeric-sparse'):
        with zipfile.ZipFile(directory / (name + '.zip')) as z:
            value = json.loads(z.read('manifest.json'))
            assert len(value['cases']) == 6
            observed = Counter()
            for row in value['cases']:
                assert row['checked'] is True
                observed[row['name'], row['role']] += 1
                for suffix, field in (('.bc', 'program_sha256'), ('.expected', 'oracle_sha256')):
                    assert digest(z.read(row['name'] + suffix)) == row[field]
                data = report(z, row)
                assert data['instrumented'] is True and data['total']['live_bytes'] == 0
                for field in ('frames', 'results'):
                    assert data['allocations'][field] == row[field]
            assert observed == Counter((case, role) for case in
                                       ('generic-calls', 'generic-recursion', 'mixed-recursion')
                                       for role in ('parent', 'candidate'))
            if name == 'numeric-sparse':
                with zipfile.ZipFile(directory / 'numeric-sparse-provenance.zip') as provenance:
                    for role, metadata in value['roles'].items():
                        for entry, field in (('binary', 'binary_sha256'),
                                             ('vm.cc', 'vm_source_sha256'),
                                             ('CMakeCache.txt', 'compiler_cache_sha256')):
                            assert digest(provenance.read(role + '/' + entry)) == metadata[field]
    return dict(root_logical=56, root_rss=126, numeric_logical=12,
                retained_profile_binaries=4, performance_acceptance=False,
                limitations=['RSS comes from local execution of retained CI binaries',
                             'optional prototype executable is unavailable',
                             'checked output records are retained, not re-executed',
                             'no elapsed-time or aggregate acceptance claim'])


if __name__ == '__main__':
    print(json.dumps(verify(Path(__file__).resolve().parent), indent=2, sort_keys=True))
