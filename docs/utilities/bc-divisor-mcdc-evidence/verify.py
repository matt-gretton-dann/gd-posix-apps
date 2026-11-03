#!/usr/bin/env python3
"""Authenticate separate LLVM collections; never pool coverage denominators.

No archived program, source or compiler is executed. LLVM exports and merge
results are retained observations, not independently regenerated here.
"""
import base64
import hashlib
import io
import json
from pathlib import Path
import re
import tarfile

ROOT = Path(__file__).resolve().parent
SCOPES = ('src', 'libgdsup', 'cmake', 'python-lib', 'CMakeLists.txt')
BASE = 'f2bfdff902c49ed35ebb8f84a81f6b657499af0f'
FOCUSED = {'focused-v1': ('19e22f708b0a834298671074c37873aa08a3cbdd', 843, 21),
           'focused-v2': ('c7b8f6ac017e1ba49b8ffb338ad3e2fc7f10e985', 900, 24),
           'focused-v3': ('fa2420cc57d51b6e578fd7379282b02cff3ae266', 916, 26)}


def require(condition, message):
    if not condition:
        raise ValueError(message)


def sha(data):
    return hashlib.sha256(data).hexdigest()


def git_hash(kind, data):
    # Git object names use SHA-1; archive integrity uses SHA-256.
    return hashlib.sha1(kind.encode() + b' ' + str(len(data)).encode() + b'\0' + data).hexdigest()


def tree_entries(raw):
    at = 0
    while at < len(raw):
        space = raw.index(b' ', at)
        end = raw.index(b'\0', space)
        require(end + 21 <= len(raw), 'truncated Git tree')
        name = raw[space + 1:end].decode()
        require(name not in ('', '.', '..') and '/' not in name, 'unsafe Git name')
        yield raw[at:space].decode(), name, raw[end + 1:end + 21].hex()
        at = end + 21
    require(at == len(raw), 'invalid Git tree extent')


def verify_source(snapshot, proof):
    commit = base64.b64decode(proof['commit'])
    require(git_hash('commit', commit) == proof['revision'] and tuple(proof['scopes']) == SCOPES,
            'source commit identity differs')
    first = commit.splitlines()[0].split()
    require(first[0] == b'tree', 'commit lacks root tree')
    root = first[1].decode()
    trees = {oid: base64.b64decode(data) for oid, data in proof['trees'].items()}
    require(all(git_hash('tree', raw) == oid for oid, raw in trees.items()), 'Git tree identity differs')
    expected, used = {}, {root}

    def walk(oid, prefix, depth=0):
        require(depth < 64, 'source tree nesting exceeds scope')
        used.add(oid)
        for mode, name, child in tree_entries(trees[oid]):
            path = prefix + '/' + name
            if mode == '40000':
                walk(child, path, depth + 1)
            else:
                require(mode in ('100644', '100755', '120000'), 'unsupported source entry')
                expected[path] = child

    for mode, name, oid in tree_entries(trees[root]):
        if name in SCOPES:
            if mode == '40000':
                walk(oid, name)
            else:
                expected[name] = oid
    require(used == set(trees), 'source proof tree inventory differs')
    files = {}
    with tarfile.open(fileobj=io.BytesIO(snapshot), mode='r:') as source:
        require(source.pax_headers.get('comment') == proof['revision'], 'source tar revision differs')
        for member in source:
            if member.isdir():
                continue
            require(member.name not in files and member.name in expected, 'unexpected source snapshot member')
            if member.isfile():
                data = source.extractfile(member).read()
            else:
                require(member.issym(), 'unsupported source tar member')
                data = member.linkname.encode()
            require(git_hash('blob', data) == expected[member.name], 'source blob identity differs')
            files[member.name] = data
    require(set(files) == set(expected), 'source snapshot file inventory differs')
    return files['src/bc/number.hh'].decode()


def load(entry):
    path = ROOT / entry['path']
    data = path.read_bytes()
    require(entry['container'] == 'tar.xz' and len(data) == entry['bytes'] and sha(data) == entry['sha256'],
            'collection identity differs')
    payloads = {}
    with tarfile.open(fileobj=io.BytesIO(data), mode='r:xz') as source:
        for member in source:
            require(member.isfile() and not member.name.startswith('/') and '..' not in member.name.split('/') and
                    member.name not in payloads, 'unsafe or duplicate collection member')
            payloads[member.name] = source.extractfile(member).read()
    return payloads


def cache_values(data):
    result = {}
    for line in data.decode().splitlines():
        if '=' in line and not line.startswith(('#', '//')):
            key, value = line.split('=', 1)
            result[key.split(':', 1)[0]] = value
    return result


def verify_baseline(payloads, entry):
    expected_id = ('baseline' if entry['eligible'] else 'failed') + '-ext-' + entry['extensions'].lower() + '-reuse-' + entry['prepared_divisors'].lower()
    require(entry['id'] == expected_id, 'baseline eligibility differs')
    report = json.loads(payloads['summary.json'])
    require(report['source_revision'] == BASE and report['source_status'] == '', 'baseline source differs')
    require(report['extensions'] == entry['extensions'] and report['prepared_divisors'] == entry['prepared_divisors'],
            'baseline modes differ')
    require(report['test_exit_code'] == (0 if entry['eligible'] else 8), 'test eligibility differs')
    require(report['adjusted_coverage'] is None and report['coverage_option'] == 'ON', 'unsupported adjusted coverage')
    cache = cache_values(payloads['build/CMakeCache.txt'])
    for key, value in (('CMAKE_BUILD_TYPE', 'Debug'), ('ENABLE_BC_COVERAGE', 'ON'), ('ENABLE_BC_PROFILE', 'OFF'),
                       ('ENABLE_NON_POSIX_EXTENSIONS', entry['extensions']),
                       ('ENABLE_BC_PREPARED_DIVISORS', entry['prepared_divisors']), ('BC_MULTIPLY_SPLIT_POINT', '126')):
        require(cache.get(key) == value, 'configured mode differs: ' + key)
    require(b'#define BC_MULTIPLY_SPLIT_POINT 126' in payloads['build/multiply-threshold.hh'], 'threshold differs')
    anchors = set()
    for command in json.loads(payloads['build/compile_commands.json']):
        filename = command['file'].replace('\\', '/')
        if filename.endswith(('/src/bc/bc.cc', '/src/bc/vm.cc')):
            flags = command['command']
            require(all(flag in flags for flag in ('-fprofile-instr-generate', '-fcoverage-mapping', '-fcoverage-mcdc')),
                    'compiled coverage instrumentation missing')
            values = re.findall(r'-DGD_BC_PREPARED_DIVISORS=([^\s]+)', flags)
            require(values == ['1' if entry['prepared_divisors'] == 'ON' else '0'] and
                    not re.search(r'(?:^|\s)-U\s*GD_BC_PREPARED_DIVISORS', flags), 'compiled activation differs')
            anchors.add(filename.rsplit('/', 1)[-1])
    require(anchors == {'bc.cc', 'vm.cc'}, 'compiled anchors missing')
    for path, totals, files in (('coverage.json', 'raw_totals', 'files'),
                                ('native-coverage.json', 'application_mappings_totals', 'application_mappings_files')):
        exported = json.loads(payloads[path])['data'][0]
        require(exported['totals'] == report[totals] and
                {row['filename']: row['summary'] for row in exported['files']} == report[files],
                'LLVM export/summary differs')
        require(exported['totals']['mcdc']['count'] > 0, 'MC/DC obligations absent')
    profiles = [name for name in payloads if name.startswith('test-profiles/') and name.endswith('.profraw')]
    manifest = payloads['profiles.txt'].decode().splitlines()
    require(len(profiles) == len(manifest) == report['profile_count'] and
            len(set(manifest)) == len(manifest) and
            {'test-profiles/' + Path(name).name for name in manifest} == set(profiles), 'profile inventory differs')
    require(payloads['tests.profdata'] and payloads['tests.log'], 'merge or test evidence missing')
    return dict(id=entry['id'], source_revision=BASE, eligible=entry['eligible'],
                extensions=entry['extensions'], prepared_divisors=entry['prepared_divisors'],
                profiles=len(profiles), test_exit_code=report['test_exit_code'],
                broad=report['raw_totals'], application_only=report['application_mappings_totals'])


def width(name):
    for tag, bits in (('BaseIhtL', '8'), ('BaseItjL', '16'), ('BaseIjyL', '32')):
        if tag in name:
            return bits
    raise ValueError('unknown captured LLVM trait identity')


def verify_focused(payloads, entry, header):
    revision, assertions, cases = FOCUSED[entry['id']]
    require(entry['eligible'] is (entry['id'] == 'focused-v3'), 'focused eligibility differs')
    require(entry['source_revision'] == revision, 'focused source differs')
    log = payloads['tests.log'].decode()
    require('All tests passed ({} assertions in {} test cases)'.format(assertions, cases) in log,
            'focused test inventory differs')
    proof = json.loads(payloads['branch-proof.json'])
    require(proof['source_revision'] == revision, 'branch proof source differs')
    source = header.splitlines()

    def location(text):
        matches = [(i, line.index(text) + 1) for i, line in enumerate(source, 1) if text in line]
        require(len(matches) == 1, 'source predicate identity differs')
        return matches[0]

    capacity = location('if (capacity > remaining)')[0]
    retry = location('if (rejected_)')[0]
    zero_line, zero_column = location('source.is_zero())')
    padding_line, padding_column = location('whole > limit / 3 - capacity')
    records = {bits: {} for bits in ('8', '16', '32')}
    seen = set()
    for function in json.loads(payloads['coverage.json'])['data'][0]['functions']:
        if 'within_budget' not in function['name'] and not ('DivisorCache' in function['name'] and '6lookup' in function['name']):
            continue
        require(function['name'] not in seen, 'duplicate focused function')
        seen.add(function['name'])
        bits = width(function['name'])
        branches = function['branches']
        require(all(type(value) is int and value >= 0 for b in branches for value in b[4:6]), 'invalid branch count')
        if 'within_budget' in function['name']:
            b = next(b for b in branches if b[0] == capacity)
            records[bits]['capacity_rejections'] = b[4]
            require(b[5] > 0, 'capacity acceptance outcomes absent')
        else:
            records[bits]['rejected_retries'] = next(b[4] for b in branches if b[0] == retry)
            records[bits]['heap_zero_rejections'] = next(b[4] for b in branches if b[0] == zero_line and b[1] == zero_column)
            records[bits]['wide_padding_rejections'] = next(b[4] for b in branches if b[0] == padding_line and b[1] == padding_column)
    require(len(seen) == 6 and all(len(row) == 4 for row in records.values()), 'focused function inventory differs')
    if entry['id'] == 'focused-v1':
        require(records['8']['capacity_rejections'] == records['8']['rejected_retries'] == 0,
                'initial admitted 8-bit fixture was misclassified')
    else:
        require(all(row['capacity_rejections'] > 0 and row['rejected_retries'] > 0 and
                    row['heap_zero_rejections'] > 0 for row in records.values()), 'focused branch outcome absent')
    if entry['id'] == 'focused-v3':
        require(all(records[bits]['wide_padding_rejections'] > 0 for bits in ('16', '32')), 'wide padding outcome absent')
    return dict(id=entry['id'], source_revision=revision, assertions=assertions, cases=cases,
                application_coverage=False, eligible=entry['eligible'], widths=records)


def verify(payloads, entry, source_proof):
    manifest = json.loads(payloads['_archive_manifest.json'])
    require(set(payloads) == set(manifest['files']) | {'_archive_manifest.json'} and
            len(manifest['files']) == entry['files'], 'payload inventory differs')
    require(manifest['source_revision'] == entry['source_revision'] and manifest['kind'] == entry['kind'], 'collection scope differs')
    for name, record in manifest['files'].items():
        require(len(payloads[name]) == record['bytes'] and sha(payloads[name]) == record['sha256'], 'payload hash differs: ' + name)
    for name, expected in entry['binary_identities'].items():
        require(sha(payloads[name]) == expected, 'binary identity differs')
    header = verify_source(payloads['source.tar'], source_proof)
    return verify_baseline(payloads, entry) if entry['kind'] == 'application-baseline' else verify_focused(payloads, entry, header)


def inventory(configuration=None):
    if configuration is None:
        configuration = json.loads((ROOT / 'inventory.json').read_text())
    require(configuration['coverage_goal_achieved'] is False and configuration['performance_acceptance'] is False,
            'collections do not establish full coverage or adoption')
    entries = configuration['collections']
    expected = {'baseline-ext-' + ext + '-reuse-' + reuse for ext in ('on', 'off') for reuse in ('on', 'off')}
    expected |= {'failed-ext-on-reuse-on', *FOCUSED}
    require(len(entries) == 8 and {e['id'] for e in entries} == expected, 'collection inventory differs')
    require({(e['extensions'], e['prepared_divisors']) for e in entries if e['kind'] == 'application-baseline' and e['eligible']} ==
            {('ON', 'ON'), ('ON', 'OFF'), ('OFF', 'ON'), ('OFF', 'OFF')}, 'passing mode matrix differs')
    return configuration


def source_proof(configuration, revision):
    entry = configuration['source_proofs'][revision]
    data = (ROOT / entry['path']).read_bytes()
    require(sha(data) == entry['sha256'], 'source proof identity differs')
    return json.loads(data)


def main():
    configuration = inventory()
    results = [verify(load(entry), entry, source_proof(configuration, entry['source_revision']))
               for entry in configuration['collections']]
    require(results == json.loads((ROOT / 'summary.json').read_text()), 'published coverage summary differs')
    print('Four separate passing modes; failed and focused attempts retained; no pooled coverage or adoption')
    return results


if __name__ == '__main__':
    main()
