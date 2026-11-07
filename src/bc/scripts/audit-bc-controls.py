#!/usr/bin/env python3
"""Replay retained A/A stages and describe control precision without running bc.

No acceptance decision, sample-size recommendation or new timing is produced.
SPDX-License-Identifier: Apache-2.0
"""
import argparse
from collections import Counter, defaultdict
import hashlib
import importlib.util
import json
import math
from pathlib import Path, PurePosixPath
import random
import re
import statistics
import zipfile


def load(name):
    spec = importlib.util.spec_from_file_location(name, Path(__file__).with_name(name + '.py'))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


BENCH = load('bench-bc')
CODEGEN = load('collect-bc-codegen')


def require(condition, message):
    if not condition:
        raise ValueError(message)


def digest(raw):
    return hashlib.sha256(raw).hexdigest()


def member(value):
    path = PurePosixPath(value.replace('\\', '/'))
    require(not path.is_absolute() and '..' not in path.parts and path.parts and
            not any(':' in part for part in path.parts),
            'unsafe archive member')
    return str(path)


def integer(value, minimum):
    require(type(value) is int and value >= minimum, 'invalid declared count')
    return value


def exact(actual, saved, path='summary'):
    """Only the non-decision autocorrelation diagnostic permits tiny rounding."""
    if isinstance(actual, dict):
        require(isinstance(saved, dict) and actual.keys() == saved.keys(), path)
        for key in actual:
            exact(actual[key], saved[key], path + '/' + key)
    elif isinstance(actual, list):
        require(isinstance(saved, list) and len(actual) == len(saved), path)
        for index, (left, right) in enumerate(zip(actual, saved)):
            exact(left, right, path + '/' + str(index))
    elif path.endswith('/autocorrelation_lag1') and actual is not None:
        require(type(saved) in (int, float) and
                math.isclose(actual, saved, rel_tol=0, abs_tol=1e-15), path)
    else:
        require(type(actual) is type(saved) and actual == saved, path)


def binaries(archive, prefix, roles, configuration_paths):
    report = json.loads(archive.read(prefix + 'codegen/manifest.json'))
    require(report['status'] == 'preserved', 'codegen evidence is incomplete')
    bindings, seen = {}, set()
    for row in report['observations']:
        name = row['name']
        require(name not in seen, 'duplicate binary role')
        seen.add(name)
        if name not in roles:
            continue
        require(row['status'] == 'preserved', 'incomplete selected binary')
        recipe, recipe_file = None, None
        if configuration_paths.get(name) is not None:
            recipe_file = prefix + member(configuration_paths[name])
            recipe_bytes = archive.read(recipe_file)
            entries = json.loads(recipe_bytes)['implementations']
            matches = [entry for entry in entries if entry['name'] == name]
            require(len(matches) == 1, 'archived configuration needs exactly one matching role')
            recipe = matches[0]
        if 'implementation' in row:
            implementation = row['implementation']
            kind = 'inline codegen recipe'
            if recipe is not None:
                for key in ('metadata', 'argv', 'input_artifacts'):
                    require(implementation.get(key, []) == recipe.get(key, []),
                            'inline and archived role recipes disagree')
        else:
            require(recipe is not None, 'legacy codegen needs explicit archived role configuration')
            implementation = recipe
            kind = 'archived role configuration'
            require(row['source_revision'] == implementation['metadata']['source_revision'],
                    'legacy source identity mismatch')
        require(implementation['name'] == name, 'codegen role and recipe disagree')
        raw = archive.read(prefix + 'codegen/' + member(row['binary']))
        require(digest(raw) == row['binary_sha256'] == implementation['metadata']['binary_sha256'],
                'binary identity mismatch')
        require(implementation['metadata']['instrumented'] is False, 'instrumented binary')
        require(len(raw) == row['file_bytes'] and CODEGEN.pe_code(raw) == row['pe_code'],
                'binary size/code identity mismatch')
        declared = implementation.get('input_artifacts', [])
        copied_inputs = row.get('input_artifacts', [])
        require(len(declared) == len(copied_inputs), 'missing preserved external input copies')
        for original, copied in zip(declared, copied_inputs):
            require(all(copied.get(k) == v for k, v in original.items()), 'input recipe mismatch')
            require(digest(archive.read(prefix + 'codegen/' + member(copied['archive_path']))) ==
                    original['sha256'], 'external input identity mismatch')
        bindings[name] = dict(implementation=implementation, sha256=digest(raw),
                              file_bytes=len(raw), pe_code=row['pe_code'], recipe_kind=kind,
                              recipe_file=recipe_file,
                              recipe_file_sha256=digest(recipe_bytes) if recipe_file else None)
    require(bindings.keys() == roles, 'missing selected binary role')
    return bindings


def relation(interval):
    low, high = interval
    if .95 <= low <= high <= 1.05:
        return 'inside band'
    if low > 1.05:
        return 'above band'
    if high < .95:
        return 'below band'
    return 'overlaps band boundary'


def duration_band(seconds):
    return ('<10ms' if seconds < .01 else '10-50ms' if seconds < .05 else
            '50-250ms' if seconds < .25 else '>=250ms')


def stage(archive, prefix, name, control, source, bindings):
    require(re.fullmatch(r'[A-Za-z0-9_-]+', name) is not None, 'invalid stage name')
    root = prefix + name + '/'
    manifest_bytes = archive.read(root + 'manifest.json')
    manifest = json.loads(manifest_bytes)
    harness = Path(__file__).with_name('bench-bc.py').read_bytes().replace(b'\r\n', b'\n')
    expected = (harness.replace(b'\n', b'\r\n') if manifest['platform']['system'] == 'Windows'
                else harness)
    require(digest(expected) == manifest['harness_sha256'], 'use the matching benchmark harness')
    cases, tools = manifest['corpus'], manifest['implementations']
    reference = manifest['reference']
    require(reference != control and {t['name'] for t in tools} == {reference, control} and
            len(tools) == 2, 'an A/A stage must contain exactly the two requested roles')
    require(cases and len({c['name'] for c in cases}) == len(cases), 'empty/duplicate corpus')
    for tool in tools:
        require(tool['complete_provenance'] is True, 'incomplete role provenance')
        require(tool['metadata']['source_revision'] == source, 'different declared source')
        require(tool['metadata']['instrumented'] is False and
                tool['binary_sha256'] == tool['metadata']['binary_sha256'], 'invalid role identity')
        binding = bindings[tool['name']]['implementation']
        require(binding['metadata'] == tool['metadata'] and binding['argv'] == tool['argv'],
                'stage and retained binary disagree')
    for case in cases:
        require(re.fullmatch(r'[A-Za-z0-9_.-]+', case['name']) is not None and
                case['name'] not in ('.', '..'), 'invalid workload name')
        for suffix, key in (('.bc', 'program_sha256'), ('.expected', 'oracle_sha256')):
            require(digest(archive.read(root + 'workloads/' + case['name'] + suffix)) == case[key],
                    'workload/oracle identity mismatch')
    samples = integer(manifest['samples'], 3)
    warmups = integer(manifest['warmups'], 0)
    integer(manifest['seed'], 0)
    integer(manifest['bootstrap_resamples'], 200)
    raw = archive.read(root + 'samples.jsonl')
    rows = [json.loads(line) for line in raw.splitlines()]
    require(len(rows) == (samples + warmups) * len(cases) * 2, 'missing/extra samples')
    outputs = {}
    for row in rows:
        require(row['checked'] is True, 'unchecked sample')
        require(type(row['seconds']) in (int, float) and math.isfinite(row['seconds']) and
                row['seconds'] > 0, 'invalid elapsed value')
        integer(row['round'], 0)
        integer(row['order'], 0)
        integer(row['output_bytes'], 0)
        require(re.fullmatch('[0-9a-f]{64}', row['output_sha256']) is not None,
                'invalid checked-output identity')
        key = row['workload'], row['implementation']
        identity = row['output_sha256'], row['output_bytes']
        require(outputs.setdefault(key, identity) == identity, 'inconsistent checked output')
    rng, index = random.Random(manifest['seed']), 0
    for phase, rounds in (('warmup', warmups), ('measurement', samples)):
        for round_number in range(rounds):
            shuffled = list(cases)
            rng.shuffle(shuffled)
            for case in shuffled:
                block = list(tools)
                rng.shuffle(block)
                for order, tool in enumerate(block):
                    row = rows[index]
                    require((row['phase'], row['round'], row['workload'], row['implementation'],
                             row['order']) == (phase, round_number, case['name'], tool['name'], order),
                            'sample order/role differs from fixed seed')
                    index += 1
    replayed = BENCH.summarize(cases, rows, reference, manifest['seed'],
                             manifest['bootstrap_resamples'])
    exact(replayed, json.loads(archive.read(root + 'summary.json')))
    controls = []
    for case in replayed['workloads']:
        times = {role: [r['seconds'] for r in rows if r['phase'] == 'measurement' and
                        r['workload'] == case['name'] and r['implementation'] == role]
                 for role in (reference, control)}
        median = statistics.median(times[reference] + times[control])
        ratio = case['candidate_over_reference'][control]
        controls.append(dict(name=case['name'], group=case['group'],
                             parameters=case['parameters'], ratio=ratio,
                             ci_width=ratio['ci95'][1] - ratio['ci95'][0],
                             interval_relation=relation(ratio['ci95']),
                             median_child_seconds=median, duration_band=duration_band(median),
                             role_median_seconds={r: statistics.median(t) for r, t in times.items()}))
    bands = defaultdict(Counter)
    for row in controls:
        bands[row['duration_band']][row['interval_relation']] += 1
    identities = {role: bindings[role] for role in (reference, control)}
    configurations = [bindings[role]['implementation']['metadata'].get('configuration')
                      for role in (reference, control)]
    return dict(stage=name, manifest_sha256=digest(manifest_bytes), raw_sha256=digest(raw),
                platform=manifest['platform'], harness_sha256=manifest['harness_sha256'],
                reference=reference, control=control, samples=samples, warmups=warmups,
                checked_rows=len(rows), identities=identities,
                binary_bytes_identical=bindings[reference]['sha256'] == bindings[control]['sha256'],
                declared_configurations_equal=(None if None in configurations else
                                               configurations[0] == configurations[1]),
                duration_bands={k: dict(v) for k, v in bands.items()}, controls=controls)


def audit(path, archive_sha256, prefix, stages, control, source,
          reference_config=None, control_config=None):
    require(re.fullmatch('[0-9a-f]{64}', archive_sha256) is not None, 'invalid archive digest')
    require(re.fullmatch('[0-9a-f]{40}', source) is not None, 'invalid source revision')
    raw = Path(path).read_bytes()
    require(digest(raw) == archive_sha256, 'archive digest mismatch')
    # upload-artifact retains the evidence directory's contents at the ZIP
    # root for normal trend jobs. Root is a prefix, never a file member.
    prefix = '' if prefix in ('', '.') else member(prefix).rstrip('/') + '/'
    require(len(stages) == len(set(stages)) and stages, 'empty/duplicate stage selection')
    with zipfile.ZipFile(path) as archive:
        require(len(archive.namelist()) == len(set(archive.namelist())), 'duplicate archive members')
        require(archive.testzip() is None, 'archive CRC mismatch')
        references = set()
        for name in stages:
            require(re.fullmatch(r'[A-Za-z0-9_-]+', name) is not None, 'invalid stage name')
            references.add(json.loads(archive.read(prefix + name + '/manifest.json'))['reference'])
        configurations = {role: reference_config for role in references}
        configurations[control] = control_config
        bindings = binaries(archive, prefix, references | {control}, configurations)
        results = [stage(archive, prefix, name, control, source, bindings) for name in stages]
    return dict(schema=1, source_revision=source, archive_sha256=archive_sha256,
                auditor_sha256=digest(Path(__file__).read_bytes()), stages=results,
                merge_eligible=False, performance_acceptance=False,
                limits=['Descriptive intervals and duration bands; no power or causality claim',
                        'Only selected A/A executable roles are audited; other codegen roles are not',
                        'Only declared external inputs are checked; undeclared inputs cannot be ruled out',
                        'Stages and hosts remain separate; counts are not pooled effects',
                        'Same source or matching file length does not prove identical instructions',
                        'Byte identity does not establish equal latency or equivalence',
                        'PE payload hashes include addresses/padding without relocation normalization',
                        'Declared configuration equality is not independent build-cache verification',
                        'Outputs were checked by the collector; numeric stdout is not replayed here',
                        'Confirmation stages remain separate; no historical label or acceptance is changed'])


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--archive', type=Path, required=True)
    parser.add_argument('--sha256', required=True)
    parser.add_argument('--prefix', default='evidence/')
    parser.add_argument('--stage', action='append', required=True)
    parser.add_argument('--control-role', required=True)
    parser.add_argument('--source', required=True)
    parser.add_argument('--reference-config', help='explicit archived reference recipe for legacy codegen')
    parser.add_argument('--control-config', help='explicit archived control recipe for legacy codegen')
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args(argv)
    try:
        report = audit(args.archive, args.sha256, args.prefix, args.stage, args.control_role, args.source,
                       args.reference_config, args.control_config)
        payload = json.dumps(report, indent=2, allow_nan=False) + '\n'
        # A previous observation is evidence; require a new path instead of replacing it.
        with args.output.open('x', encoding='utf-8') as output:
            output.write(payload)
    except (ValueError, KeyError, TypeError, IndexError, OSError, zipfile.BadZipFile) as error:
        parser.error(str(error))
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
