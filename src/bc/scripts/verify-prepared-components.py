#!/usr/bin/env python3
"""Replay retained coefficient diagnostics without rerunning or accepting timings."""
import hashlib
import json
import re
from pathlib import Path
import statistics
import sys
import zipfile

MODES = {'coefficient-control', 'prepare-divisor', 'coefficient-divide',
         'prepared-divide', 'coefficient-modulo', 'prepared-modulo'}


def require(condition, message):
    if not condition:
        raise ValueError(message)


def digest(data):
    return hashlib.sha256(data).hexdigest()


def verify(payloads, summary):
    data = json.loads(payloads['manifest.json'])
    require(data['source_revision'] == summary['source_revision'], 'source revision')
    require(data['adoption_accepted'] is False and summary['adoption_accepted'] is False,
            'component results do not establish adoption')
    for name, record in data['binaries'].items():
        require(digest(payloads[name]) == record['sha256'], 'binary/recipe hash: ' + name)
        require(len(payloads[name]) == record['bytes'], 'binary/recipe size: ' + name)
    require(set(data['binaries']) == {role + '/' + name
            for role in ('profile', 'timing')
            for name in ('bc-components', 'CMakeCache.txt', 'build.ninja')}, 'recipe inventory')
    for name, expected in data['sources'].items():
        require(digest(payloads['source/' + name]) == expected, 'source hash: ' + name)
    cases = {case['name']: case for case in data['cases']}
    require(len(cases) == len(data['cases']) == 12, 'case inventory')
    require(set(cases) == {f'{preset}-{size}-leading-{lead}'
                          for preset in ('full', 'held-out')
                          for size in range(3) for lead in ('1', '9')}, 'case names')
    for case in cases.values():
        a, b = map(int, case['source'].split())
        require(b > 0 and str(a) == case['dividend'] and str(b) == case['divisor'],
                'coefficient fixture')
        require(str(a // b) == case['quotient'] and str(a % b) == case['remainder'],
                'independent integer oracle')
        require(case['iterations'] == 2000, 'iteration plan')
    seen = set()
    positions = set()
    timings = {}
    for row in data['observations']:
        result = row['result']
        phase, name, trial = row['phase'], row['case'], row['trial']
        mode = result['component']
        key = (phase, name, trial, mode)
        require(key not in seen, 'duplicate observation')
        seen.add(key)
        position = (phase, name, trial, row['position'])
        require(position not in positions and 0 <= row['position'] < 6, 'duplicate position')
        positions.add(position)
        require(row['returncode'] == 0 and phase in ('profile', 'timing'), 'successful phase')
        require(mode in MODES and name in cases, 'observation dimensions')
        case = cases[name]
        require(result['iterations'] == case['iterations'] and result['split_point'] == 126,
                'operation recipe')
        require(result['instrumented'] == (phase == 'profile'), 'instrumentation separation')
        for stream in ('stdout', 'stderr'):
            require(digest(payloads[row[stream]]) == row[stream + '_sha256'], 'stream hash')
        require(payloads[row['stderr']] == b'', 'unexpected stderr')
        require(json.loads(payloads[row['stdout']]) == result, 'recorded result')
        oracle = ('divisor' if mode == 'prepare-divisor' else
                  'dividend' if mode == 'coefficient-control' else
                  'remainder' if mode.endswith('modulo') else 'quotient')
        require(result['result'] == case[oracle], 'checked output')
        if phase == 'profile':
            require(result['seconds'] is None and result['setup_seconds'] is None,
                    'profile timing must be absent')
            for field in ('operation_allocation_calls', 'operation_allocated_bytes',
                          'setup_allocation_calls', 'setup_allocated_bytes',
                          'operation_cow_clones'):
                require(type(result[field]) is int and result[field] >= 0, 'allocation counter')
        else:
            require(result['seconds'] >= 0 and result['setup_seconds'] >= 0, 'timing value')
            timings.setdefault((name, mode), []).append(result['seconds'])
    expected = {(phase, name, trial, mode)
                for phase, trials in [('profile', range(1)), ('timing', range(8))]
                for name in cases for trial in trials for mode in MODES}
    require(seen == expected, 'complete observation grid')
    require(summary['profile_checked'] == 72 and summary['timing_checked'] == 576,
            'summary counts')
    ratios = {row['case']: row for row in summary['median_ratios']}
    require(len(ratios) == len(summary['median_ratios']) == 12 and set(ratios) == set(cases),
            'ratio inventory')
    for name in cases:
        for operation, label in [('divide', 'quotient'), ('modulo', 'remainder')]:
            ratio = (statistics.median(timings[name, 'prepared-' + operation]) /
                     statistics.median(timings[name, 'coefficient-' + operation]))
            require(ratio == ratios[name][label + '_median_ratio'], 'reported median ratio')
    return {'profile_checked': 72, 'timing_checked': 576, 'adoption_accepted': False}


def verify_rss(payloads, summary, components):
    data = json.loads(payloads['manifest.json'])
    source = json.loads(components['manifest.json'])
    require(data['component_manifest_sha256'] == digest(components['manifest.json']),
            'RSS component source binding')
    require(data['source_revision'] == summary['source_revision'] == source['source_revision'],
            'RSS source revision')
    require(data['binary_sha256'] == source['binaries']['timing/bc-components']['sha256'],
            'RSS executable binding')
    require(data['adoption_accepted'] is False and summary['adoption_accepted'] is False,
            'RSS does not establish adoption')
    cases = {case['name']: case for case in source['cases']}
    seen, samples = set(), {}
    for row in data['observations']:
        name, mode, trial = row['case'], row['mode'], row['trial']
        key = (name, mode, trial)
        require(key not in seen, 'duplicate RSS observation')
        seen.add(key)
        require(row['returncode'] == 0 and name in cases and mode in MODES, 'RSS dimensions')
        for stream in ('stdout', 'stderr', 'resource'):
            require(digest(payloads[row[stream]]) == row[stream + '_sha256'], 'RSS stream hash')
        require(payloads[row['stderr']] == b'', 'RSS stderr')
        case = cases[name]
        oracle = ('divisor' if mode == 'prepare-divisor' else
                  'dividend' if mode == 'coefficient-control' else
                  'remainder' if mode.endswith('modulo') else 'quotient')
        output = json.loads(payloads[row['stdout']])
        require(output['result'] == case[oracle] and output['instrumented'] is False,
                'RSS checked output')
        require(output['component'] == mode and output['iterations'] == case['iterations'],
                'RSS operation recipe')
        matches = re.findall(rb'^\s*(\d+)\s+maximum resident set size\s*$',
                             payloads[row['resource']], re.M)
        require(len(matches) == 1 and int(matches[0]) == row['maximum_resident_set_size_bytes'],
                'RSS recorded peak')
        samples.setdefault((name, mode), []).append(row['maximum_resident_set_size_bytes'])
    require(seen == {(name, mode, trial) for name in cases for mode in MODES
                     for trial in range(3)}, 'complete RSS grid')
    medians = {row['case']: row['median_peak_rss_bytes'] for row in summary['medians']}
    require(len(medians) == len(summary['medians']) == 12 and set(medians) == set(cases),
            'RSS median inventory')
    for name in cases:
        require(set(medians[name]) == MODES, 'RSS mode inventory')
        for mode in MODES:
            require(medians[name][mode] == statistics.median(samples[name, mode]),
                    'RSS reported median')
    require(summary['rss_checked'] == 216, 'RSS summary count')
    return {'rss_checked': 216, 'adoption_accepted': False}


def load(folder, stem="components-v1"):
    summary = json.loads((folder / (stem + ".json")).read_text())
    archive = folder / (stem + ".zip")
    require(digest(archive.read_bytes()) == summary['archive_sha256'], 'published archive hash')
    require(archive.stat().st_size == summary['archive_bytes'], 'published archive size')
    with zipfile.ZipFile(archive) as bundle:
        require(len(bundle.namelist()) == len(set(bundle.namelist())), 'duplicate archive entry')
        payloads = {name: bundle.read(name) for name in bundle.namelist()}
    return payloads, summary


def main():
    folder = (Path(sys.argv[1]) if len(sys.argv) > 1 else
              Path(__file__).resolve().parents[3] / 'docs/utilities/bc-prepared-divisors')
    components, summary = load(folder)
    result = verify(components, summary)
    result.update(verify_rss(*load(folder, "rss-v1"), components))
    print(json.dumps(result, sort_keys=True))


if __name__ == '__main__':
    main()
