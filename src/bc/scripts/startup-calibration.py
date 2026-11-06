#!/usr/bin/env python3
"""Fixed, advisory startup null study; never relabel historical trend samples.

SPDX-License-Identifier: Apache-2.0
"""
import argparse
import copy
import hashlib
import importlib.util
import json
import math
from pathlib import Path
import platform
import random
import statistics
import sys
import time


BLOCKS = 60
PAIRS = 151
WARMUPS = 5
RESAMPLES = 2000
SEED = 200659
STAGES = ('initial', 'confirmation')
NULL_ROLES = ('self', 'aa')
POLICY = 'startup-null-2026-11-06-v1'


def source_sha256():
    return hashlib.sha256(Path(__file__).read_bytes()).hexdigest()


def finite_number(value):
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        return False
    try:
        return math.isfinite(value)
    except OverflowError:
        return False


def interval(stat, pairs=PAIRS):
    if not isinstance(stat, dict) or isinstance(stat.get('pairs'), bool) or not isinstance(stat.get('pairs'), int):
        return None
    if stat['pairs'] != pairs:
        return None
    bounds = stat.get('ci95')
    if not isinstance(bounds, (list, tuple)) or len(bounds) != 2:
        return None
    if any(not finite_number(x) for x in bounds):
        return None
    return bounds if 0 < bounds[0] <= bounds[1] else None


def direction(bounds):
    return 1 if bounds[0] > 1 else -1 if bounds[1] < 1 else 0


def in_band(bounds):
    return bounds[0] >= .95 and bounds[1] <= 1.05


def assess_calibration(blocks, clock):
    """Describe this empirical study, not a universal false-alarm guarantee."""
    report = dict(policy=POLICY, policy_sha256=source_sha256(), status='inconclusive',
                  merge_eligible=False, required_blocks=BLOCKS, pairs_per_stage=PAIRS,
                  precise_blocks=0, scaled_5pct_detected_blocks=0, confirmed_null_blocks=0,
                  invalid_blocks=0, null_confirmations=[], clock=clock,
                  limitations='single host; serial correlation and nonstationarity remain; scaled sensitivity is derived, not measured power')
    if not isinstance(blocks, list) or len(blocks) != BLOCKS or not isinstance(clock, dict):
        return report
    medians = []
    for index, block in enumerate(blocks):
        if not isinstance(block, dict):
            report['invalid_blocks'] += 1
            continue
        values = {stage: {role: interval(block[stage].get(role)) if isinstance(block.get(stage), dict) else None
                          for role in NULL_ROLES}
                  for stage in STAGES}
        median = block.get('base_median_seconds')
        if (any(value is None for stage in values.values() for value in stage.values()) or
                not finite_number(median) or median <= 0):
            report['invalid_blocks'] += 1
            continue
        medians.append(median)
        confirmed = []
        for role in NULL_ROLES:
            first, second = values['initial'][role], values['confirmation'][role]
            a, b = direction(first), direction(second)
            if a and a == b:
                confirmed.append(dict(role=role, direction='slower' if a > 0 else 'faster'))
        if confirmed:
            report['confirmed_null_blocks'] += 1
            report['null_confirmations'].append(dict(block=index, observations=confirmed))
        if all(in_band(value) for stage in values.values() for value in stage.values()):
            report['precise_blocks'] += 1
        if all(value[0] * 1.05 > 1 for stage in values.values() for value in stage.values()):
            report['scaled_5pct_detected_blocks'] += 1
    resolution = clock.get('resolution')
    timer = (clock.get('monotonic') is True and clock.get('adjustable') is False and
             finite_number(resolution) and resolution > 0 and medians and
             resolution <= min(medians) / 1000)
    report['clock_qualified'] = bool(timer)
    if (timer and report['invalid_blocks'] == 0 and report['confirmed_null_blocks'] == 0 and
            report['precise_blocks'] >= 54 and report['scaled_5pct_detected_blocks'] >= 54):
        report['status'] = 'qualified empirical study'
    return report


def load(name):
    spec = importlib.util.spec_from_file_location(name, Path(__file__).with_name(name + '.py'))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def check_binary(tool):
    argv, metadata = tool.get('argv'), tool.get('metadata')
    if (not isinstance(argv, list) or not argv or any(not isinstance(arg, str) for arg in argv) or
            not isinstance(metadata, dict) or metadata.get('instrumented') is not False):
        raise ValueError('startup requires declared ordinary executable roles')
    declared = metadata.get('binary_sha256')
    if not isinstance(declared, str) or len(declared) != 64:
        raise ValueError('startup role lacks declared binary identity')
    if hashlib.sha256(Path(argv[0]).read_bytes()).hexdigest() != declared:
        raise ValueError('startup binary changed or has a mismatched identity')


def null_configuration(metadata):
    config = metadata.get('configuration')
    if not isinstance(config, dict) or config.get('diagnostic_profile') is not False:
        raise ValueError('startup role lacks an ordinary build configuration')
    result = {key: config.get(key) for key in ('build_type', 'generator', 'extensions',
        'multiply_split_point', 'array_storage', 'preparsed_library_image_sha256')}
    for key in ('ipo_policy', 'prepared_divisors_policy', 'prefix_results_policy'):
        policy = config.get(key) or {}
        if not isinstance(policy, dict):
            raise ValueError('invalid startup execution policy')
        result[key] = {field: policy.get(field) for field in ('status', 'schema', 'mode', 'enabled', 'automatic_ipo')}
    return result


def validate_tools(tools):
    if (not isinstance(tools, list) or len(tools) != 3 or any(not isinstance(t, dict) for t in tools) or
            {t.get('name') for t in tools} != {'base', 'aa', 'candidate'}):
        raise ValueError('startup needs exactly base, independent aa, and candidate roles')
    roles = {tool['name']: tool for tool in tools}
    for tool in tools:
        check_binary(tool)
        metadata = tool['metadata']
        source = metadata.get('source_revision')
        if (not isinstance(source, str) or len(source) != 40 or
                any(c not in '0123456789abcdef' for c in source) or
                not all(metadata.get(k) for k in ('compiler', 'flags', 'version'))):
            raise ValueError('startup lacks immutable source/compiler/flags/version provenance')
        null_configuration(metadata)
    base, aa = roles['base'], roles['aa']
    if Path(base['argv'][0]).resolve() == Path(aa['argv'][0]).resolve():
        raise ValueError('startup build-null must be a separate independently built executable')
    for key in ('source_revision', 'compiler', 'standard_library', 'version'):
        if base['metadata'].get(key) != aa['metadata'].get(key):
            raise ValueError('startup null builds differ in ' + key)
    if base['argv'][1:] != aa['argv'][1:] or null_configuration(base['metadata']) != null_configuration(aa['metadata']):
        raise ValueError('startup null builds differ in arguments or execution policy')
    flags = [tool['metadata']['flags'] for tool in (base, aa)]
    flags = [{k: v for k, v in value.items() if k.startswith('CMAKE_')}
             if isinstance(value, dict) else value for value in flags]
    if flags[0] != flags[1]:
        raise ValueError('startup null builds differ in explicit compiler/linker flags')
    return roles


def collect_phase(bench, tools, case, raw, block, stage, seed, deadline, child_timeout=5):
    """Collect exactly one predeclared stage; keep every checked child sample."""
    rows = []
    order = list(tools)
    randomizer = random.Random(seed)
    for round_number in range(-WARMUPS, PAIRS):
        randomizer.shuffle(order)
        for position, tool in enumerate(order):
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise TimeoutError('startup study exceeded its total budget')
            check_binary(tool)
            observed = bench.run_checked(tool, case, min(child_timeout, remaining))
            seconds = observed.get('seconds')
            if observed.get('checked') is not True or not finite_number(seconds) or seconds <= 0:
                raise ValueError('startup child has invalid timing/output evidence')
            row = dict(block=block, stage=stage, workload=case['name'], implementation=tool['name'],
                       phase='warmup' if round_number < 0 else 'measurement', round=round_number,
                       order=position, **observed)
            raw.write(json.dumps(row, sort_keys=True) + '\n')
            raw.flush()
            rows.append(row)
    summary = bench.summarize([case], rows, 'base', seed, RESAMPLES)
    median = statistics.median(row['seconds'] for row in rows
                              if row['phase'] == 'measurement' and row['implementation'] == 'base')
    return dict(ratios=summary['workloads'][0]['candidate_over_reference'], base_median_seconds=median)


def main():
    bench = load('bench-bc')
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--config', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--total-seconds', type=bench.positive_timeout, default=2400)
    parser.add_argument('--child-timeout-seconds', type=bench.positive_timeout, default=5)
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=False)
    report = dict(status='incomplete', policy=POLICY, policy_sha256=source_sha256(), merge_eligible=False,
                  blocks=[], candidate_stages={}, constants=dict(blocks=BLOCKS, pairs=PAIRS,
                  warmups=WARMUPS, resamples=RESAMPLES, seed=SEED),
                  platform=dict(system=platform.system(), machine=platform.machine(), python=platform.python_version()),
                  total_seconds=args.total_seconds, child_timeout_seconds=args.child_timeout_seconds)
    try:
        report['config_sha256'] = hashlib.sha256(args.config.read_bytes()).hexdigest()
        tools = bench.implementations(args.config, require_provenance=True)
        roles = validate_tools(tools)
        report['implementations'] = tools
        info = time.get_clock_info('perf_counter')
        clock = {key: getattr(info, key) for key in ('implementation', 'monotonic', 'adjustable', 'resolution')}
        report['clock'] = clock
        case = next(case for case in bench.corpus('full') if case['name'] == 'startup')
        report['input_sha256'] = hashlib.sha256(case['program'].encode()).hexdigest()
        report['oracle_sha256'] = hashlib.sha256(bench.oracle_payload(case)).hexdigest()
        (args.output / 'input.bc').write_text(case['program'], encoding='utf-8')
        (args.output / 'expected.txt').write_bytes(bench.oracle_payload(case))
        alias = dict(copy.deepcopy(roles['base']), name='self')
        null_tools = [roles['base'], alias, roles['aa']]
        deadline = time.monotonic() + args.total_seconds
        with (args.output / 'samples.jsonl').open('w', encoding='utf-8') as raw:
            for block in range(BLOCKS):
                row = dict(block=block)
                report['blocks'].append(row)
                medians = []
                for index, stage in enumerate(STAGES):
                    phase = collect_phase(bench, null_tools, case, raw, block, stage,
                                          SEED + block * 2 + index, deadline, args.child_timeout_seconds)
                    row[stage] = phase['ratios']
                    medians.append(phase['base_median_seconds'])
                row['base_median_seconds'] = min(medians)
            report['calibration'] = assess_calibration(report['blocks'], clock)
            # Always retain the declared candidate stages, even if calibration
            # is inconclusive. Such stages cannot claim startup acceptance.
            for index, stage in enumerate(STAGES):
                phase = collect_phase(bench, [roles[name] for name in ('base', 'aa', 'candidate')],
                                      case, raw, 'candidate', stage, SEED + BLOCKS * 2 + index,
                                      deadline, args.child_timeout_seconds)
                report['candidate_stages'][stage] = phase['ratios']
            report['candidate_assessment'] = assess_candidate(report['calibration'], report['candidate_stages'])
        report['status'] = 'collected'
        return 0
    except (ValueError, OSError, KeyError, TypeError, bench.BenchmarkError) as error:
        report['error'] = str(error)
        print('startup study failed: ' + str(error), file=sys.stderr)
        return 1
    finally:
        raw_path = args.output / 'samples.jsonl'
        if raw_path.is_file():
            report['raw_samples_sha256'] = hashlib.sha256(raw_path.read_bytes()).hexdigest()
        (args.output / 'manifest.json').write_text(json.dumps(report, indent=2, sort_keys=True) + '\n', encoding='utf-8')


def assess_candidate(calibration, stages):
    """No positive significant difference is permitted; mixed stages stay unknown."""
    report = dict(status='inconclusive', merge_eligible=False,
                  scope='startup only; no Howard-margin exception or overall acceptance')
    if (not isinstance(calibration, dict) or calibration.get('status') != 'qualified empirical study' or
            calibration.get('policy_sha256') != source_sha256() or not isinstance(stages, dict)):
        return report
    controls, candidate = [], []
    for stage in STAGES:
        row = stages.get(stage)
        if not isinstance(row, dict):
            return report
        aa, actual = interval(row.get('aa')), interval(row.get('candidate'))
        if aa is None or actual is None or not in_band(aa):
            return report
        controls.append(aa)
        candidate.append(actual)
    if direction(controls[0]) and direction(controls[0]) == direction(controls[1]):
        return report
    positive = [bounds[0] > 1 for bounds in candidate]
    if all(positive):
        report['status'] = 'confirmed startup regression'
    elif not any(positive):
        report['status'] = 'no statistically significant startup regression observed'
    return report


if __name__ == '__main__':
    sys.exit(main())
