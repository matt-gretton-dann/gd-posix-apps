#!/usr/bin/env python3
"""Compare fixed accepted-source Windows deployments using CPU and wrapper time.

This is a diagnostic, never runtime acceptance. Reuse the existing native
process-handle adapter; other platforms report unavailable without executing bc.
SPDX-License-Identifier: Apache-2.0
"""
import argparse
import copy
import hashlib
import importlib.util
import json
import math
import os
from pathlib import Path
import random
import shutil
import statistics
import sys
import time

BASE = 'e2de128f090f1d3379927b0e31b6882ca41fdda0'
SAMPLES, WARMUPS, ITERATIONS = 19, 3, 4096
TOTAL_SECONDS, CHILD_SECONDS = 6600, 120
MINIMUM_CPU_SECONDS = .5
SCRIPTS = Path(__file__).resolve().parent


def load(name):
    spec = importlib.util.spec_from_file_location('process_controls_' + name,
                                                 str(SCRIPTS / (name + '.py')))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def cases(bench, preset):
    """Predetermined regimes, fixed counts; no duration calibration or retry."""
    selected = [case for case in bench.math_corpus(preset)
                if case['parameters']['mode'] == 'repeated' and
                ((case['parameters']['scale'] == 20 and
                  ('_regular_' in case['name'] or '_turning_' in case['name'])) or
                 (case['parameters']['scale'] == 100 and
                  '_argument_dominates_' in case['name']))]
    if len(selected) != 8:
        raise ValueError('fixed process-control regime inventory changed')
    return [bench.math_iterations(case, ITERATIONS) for case in selected]


def deployments(first, second, directory, bench):
    """One same-file alias, one physical copy, and an independent build."""
    for entry in (first, second):
        if (len(entry['argv']) != 1 or entry['metadata']['source_revision'] != BASE or
                entry['metadata'].get('instrumented') is not False or
                bench.digest_file(entry['argv'][0]) != entry['metadata']['binary_sha256']):
            raise ValueError('control needs an unchanged ordinary accepted-source image')
    a, b = [Path(entry['argv'][0]).resolve(strict=True) for entry in (first, second)]
    if os.path.samefile(str(a), str(b)):
        raise ValueError('independent build must have its own file identity')
    directory.mkdir(exist_ok=False)
    image = directory / a.name
    shutil.copyfile(str(a), str(image))
    if (os.path.samefile(str(a), str(image)) or
            bench.digest_file(image) != first['metadata']['binary_sha256']):
        raise ValueError('deployment must be a distinct byte-identical file')
    entries = []
    for name, original, binary in (('reference', first, a), ('alias', first, a),
                                   ('copy', first, image), ('independent', second, b)):
        tool = copy.deepcopy(original)
        tool.update(name=name, argv=[str(binary), '-l'])
        entries.append(tool)
    return entries


def checked(bench, profile, tool, case, deadline):
    if bench.digest_file(tool['argv'][0]) != tool['binary_sha256']:
        raise ValueError('measured image changed before invocation')
    payload = case['program'].encode('utf-8')
    environment = bench.environment()
    environment.pop('GD_BC_PROFILE_OUTPUT', None)
    remaining = deadline - time.monotonic()
    if remaining <= 0:
        raise ValueError('process-control experiment exhausted its deadline')
    started = time.perf_counter()
    child, fields = profile.run_windows_process(tool['argv'], payload, environment,
                                                min(CHILD_SECONDS, remaining))
    elapsed = time.perf_counter() - started
    # The wrapper includes launch, communication and native accounting queries;
    # it is not a replacement for bench-bc.py's production elapsed protocol.
    profile.check(bench, case, 1, child.returncode, child.stdout, child.stderr)
    if not math.isfinite(elapsed) or elapsed <= 0:
        raise ValueError('invalid wrapper duration')
    return dict(seconds=elapsed, native=fields, checked=True,
                output_sha256=hashlib.sha256(child.stdout).hexdigest(),
                output_bytes=len(child.stdout))


def measure(bench, profile, tools, workloads, target, deadline, seed):
    """Stream all checked samples in fixed seeded case/role order."""
    target.mkdir(exist_ok=False)
    prepared = target / 'workloads'
    prepared.mkdir()
    for case in workloads:
        (prepared / (case['name'] + '.bc')).write_bytes(case['program'].encode('utf-8'))
        (prepared / (case['name'] + '.expected')).write_bytes(bench.oracle_payload(case))
    manifest = dict(seed=seed, samples=SAMPLES, warmups=WARMUPS,
                    implementations=tools, corpus=workloads,
                    harness_sha256=bench.digest_file(SCRIPTS / 'bench-bc.py'),
                    profile_adapter_sha256=bench.digest_file(SCRIPTS / 'profile-bc.py'))
    bench.write_json(target / 'manifest.json', manifest)
    rng, rows = random.Random(seed), []
    with (target / 'samples.jsonl').open('x', encoding='utf-8') as log:
        for phase, count in (('warmup', WARMUPS), ('measurement', SAMPLES)):
            for number in range(count):
                ordered = list(workloads)
                rng.shuffle(ordered)
                for case in ordered:
                    roles = list(tools)
                    rng.shuffle(roles)
                    for order, tool in enumerate(roles):
                        row = dict(phase=phase, round=number, workload=case['name'],
                                   implementation=tool['name'], order=order,
                                   **checked(bench, profile, tool, case, deadline))
                        log.write(json.dumps(row, sort_keys=True) + '\n')
                        log.flush()
                        rows.append(row)
    return summarize(bench, tools, workloads, rows, seed)


def cpu_seconds(row):
    cpu = row['native'].get('cpu_time', {})
    if cpu.get('status') != 'collected':
        return None
    values = [cpu.get(key) for key in ('user_ticks_100ns', 'system_ticks_100ns')]
    if any(type(value) is not int or value < 0 for value in values):
        raise ValueError('invalid native CPU ticks')
    return sum(values) / 10000000


def summarize(bench, tools, workloads, rows, seed):
    result = []
    for case in workloads:
        observations = {tool['name']: [r for r in rows if r['phase'] == 'measurement'
                        and r['workload'] == case['name'] and
                        r['implementation'] == tool['name']] for tool in tools}
        if any(len(values) != SAMPLES for values in observations.values()):
            raise ValueError('incomplete fixed process-control samples')
        wall = {name: [r['seconds'] for r in values] for name, values in observations.items()}
        cpu = {name: [cpu_seconds(r) for r in values] for name, values in observations.items()}
        complete = all(value is not None for values in cpu.values() for value in values)
        positive = complete and all(value > 0 for values in cpu.values() for value in values)
        medians = {name: statistics.median(values) for name, values in cpu.items()} if complete else None
        adequate = complete and all(value >= MINIMUM_CPU_SECONDS for value in medians.values())
        ratios = []
        for tool in tools[1:]:
            name = tool['name']
            ratios.append(dict(implementation=name,
                wrapper_wall=bench.paired_statistics(wall[name], wall['reference'], seed),
                cpu=bench.paired_statistics(cpu[name], cpu['reference'], seed) if positive else None))
        result.append(dict(name=case['name'], comparisons=ratios,
            cpu_status='positive' if positive else 'zero or unavailable; ratio unqualified',
            cpu_role_medians_seconds=medians, cpu_duration_adequate=adequate,
            minimum_role_median_cpu_seconds=MINIMUM_CPU_SECONDS,
            cpu_observed_seconds={name: sorted(set(v for v in values if v is not None))
                                      for name, values in cpu.items()},
            inference='within fixed deployment and replica only; no acceptance or source causality'))
    return result


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--replica', type=int, choices=(1, 2, 3), required=True)
    args = parser.parse_args(argv)
    ci, bench = load('benchmark-math-ci'), load('bench-bc')
    root = args.output.resolve()
    root.mkdir(exist_ok=False)
    evidence = root / 'evidence'
    evidence.mkdir()
    commands = ci.Commands(root, TOTAL_SECONDS)
    tools = []
    report = dict(schema=1, source_revision=BASE, replica=args.replica, status='incomplete',
        controller_sha256=bench.digest_file(__file__),
        declared_controller_revision=os.environ.get('BC_CONTROL_SOURCE'), samples=SAMPLES, warmups=WARMUPS,
        iterations=ITERATIONS, total_seconds=TOTAL_SECONDS, child_seconds=CHILD_SECONDS,
        minimum_role_median_cpu_seconds=MINIMUM_CPU_SECONDS, performance_acceptance=False,
        presets={}, limitations=[
            'Fixed accepted-source deployment diagnostic; not a runtime candidate',
            'Wrapper wall time includes native accounting queries; not production benchmark timing',
            'CPU counter units may be finer than actual accounting resolution',
            'One physical copy and two independent builds per replica, not a population of layouts',
            'Build and deployment paths differ and are recorded, not isolated causal factors',
            'No pooling, confirmation, adaptive counts, restart, startup or source-causal inference'])
    result = 1
    try:
        bench.write_json(evidence / 'host.json', load('benchmark-ci').host_info())
        if not load('windows-image-probe').native_windows_64():
            report.update(status='unavailable', reason='requires native 64-bit Windows Python')
            result = 0
        else:
            built = []
            for name in ('build0', 'build1'):
                destination = root / name
                commands.run([sys.executable, str(SCRIPTS / 'build-gd-bc.py'),
                    '--source', str(args.source.resolve()), '--revision', BASE,
                    '--output', str(destination), '--name', name, '--split-point', '126',
                    '--extensions', 'ON'], timeout=1800)
                entry, = json.loads((destination / 'implementations.json').read_text())['implementations']
                built.append(entry)
                tools.append(entry)
            tools = deployments(built[0], built[1], root / 'copy', bench)
            bench.write_json(evidence / 'implementations.json', dict(implementations=tools))
            tools = bench.implementations(evidence / 'implementations.json', require_provenance=True)
            report['deployments'] = [dict(name=tool['name'], argv=tool['argv'],
                binary_sha256=tool['binary_sha256'],
                same_file_as_reference=os.path.samefile(tool['argv'][0], tools[0]['argv'][0]),
                file_identity_sha256=hashlib.sha256(repr((Path(tool['argv'][0]).stat().st_dev,
                    Path(tool['argv'][0]).stat().st_ino)).encode('ascii')).hexdigest())
                for tool in tools]
            # All builds and file copies finish before the first checked sample.
            for index, preset in enumerate(('full', 'holdout')):
                seed = 19768000 + args.replica * 10 + index
                report['presets'][preset] = measure(bench, load('profile-bc'), tools,
                    cases(bench, preset), evidence / preset, commands.deadline, seed)
                bench.write_json(evidence / 'report.json', report)
            report['status'] = 'diagnostic complete; no acceptance decision'
            result = 0
    except Exception as error:
        report.update(status='failed', failure=str(error))
    finally:
        # Preserve all completed ordinary images, even after an incomplete run.
        try:
            if tools:
                load('collect-bc-codegen').collect(tools, evidence / 'codegen',
                                                  timeout=15, deadline=commands.deadline)
        except Exception as error:
            report['status'] = 'failed'
            report['preservation_failure'] = str(error)
            result = 1
        for name in ('build0', 'build1'):
            source = root / name
            if source.is_dir():
                target = evidence / name
                target.mkdir(exist_ok=True)
                for path in source.iterdir():
                    if path.is_file():
                        shutil.copyfile(str(path), str(target / path.name))
        report['commands'] = commands.commands
        for path in root.glob('command-*.log'):
            shutil.copyfile(str(path), str(evidence / path.name))
        bench.write_json(evidence / 'report.json', report)
    return result


if __name__ == '__main__':
    sys.exit(main())
