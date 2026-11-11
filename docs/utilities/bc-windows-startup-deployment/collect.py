#!/usr/bin/env python3
"""Collect checked startup controls while crossing build and launch path."""
# SPDX-License-Identifier: Apache-2.0
import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import platform
import random
import shutil
import subprocess
import time

RUNTIME = '19ef80161fd2bdb24bf9ba7e93942caa97ea6ea1'
SCRIPTS = Path(__file__).resolve().parents[3] / 'src/bc/scripts'
BLOCKS = 20
SEED = 200729
TOTAL_SECONDS = 1800
CHILD_SECONDS = 5
BUILDS = ('base', 'aa')
SITES = ('site-a', 'site-b')


def load(name):
    spec = importlib.util.spec_from_file_location(name, SCRIPTS / (name + '.py'))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def assignments():
    """Reverse the orientation within every adjacent pair of blocks."""
    randomizer = random.Random(SEED)
    for _ in range(BLOCKS // 2):
        first = randomizer.randrange(2)
        yield first
        yield 1 - first


def deploy(originals, output, orientation):
    tools = {}
    locations = {}
    for role, site in zip(BUILDS, SITES[orientation:] + SITES[:orientation]):
        tool = originals[role]
        directory = output / site
        directory.mkdir(exist_ok=True)
        executable = directory / 'bc.exe'
        shutil.copy2(tool['argv'][0], executable)
        tools[role] = dict(tool, argv=[str(executable.resolve()), *tool['argv'][1:]])
        locations[role] = site
    tools['self'] = dict(tools['base'], name='self')
    return [tools[role] for role in ('base', 'self', 'aa')], locations


def collect(config, output, bench, startup):
    output.mkdir(parents=True, exist_ok=False)
    info = time.get_clock_info('perf_counter')
    report = dict(status='incomplete', merge_eligible=False, runtime=RUNTIME,
                  collector=subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip(),
                  platform=dict(system=platform.system(), machine=platform.machine(),
                                python=platform.python_version()),
                  clock={key: getattr(info, key) for key in
                         ('implementation', 'monotonic', 'adjustable', 'resolution')},
                  constants=dict(blocks=BLOCKS, pairs=startup.PAIRS, warmups=startup.WARMUPS,
                                 resamples=startup.RESAMPLES, seed=SEED, total_seconds=TOTAL_SECONDS,
                                 child_seconds=CHILD_SECONDS),
                  harness={name: hashlib.sha256((SCRIPTS / name).read_bytes()).hexdigest()
                           for name in ('bench-bc.py', 'startup-calibration.py')}, blocks=[])
    raw_path = output / 'samples.jsonl'
    try:
        report['config_sha256'] = hashlib.sha256(config.read_bytes()).hexdigest()
        tools = bench.implementations(config, require_provenance=True)
        originals = startup.validate_tools(tools)
        if any(originals[role]['metadata']['source_revision'] != RUNTIME for role in BUILDS):
            raise ValueError('both control builds must use the fixed accepted runtime')
        report['originals'] = {role: originals[role] for role in BUILDS}
        for role in BUILDS:
            directory = output / 'originals' / role
            directory.mkdir(parents=True)
            shutil.copy2(originals[role]['argv'][0], directory / 'bc.exe')
        case, = [case for case in bench.corpus('full') if case['name'] == 'startup']
        (output / 'input.bc').write_bytes(case['program'].encode())
        (output / 'expected.txt').write_bytes(bench.oracle_payload(case))
        deadline = time.monotonic() + TOTAL_SECONDS
        with raw_path.open('w', encoding='utf-8') as raw:
            for number, orientation in enumerate(assignments()):
                deployed, locations = deploy(originals, output, orientation)
                block = dict(block=number, sites=locations, stages={})
                report['blocks'].append(block)
                for index, stage in enumerate(startup.STAGES):
                    block['stages'][stage] = startup.collect_phase(
                        bench, deployed, case, raw, number, stage,
                        SEED + 2 * number + index, deadline, CHILD_SECONDS)
        report['status'] = 'collected'
        return 0
    except (OSError, ValueError, KeyError, TypeError, bench.BenchmarkError) as error:
        report['error'] = str(error)
        return 1
    finally:
        if raw_path.is_file():
            report['raw_sha256'] = hashlib.sha256(raw_path.read_bytes()).hexdigest()
        (output / 'manifest.json').write_text(json.dumps(report, indent=2) + '\n', encoding='utf-8')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--config', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    if os.name != 'nt' or os.environ.get('GITHUB_RUN_ATTEMPT') != '1':
        parser.error('the declared study runs once on a hosted Windows runner')
    return collect(args.config, args.output, load('bench-bc'), load('startup-calibration'))


if __name__ == '__main__':
    raise SystemExit(main())
