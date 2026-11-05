#!/usr/bin/env python3
"""Collect checked integer-tier CPU/RSS and allocation diagnostics, never timing.

Build both modes from one immutable revision. Reuse that revision's builders,
corpus and native resource adapters; unsupported adapters remain unavailable.
Python 3.6+; SPDX-License-Identifier: Apache-2.0
"""
import argparse
import importlib.util
import json
import os
from pathlib import Path
import shutil
import sys


def load(directory, name):
    spec = importlib.util.spec_from_file_location(name.replace('-', '_'), str(directory / (name + '.py')))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def selected_cases(bench, preset):
    names = (['startup', 'calls_200000', 'recursion_50_x1000'] if preset == 'full' else
             ['startup', 'calls_175000', 'recursion_56_x875'])
    return bench.corpus(preset, selected=names)


def profile_record(bench, tool, case, destination, enabled, timeout):
    if tool['metadata'].get('instrumented') is not True:
        raise ValueError('allocation diagnostics require an instrumented binary')
    if bench.digest_file(tool['argv'][0]) != tool['metadata']['binary_sha256']:
        raise ValueError('profile binary identity changed')
    previous = os.environ.get('GD_BC_PROFILE_OUTPUT')
    try:
        os.environ['GD_BC_PROFILE_OUTPUT'] = str(destination)
        observed = bench.run_checked(tool, case, timeout)
        observed.pop('seconds')  # Instrumented elapsed time is never evidence.
        counters = json.loads(destination.read_text(encoding='utf-8'))
        if (counters.get('instrumented') is not True or counters['total']['live_bytes'] != 0 or
                any(row['live_bytes'] != 0 for row in counters['allocations'].values())):
            raise ValueError('unreleased or invalid profile storage')
        events = counters['events']
        attempts = events.get('integer_function_attempts', 0)
        if enabled and case['group'] in ('calls', 'recursion'):
            if (attempts <= 0 or events.get('integer_function_successes') != attempts or
                    events.get('integer_function_abandonments') != 0):
                raise ValueError('target workload did not execute the integer tier successfully')
        elif attempts != 0:
            raise ValueError('unexpected integer execution in the control workload')
        return dict(counters=destination.name, **observed)
    finally:
        if previous is None:
            os.environ.pop('GD_BC_PROFILE_OUTPUT', None)
        else:
            os.environ['GD_BC_PROFILE_OUTPUT'] = previous


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', type=Path, required=True)
    parser.add_argument('--revision', required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    source = args.source.resolve()
    scripts = source / 'src/bc/scripts'
    builder = load(scripts, 'build-gd-bc')
    revision = builder.revision_value(args.revision)
    bench = load(scripts, 'bench-bc')
    ci = load(scripts, 'benchmark-math-ci')
    helper = load(scripts, 'build-bc-comparators')
    root = args.output.resolve()
    root.mkdir(parents=True, exist_ok=False)
    evidence = root / 'evidence'
    evidence.mkdir()
    ci.write(evidence / 'host.json', load(scripts, 'benchmark-ci').host_info())
    manifest = dict(schema=1, source_revision=revision, status='incomplete',
                    collector_sha256=bench.digest_file(__file__), observations=[],
                    scope='one cold invocation per full/holdout workload and mode; not paired elapsed performance')
    commands = ci.Commands(root, 7200)
    roles = []
    try:
        for mode in ('OFF', 'ON'):
            for instrumented in (False, True):
                name = mode.lower() + ('-profile' if instrumented else '-release')
                roles.append(name)
                destination = root / name
                command = [sys.executable, str(scripts / 'build-gd-bc.py'), '--source', str(source),
                           '--revision', revision, '--split-point', '126', '--output', str(destination),
                           '--name', name, '--integer-functions', mode]
                if instrumented:
                    command.append('--diagnostic-profile')
                commands.run(command, timeout=1200)
                config = destination / 'implementations.json'
                tool = json.loads(config.read_text(encoding='utf-8'))['implementations'][0]
                if tool['metadata']['source_revision'] != revision:
                    raise ValueError('builder source revision mismatch')
                builder.require_integer_mode(helper, tool, mode == 'ON')
                if tool['metadata']['instrumented'] is not instrumented:
                    raise ValueError('builder instrumentation mismatch')
                if not instrumented:
                    # This existing loader validates release hashes/provenance.
                    tool = bench.implementations(config, require_provenance=True)[0]
                for preset in ('full', 'holdout'):
                    cases = selected_cases(bench, preset)
                    target = evidence / (name + '-' + preset)
                    target.mkdir()
                    if not instrumented:
                        ci.resource_diagnostics(bench, config, cases, target / 'native', timeout=600)
                    else:
                        for case in cases:
                            (target / (case['name'] + '.bc')).write_bytes(case['program'].encode('utf-8'))
                            (target / (case['name'] + '.expected')).write_bytes(bench.oracle_payload(case))
                            row = profile_record(bench, tool, case, target / (case['name'] + '.json'),
                                                 mode == 'ON', 600)
                            manifest['observations'].append(dict(role=name, preset=preset,
                                name=case['name'], parameters=case['parameters'],
                                program_sha256=bench.hashlib.sha256(case['program'].encode('utf-8')).hexdigest(),
                                oracle_sha256=bench.hashlib.sha256(bench.oracle_payload(case)).hexdigest(),
                                counters_directory=target.name, **row))
                    ci.write(evidence / 'manifest.json', manifest)
        manifest['status'] = 'checked; native CPU/RSS availability is per observation'
        return 0
    except (ci.CommandFailure, helper.BuildError, bench.BenchmarkError, OSError, ValueError, KeyError) as error:
        manifest['failure'] = str(error)
        print(str(error), file=sys.stderr)
        return 1
    finally:
        manifest['commands'] = commands.commands
        ci.write(evidence / 'manifest.json', manifest)
        for path in root.glob('command-*.log'):
            shutil.copyfile(path, evidence / path.name)
        for name in roles:
            target = evidence / name
            target.mkdir(exist_ok=True)
            for path in (root / name).glob('*'):
                if path.is_file():
                    shutil.copyfile(path, target / path.name)
            config = root / name / 'implementations.json'
            if config.is_file():
                tool = json.loads(config.read_text(encoding='utf-8'))['implementations'][0]
                shutil.copyfile(tool['argv'][0], target / ('bc.exe' if os.name == 'nt' else 'bc'))
            for directory in ('integer-policy', 'prepared-policy'):
                path = root / name / directory
                if path.is_dir():
                    shutil.copytree(str(path), str(target / directory))


if __name__ == '__main__':
    sys.exit(main())
