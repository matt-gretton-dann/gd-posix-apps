#!/usr/bin/env python3
"""Attribute the fixed integer-tier Bessel loss without new timing samples."""
import argparse
import importlib.util
import json
import math
import os
from pathlib import Path
import shutil
import subprocess
import sys
import time

REVISION = '2aba8ec2cc1ed23a3806079d2f6dc1caa6cfc27d'


def load(directory, name):
    spec = importlib.util.spec_from_file_location(name.replace('-', '_'), str(directory / (name + '.py')))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def frozen_cases(bench, directory):
    provenance = json.loads((directory / 'provenance.json').read_text())
    if provenance['source'] != REVISION:
        raise ValueError('unexpected frozen source')
    for name, digest in provenance['files'].items():
        if bench.digest_file(directory / 'frozen' / name) != digest:
            raise ValueError('frozen input changed: ' + name)
    cases, replay = bench.replay_corpus(directory / 'frozen', 'math', provenance['workloads'])
    return cases, replay, provenance


def software_profile(bench, tool, case, destination, enabled, timeout=600):
    if tool['metadata'].get('instrumented') is not True:
        raise ValueError('software counters require a separate instrumented binary')
    if bench.digest_file(tool['argv'][0]) != tool['metadata']['binary_sha256']:
        raise ValueError('profile binary identity changed')
    previous = os.environ.get('GD_BC_PROFILE_OUTPUT')
    try:
        os.environ['GD_BC_PROFILE_OUTPUT'] = str(destination)
        observed = bench.run_checked(tool, case, timeout)
        observed.pop('seconds')  # Instrumented elapsed values are never retained.
        counters = json.loads(destination.read_text())
        if (counters.get('instrumented') is not True or counters['total']['live_bytes'] != 0 or
                any(row['live_bytes'] != 0 for row in counters['allocations'].values())):
            raise ValueError('unreleased or invalid software-profile storage')
        events = counters['events']
        attempts = events.get('integer_function_attempts', 0)
        if attempts != (events.get('integer_function_successes', 0) +
                        events.get('integer_function_abandonments', 0)):
            raise ValueError('inconsistent completed integer attempts')
        if not enabled and any(value for key, value in events.items()
                               if key.startswith('integer_function_')):
            raise ValueError('integer execution in the OFF control')
        return dict(counters=destination.name, **observed)
    finally:
        if previous is None:
            os.environ.pop('GD_BC_PROFILE_OUTPUT', None)
        else:
            os.environ['GD_BC_PROFILE_OUTPUT'] = previous


def remaining(commands, maximum, children=1):
    budget = commands.deadline - time.monotonic()
    if budget <= 0:
        raise ValueError('diagnostic collection exhausted its total deadline')
    return min(maximum, budget / children)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', type=Path, required=True)
    parser.add_argument('--scripts', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--perf', type=Path, required=True)
    args = parser.parse_args()
    scripts = args.scripts.resolve()
    bench = load(scripts, 'bench-bc')
    ci = load(scripts, 'benchmark-math-ci')
    builder = load(scripts, 'build-gd-bc')
    helper = load(scripts, 'build-bc-comparators')
    profiler = load(scripts, 'profile-bc')
    fixture = Path(__file__).resolve().parent
    cases, replay, provenance = frozen_cases(bench, fixture)
    root = args.output.resolve()
    root.mkdir(parents=True, exist_ok=False)
    evidence = root / 'evidence'
    evidence.mkdir()
    shutil.copytree(str(fixture / 'frozen'), str(evidence / 'frozen-inputs'))
    ci.write(evidence / 'host.json', load(scripts, 'benchmark-ci').host_info())
    manifest = dict(source=REVISION, purpose='diagnostic attribution; no new timing acceptance',
                    replay=replay, frozen_provenance=provenance, status='incomplete', observations=[],
                    collector_sha256=bench.digest_file(__file__),
                    script_hashes={p.name: bench.digest_file(p) for p in scripts.glob('*.py')})
    summary = json.loads((fixture / 'frozen/summary.json').read_text())
    observations = {row['name']: row for row in summary['workloads']}
    commands = ci.Commands(root, 3600)
    roles = []
    try:
        for mode, name in [('OFF', 'gd-base'), ('ON', 'gd-portable')]:
            for instrumented in (False, True):
                role = name + ('-profile' if instrumented else '')
                roles.append(role)
                destination = root / role
                command = [sys.executable, str(scripts / 'build-gd-bc.py'), '--source',
                           str(args.source.resolve()), '--revision', REVISION, '--split-point', '126',
                           '--output', str(destination), '--name', name, '--integer-functions', mode]
                if instrumented:
                    command.append('--diagnostic-profile')
                commands.run(command, timeout=1200)
                # These are fresh, controller-owned clones. Keep their source
                # and Git objects while dropping duplicate tracked docs from
                # the working trees before building the next role.
                commands.run(['git', '-C', str(destination / 'source'), 'sparse-checkout',
                              'set', '--no-cone', '/*', '!/*/', '/src/', '/include/',
                              '/libgdsup/', '/cmake/'], timeout=60)
                config = destination / 'implementations.json'
                tool = json.loads(config.read_text())['implementations'][0]
                if (tool['metadata']['source_revision'] != REVISION or
                        tool['metadata']['instrumented'] is not instrumented):
                    raise ValueError('builder source or instrumentation mismatch')
                builder.require_integer_mode(helper, tool, mode == 'ON')
                tool['argv'].append('-l')
                library = destination / 'source/src/bc/math-library.bc'
                tool['metadata']['library'] = ci.embedded_library_metadata(
                    tool, REVISION, helper.digest(library))
                config = destination / 'math-implementations.json'
                ci.write(config, dict(implementations=[tool]))
                if not instrumented:
                    tool = bench.implementations(config, require_provenance=True)[0]
                target = evidence / role
                target.mkdir()
                if not instrumented:
                    for executable, option, output in [('nm', '--size-sort', 'symbols.txt'),
                                                       ('objdump', '-d', 'disassembly.txt')]:
                        argv = [executable, option, '--demangle', tool['argv'][0]]
                        with (target / output).open('w') as log:
                            subprocess.run(argv, stdout=log, stderr=subprocess.STDOUT,
                                           timeout=remaining(commands, 60), check=True)
                        manifest.setdefault('native_dumps', []).append(dict(
                            argv=argv, path=role + '/' + output,
                            sha256=bench.digest_file(target / output)))
                for case in cases:
                    if instrumented:
                        result = software_profile(bench, tool, case,
                                                  target / (case['name'] + '.json'), mode == 'ON',
                                                  remaining(commands, 300))
                    else:
                        median = observations[case['name']]['implementations'][name]['median_seconds']
                        repeats = max(1, min(10000, int(math.ceil(5 / max(median, 0.000001)))))
                        repeats = min(repeats, max(1, 64 * 1024 * 1024 //
                                                  len(case['program'].encode('utf-8'))))
                        # The CPU adapter records and then decodes in two
                        # separately bounded children under the same budget.
                        cpu = profiler.cpu_profile(bench, tool, case, repeats, target,
                                                   remaining(commands, 300, 2), args.perf.resolve())
                        trace = (target / (case['name'] + '.perf.txt')).read_text()
                        if cpu.get('event') != 'cpu-clock:u' or '/bc)' not in trace:
                            raise ValueError('native samples do not identify the interpreter')
                        result = dict(cpu=cpu, memory=profiler.rss_profile(
                            bench, tool, case, target, remaining(commands, 300)))
                    manifest['observations'].append(dict(role=role, name=case['name'], result=result))
                    ci.write(evidence / 'manifest.json', manifest)
        manifest['status'] = 'checked diagnostic collection; not a performance decision'
        return 0
    except (ci.CommandFailure, helper.BuildError, bench.BenchmarkError, OSError, ValueError,
            KeyError, subprocess.SubprocessError) as error:
        manifest['failure'] = str(error)
        print('Diagnostic collection failed: ' + str(error), file=sys.stderr)
        return 1
    finally:
        manifest['commands'] = commands.commands
        for role in roles:
            target = evidence / role
            target.mkdir(exist_ok=True)
            for path in (root / role).glob('*'):
                if path.is_file():
                    shutil.copyfile(path, target / path.name)
            config = root / role / 'implementations.json'
            if config.is_file():
                tool = json.loads(config.read_text())['implementations'][0]
                shutil.copyfile(tool['argv'][0], target / 'bc')
            for relative in ['build/CMakeCache.txt', 'build/src/bc/liblibbc.a',
                             'source/src/bc/math-library.bc',
                             'build/src/bc/CMakeFiles/libbc.dir/vm.cc.o',
                             'build/src/bc/CMakeFiles/bc-integer-engine.dir/integer-function.cc.o']:
                path = root / role / relative
                if path.is_file():
                    shutil.copyfile(path, target / path.name)
            policy = root / role / 'integer-policy'
            if policy.is_dir():
                shutil.copytree(str(policy), str(target / 'integer-policy'))
        for path in root.glob('command-*.log'):
            shutil.copyfile(path, evidence / path.name)
        manifest['retained_files'] = {
            str(path.relative_to(evidence)): bench.digest_file(path)
            for path in evidence.rglob('*') if path.is_file() and path.name != 'manifest.json'}
        ci.write(evidence / 'manifest.json', manifest)


if __name__ == '__main__':
    sys.exit(main())
