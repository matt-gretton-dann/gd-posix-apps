#!/usr/bin/env python3
"""Collect checked release CPU/RSS diagnostics separately from benchmark timings.

Darwin sample and native time adapters are optional. Other hosts retain an
explicit unavailable status rather than invented CPU/RSS observations.
Python 3.6+; SPDX-License-Identifier: Apache-2.0
"""
import argparse
import contextlib
import importlib.util
import json
import math
import os
from pathlib import Path
import platform
import re
import signal
import subprocess
import sys
import tempfile


def load_benchmark():
    path = Path(__file__).with_name('bench-bc.py')
    spec = importlib.util.spec_from_file_location('bc_benchmark', str(path))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def check(bench, case, repetitions, status, stdout, stderr):
    if status != 0 or stderr:
        raise bench.BenchmarkError('{} diagnostic child failed: {} {}'.format(
            case['name'], status, stderr.decode('utf-8', errors='replace')[:500]))
    actual = bench.canonical_output(stdout)
    if 'oracle' in case:
        width = 1 + len(case['oracle'].get('expected_state', []))
        if len(actual) != width * repetitions:
            raise bench.BenchmarkError(case['name'] + ' diagnostic result count is incorrect')
        for index in range(repetitions):
            bench.check_output(case, actual[index * width:(index + 1) * width],
                               '{} diagnostic repetition {}'.format(case['name'], index))
    else:
        expected = ('\n'.join(case['expected'] * repetitions) + '\n').encode('utf-8')
        if actual != bench.canonical_output(expected):
            raise bench.BenchmarkError(case['name'] + ' diagnostic output is incorrect')


@contextlib.contextmanager
def diagnostic_input(bench, tool, case, repetitions):
    """Preserve transport and recheck declared files immediately before launch."""
    for artifact in tool.get('input_artifacts', []):
        if bench.digest_file(artifact['realpath']) != artifact['sha256']:
            raise bench.BenchmarkError('declared input artifact changed before diagnostic')
    source = (case['program'] * repetitions).encode('utf-8')
    mode = case.get('input_mode', 'stdin')
    if mode not in ('stdin', 'file'):
        raise bench.BenchmarkError('unknown diagnostic input transport')
    metadata = dict(input_mode=mode, expanded_program_sha256=bench.hashlib.sha256(source).hexdigest(),
                    expanded_program_bytes=len(source))
    with tempfile.TemporaryDirectory(prefix='bc-profile-') as directory:
        argv = list(tool['argv'])
        payload = source
        if mode == 'file':
            path = Path(directory) / 'workload.bc'
            path.write_bytes(source)
            argv.append(str(path))
            payload = b''
        yield argv, payload, metadata


def cpu_profile(bench, tool, case, repetitions, root, timeout):
    output = root / (case['name'] + '.sample.txt')
    environment = bench.environment()
    environment.pop('GD_BC_PROFILE_OUTPUT', None)
    with diagnostic_input(bench, tool, case, repetitions) as (argv, payload, metadata), \
            tempfile.TemporaryFile() as incoming, tempfile.TemporaryFile() as outgoing, \
            tempfile.TemporaryFile() as errors:
        incoming.write(payload)
        incoming.seek(0)
        with subprocess.Popen(argv, stdin=incoming, stdout=outgoing, stderr=errors,
                              env=environment) as child:
            try:
                if platform.system() == 'Darwin' and Path('/usr/bin/sample').is_file():
                    command = ['/usr/bin/sample', str(child.pid), '3', '1', '-mayDie',
                               '-file', str(output)]
                    sampled = subprocess.run(command, stdout=subprocess.PIPE,
                                             stderr=subprocess.STDOUT, timeout=10)
                    (root / (case['name'] + '.sampler.log')).write_bytes(sampled.stdout)
                    sampling = {'status': 'collected' if sampled.returncode == 0 and
                                output.is_file() else 'unavailable', 'argv': command,
                                'returncode': sampled.returncode, 'repetitions': repetitions}
                else:
                    sampling = {'status': 'unavailable', 'reason': 'no native sampling adapter',
                                'repetitions': repetitions}
                child.wait(timeout=timeout)
            except BaseException:
                child.kill()
                child.wait()
                raise
            outgoing.seek(0)
            errors.seek(0)
            check(bench, case, repetitions, child.returncode, outgoing.read(), errors.read())
    sampling.update(metadata)
    return sampling


def run_group(argv, payload, environment, timeout):
    # Native time is a wrapper around bc. A deadline must stop its process group,
    # not merely kill time and leave the interpreter running with inherited pipes.
    with subprocess.Popen(argv, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE, env=environment,
                          start_new_session=(os.name == 'posix')) as child:
        try:
            output, errors = child.communicate(payload, timeout=timeout)
        except BaseException:
            if os.name == 'posix':
                try:
                    os.killpg(child.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
            else:
                child.kill()
            child.communicate()
            raise
        return subprocess.CompletedProcess(argv, child.returncode, output, errors)


def cpu_time_fields(system, content):
    """Native printed CPU accounting, separate from paired wall-clock results."""
    number = r'([0-9]+(?:\.[0-9]+)?)'
    values = None
    if system == 'Darwin':
        match = re.search(r'^\s*' + number + r'\s+real\s+' + number +
                          r'\s+user\s+' + number + r'\s+sys\s*$', content, re.MULTILINE)
        if match:
            values = [float(match.group(2)), float(match.group(3))]
    elif system == 'Linux':
        fields = [re.search(r'^' + field + '=' + number + r'\s*$', content, re.MULTILINE)
                  for field in ('USER_SECONDS', 'SYSTEM_SECONDS')]
        if all(fields):
            values = [float(field.group(1)) for field in fields]
    if values is None or not all(math.isfinite(value) for value in values):
        return dict(status='unavailable', reason='no supported native CPU-time fields')
    return dict(status='collected', user_seconds=values[0], system_seconds=values[1],
                native_unit='seconds', precision='native printed precision; short processes may round to zero',
                scope='native child CPU accounting; not paired elapsed timing or hardware counters')


def rss_fields(system, content):
    if system == 'Darwin':
        pattern = r'^\s*(\d+)\s+maximum resident set size\s*$'
        unit = 'bytes (Darwin ru_maxrss)'
    elif system == 'Linux':
        pattern = r'^MAX_RSS_KIB=(\d+)\s*$'
        unit = 'KiB (GNU time %M)'
    else:
        return {'status': 'unavailable', 'reason': 'no native per-process memory adapter',
                'cpu_time': cpu_time_fields(system, content)}
    match = re.search(pattern, content, re.MULTILINE)
    if not match:
        return {'status': 'unavailable', 'reason': 'native memory report has no supported RSS field',
                'cpu_time': cpu_time_fields(system, content)}
    result = {'status': 'collected', 'peak_rss_native': int(match.group(1)), 'native_unit': unit,
              'cpu_time': cpu_time_fields(system, content)}
    if system == 'Darwin':
        counters = {}
        for label, key in (('instructions retired', 'instructions_retired'),
                           ('cycles elapsed', 'cycles_elapsed')):
            observed = re.search(r'^\s*(\d+)\s+' + label + r'\s*$', content, re.MULTILINE)
            if observed:
                counters[key] = int(observed.group(1))
        result['native_counters'] = counters
        footprint = re.search(r'^\s*(\d+)\s+peak memory footprint\s*$', content, re.MULTILINE)
        if footprint:
            result['peak_memory_footprint_bytes'] = int(footprint.group(1))
    return result


def rss_profile(bench, tool, case, root, timeout):
    report = root / (case['name'] + '.rss.txt')
    system = platform.system()
    if not Path('/usr/bin/time').is_file() or system not in ('Darwin', 'Linux'):
        return {'status': 'unavailable', 'reason': 'no native per-process memory adapter',
                'cpu_time': cpu_time_fields(system, ''), 'checked': False}
    if system == 'Darwin':
        command = ['/usr/bin/time', '-l', '-o', str(report)]
    else:
        command = ['/usr/bin/time', '-f', r'MAX_RSS_KIB=%M\nUSER_SECONDS=%U\nSYSTEM_SECONDS=%S', '-o', str(report), '--']
    environment = bench.environment()
    environment.pop('GD_BC_PROFILE_OUTPUT', None)
    with diagnostic_input(bench, tool, case, 1) as (argv, payload, metadata):
        command += argv
        child = run_group(command, payload, environment, timeout)
    check(bench, case, 1, child.returncode, child.stdout, child.stderr)
    result = rss_fields(system, report.read_text(encoding='utf-8'))
    result.update(metadata, argv=command, repetitions=1, checked=True)
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--config', type=Path, required=True)
    parser.add_argument('--implementation', required=True)
    parser.add_argument('--summary', type=Path, required=True, help='checked release summary for repeats')
    parser.add_argument('--output', type=Path, required=True, help='fresh diagnostics directory')
    parser.add_argument('--workload', action='append', required=True)
    parser.add_argument('--suite', choices=('arithmetic', 'math'), default='arithmetic')
    parser.add_argument('--replay-corpus', type=Path, help='exact frozen benchmark programs and oracles')
    parser.add_argument('--timeout', type=float, default=60)
    args = parser.parse_args()
    if not math.isfinite(args.timeout) or args.timeout <= 0:
        parser.error('--timeout must be finite and positive')
    root = args.output.resolve()
    try:
        root.mkdir(parents=True, exist_ok=False)
    except OSError as error:
        parser.error('cannot create fresh diagnostics directory: ' + str(error))
    bench = load_benchmark()
    manifest = {'schema': 1, 'purpose': 'diagnostic attribution; not timing results',
                'collector_sha256': bench.digest_file(__file__), 'platform': platform.platform(),
                'hardware_counters': {'status': 'platform-dependent; see memory.native_counters',
                                      'scope': 'native per-process counters where time exposes them; no PMU setup',
                                      'unavailable_metrics': ['branch misses', 'cache misses']},
                'suite': args.suite, 'corpus_seed': None if args.replay_corpus else 197200,
                'repeat_estimate_summary_sha256': bench.digest_file(args.summary),
                'workloads': [], 'status': 'incomplete'}
    try:
        tools = bench.implementations(args.config, require_provenance=True)
        tool = next((item for item in tools if item['name'] == args.implementation), None)
        if tool is None or tool['role'] == 'installed':
            raise bench.BenchmarkError('select a source-built release implementation')
        for index, artifact in enumerate(tool.get('input_artifacts', [])):
            directory = root / 'declared-inputs'
            directory.mkdir(exist_ok=True)
            archived = directory / '{}-{}'.format(index, Path(artifact['realpath']).name)
            archived.write_bytes(Path(artifact['realpath']).read_bytes())
            if bench.digest_file(archived) != artifact['sha256']:
                raise bench.BenchmarkError('declared input changed while archiving')
            artifact['archived_as'] = str(archived.relative_to(root))
        manifest['implementation'] = tool
        summary = json.loads(args.summary.read_text(encoding='utf-8'))
        observations = {row['name']: row for row in summary['workloads']}
        if args.replay_corpus:
            cases, manifest['replay'] = bench.replay_corpus(args.replay_corpus, args.suite, args.workload)
        else:
            cases = bench.corpus('full', args.workload, seed=197200, suite=args.suite)
        for case in cases:
            median = observations[case['name']]['implementations'][tool['name']]['median_seconds']
            repetitions = max(1, min(10000, int(math.ceil(5 / max(median, 0.000001)))))
            # Bound repeated-source memory independently of the child deadline.
            repetitions = min(repetitions, max(1, 64 * 1024 * 1024 //
                                               len(case['program'].encode('utf-8'))))
            (root / (case['name'] + '.bc')).write_bytes(case['program'].encode('utf-8'))
            oracle = bench.oracle_payload(case)
            (root / (case['name'] + '.expected')).write_bytes(oracle)
            row = {'oracle_sha256': bench.hashlib.sha256(oracle).hexdigest(),
                   'input_mode': case.get('input_mode', 'stdin'), 'name': case['name'], 'parameters': case['parameters'],
                   'program_sha256': bench.hashlib.sha256(case['program'].encode('utf-8')).hexdigest(),
                   'cpu': cpu_profile(bench, tool, case, repetitions, root, args.timeout),
                   'memory': rss_profile(bench, tool, case, root, args.timeout), 'checked': True}
            manifest['workloads'].append(row)
            bench.write_json(root / 'manifest.json', manifest)
            print(case['name'] + ': checked CPU/RSS diagnostic', flush=True)
        manifest['status'] = 'all diagnostic children checked; availability is per measurement'
        return 0
    except (bench.BenchmarkError, OSError, ValueError, KeyError, subprocess.TimeoutExpired) as error:
        manifest['failure'] = str(error)
        print('Diagnostics failed: ' + str(error), file=sys.stderr)
        return 1
    finally:
        bench.write_json(root / 'manifest.json', manifest)


if __name__ == '__main__':
    sys.exit(main())
