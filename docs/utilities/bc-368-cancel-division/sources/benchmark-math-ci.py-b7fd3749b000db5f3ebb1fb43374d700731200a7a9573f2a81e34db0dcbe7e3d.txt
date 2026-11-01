#!/usr/bin/env python3
"""Build native library roles and run calibrated, checked math comparisons.

All builds finish before timing. A/A uses a second build of the same GD source;
the standard and portable Howard libraries remain separately identified roles.
"""
import argparse
import copy
import importlib.util
import json
import os
from pathlib import Path
import shutil
import signal
import subprocess
import sys
import time

SCRIPTS = Path(__file__).resolve().parent


def load(name):
    spec = importlib.util.spec_from_file_location(name.replace('-', '_'), str(SCRIPTS / (name + '.py')))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def write(path, value):
    path.write_bytes((json.dumps(value, indent=2, sort_keys=True) + '\n').encode('utf-8'))


class CommandFailure(RuntimeError):
    pass


def stop_tree(child):
    if os.name == 'posix':
        try:
            os.killpg(child.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
    elif os.name == 'nt':
        # The harness can itself own a timed interpreter. Stop the complete
        # native tree before reaping its parent, including inherited pipes.
        try:
            subprocess.run(['taskkill', '/PID', str(child.pid), '/T', '/F'],
                           stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=10)
        except (OSError, subprocess.TimeoutExpired):
            pass
        if child.poll() is None:
            child.kill()
    elif child.poll() is None:
        child.kill()  # Generic direct-child fallback on other Python platforms.


class Commands:
    def __init__(self, root, total_seconds):
        self.root, self.deadline = root, time.monotonic() + total_seconds
        self.commands = []

    def run(self, argv, timeout=3600):
        remaining = self.deadline - time.monotonic()
        if remaining <= 0:
            raise CommandFailure('native math experiment exhausted its total deadline')
        timeout = min(timeout, remaining)
        log = self.root / 'command-{:03d}.log'.format(len(self.commands) + 1)
        row = dict(argv=list(map(str, argv)), cwd=str(self.root), log=log.name,
                   timeout_seconds=timeout)
        self.commands.append(row)
        with subprocess.Popen(row['argv'], cwd=str(self.root), stdout=subprocess.PIPE,
                              stderr=subprocess.STDOUT, start_new_session=(os.name == 'posix')) as child:
            try:
                output, _ = child.communicate(timeout=timeout)
            except subprocess.TimeoutExpired:
                stop_tree(child)
                output, _ = child.communicate()
                log.write_bytes(output)
                row['status'] = 'timeout'
                raise CommandFailure('command exceeded its deadline: ' + log.name)
            except BaseException:
                stop_tree(child)
                child.communicate()
                raise
        log.write_bytes(output)
        row['returncode'] = child.returncode
        if child.returncode:
            raise CommandFailure('command failed; see ' + log.name)


def selected_cases(bench, preset, broad):
    cases = bench.corpus(preset, suite='math')
    if broad:
        return [case['name'] for case in cases]
    # Prespecify a representative near scale100 for every regime AND mode.
    # Newly added groups cannot silently disappear from PR observations.
    chosen = {}
    for case in cases:
        key = case['group'], case['parameters']['mode']
        previous = chosen.get(key)
        if previous is None or abs(case['parameters']['scale'] - 100) < abs(previous['parameters']['scale'] - 100):
            chosen[key] = case
    names = {case['name'] for case in chosen.values()}
    return [case['name'] for case in cases if case['name'] in names]


def configurations(gd, aa, howard, library, aa_library, revision):
    helper = load('build-bc-comparators')
    digest = helper.digest(library)
    if helper.digest(aa_library) != digest:
        raise CommandFailure('independent GD builds do not contain identical portable source')
    if any(entry['metadata']['source_revision'] != revision for entry in (gd, aa)):
        raise CommandFailure('GD recipe source does not match the requested candidate')
    if howard['metadata']['source_revision'] != helper.HOWARD_REVISION:
        raise CommandFailure('Howard comparison is not the pinned source revision')
    gd, aa, standard, portable = map(copy.deepcopy, (gd, aa, howard, howard))
    for entry, name in ((gd, 'gd-portable'), (aa, 'gd-aa')):
        entry.update(name=name, argv=entry['argv'] + ['-l'])
        entry['metadata']['library'] = dict(kind='embedded ordinary source',
                                            source_revision=revision, source_sha256=digest)
    standard.update(name='howard-standard', role='reference', argv=standard['argv'] + ['-l'])
    standard['metadata']['library'] = dict(kind='pinned standard library',
                                           source_revision=helper.HOWARD_REVISION)
    portable.update(name='howard-portable', role='candidate',
                    argv=portable['argv'] + [str(library)])
    portable['metadata']['library'] = dict(kind='ordinary source file',
                                           source_revision=revision, source_sha256=digest)
    portable['input_artifacts'] = [dict(path=str(library), sha256=digest, purpose='ordinary math library')]
    return dict(preparation=[gd, aa, standard, portable], paired=[standard, gd, portable], aa=[gd, aa])


def aa_status(first, second=None):
    bounds = first.get('ci95')
    if not bounds:
        return 'inconclusive'
    direction = 1 if bounds[0] > 1.05 else -1 if bounds[1] < .95 else 0
    if direction:
        other = second.get('ci95') if second else None
        same = other and (other[0] > 1.05 if direction == 1 else other[1] < .95)
        return 'confirmed A/A bias' if same else 'inconclusive'
    return 'pass' if bounds[0] >= .95 and bounds[1] <= 1.05 else 'inconclusive'


def goal_status(row, control):
    if row['parameters']['mode'] == 'one_shot':
        return 'one-shot latency observation'
    if not row.get('duration_target_met') or not all(row['duration_target_met'].values()):
        return 'insufficient measured duration'
    if control == 'confirmed A/A bias':
        return 'inconclusive control bias'
    ratio = row['candidate_over_reference']['gd-portable']
    if not ratio.get('ci95'):
        return 'insufficient pairs'
    if ratio['median_paired_ratio'] <= .90 and ratio['ci95'][1] < 1:
        return 'target observed' if control == 'pass' else 'target observed; A/A inconclusive'
    return 'target not established'


def measurements(directory):
    rows = [json.loads(line) for line in (directory / 'samples.jsonl').read_text().splitlines()]
    if not rows or not all(row['checked'] for row in rows):
        raise CommandFailure('missing or unchecked mathematical observations')
    return rows


def summary_markdown(decisions):
    lines = ['# Native ordinary-library comparisons', '',
             'Ratios below one favor GD. Each row retains its own regime, scale and mode.',
             'Intervals are the union of paired IID and circular-block intervals; small samples',
             'and nonstationary host noise remain limitations. This report is advisory.', '',
             '| Corpus | Workload | GD / standard [CI] | GD / portable [CI] | A/A | Observation |',
             '|---|---|---|---|---|---|']
    def ratio(value):
        return '{:.4f} {}'.format(value['median_paired_ratio'], value.get('ci95'))
    for row in decisions:
        lines.append('| {} | `{}` | {} | {} | {} | {} |'.format(
            row['preset'], row['name'], ratio(row['candidate_over_standard']),
            ratio(row['candidate_over_portable']), row['aa_status'], row['status']))
    lines += ['', 'Whole-process samples include library loading, parsing and constant generation.',
              'Repeated cases share a frozen calibrated invocation count across every role.',
              'One-shot rows report latency without a substantive-duration speed target.',
              'No aggregate pass is inferred. Instruction, allocation and peak-memory diagnostics',
              'remain separate work; a green workflow alone does not establish acceptance.', '']
    return '\n'.join(lines)


def main():
    gd_builder, bench = load('build-gd-bc'), load('bench-bc')
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', type=Path, required=True)
    parser.add_argument('--candidate', type=gd_builder.revision_value, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--broad', action='store_true')
    parser.add_argument('--samples', type=bench.positive_integer, default=15)
    parser.add_argument('--duration-seconds', type=bench.positive_timeout, default=.1)
    parser.add_argument('--total-seconds', type=bench.positive_timeout, default=4800)
    args = parser.parse_args()
    if args.samples < 3:
        parser.error('at least three paired samples are required')
    root = args.output.resolve()
    root.mkdir(parents=True, exist_ok=False)
    evidence = root / 'evidence'
    evidence.mkdir()
    write(evidence / 'host.json', load('benchmark-ci').host_info())
    commands = Commands(root, args.total_seconds)
    configs = {}
    try:
        if not (args.source / 'src/bc/math-library.bc').is_file():
            write(evidence / 'unavailable.json', dict(status='ordinary library source unavailable',
                                                     candidate=args.candidate,
                                                     performance_acceptance=False))
            return 0
        entries = []
        cc, cxx = ('cl', 'cl') if os.name == 'nt' else ('cc', 'c++')
        for name in ('gd', 'aa'):
            destination = root / name
            commands.run([sys.executable, str(SCRIPTS / 'build-gd-bc.py'), '--source',
                          str(args.source.resolve()), '--revision', args.candidate,
                          '--split-point', '126', '--output', str(destination), '--name', name,
                          '--c-compiler', cc, '--cxx-compiler', cxx], timeout=1200)
            entries.append(json.loads((destination / 'implementations.json').read_text())['implementations'][0])
        destination = root / 'howard'
        commands.run([sys.executable, str(SCRIPTS / 'build-bc-comparators.py'), '--output',
                      str(destination), '--implementation', 'howard', '--cc', cc], timeout=1200)
        howard = json.loads((destination / 'implementations.json').read_text())['implementations'][0]
        configs = configurations(*entries, howard,
                                 root / 'gd/source/src/bc/math-library.bc',
                                 root / 'aa/source/src/bc/math-library.bc', args.candidate)
        for name, entries in configs.items():
            write(evidence / (name + '.json'), dict(implementations=entries))
        decisions = []
        for preset in (('full', 'holdout') if args.broad else ('full',)):
            prepared = evidence / (preset + '-prepared')
            command = [sys.executable, str(SCRIPTS / 'bench-bc.py'), '--suite', 'math',
                       '--config', str(evidence / 'preparation.json'), '--output', str(prepared),
                       '--preset', preset, '--calibrate-seconds', str(args.duration_seconds),
                       '--calibrate-only', '--require-provenance']
            for name in selected_cases(bench, preset, args.broad):
                command += ['--workload', name]
            commands.run(command)
            reports = {}
            for comparison, reference in (('aa', 'gd-portable'), ('paired', 'howard-standard')):
                target = evidence / (preset + '-' + comparison)
                command = [sys.executable, str(SCRIPTS / 'bench-bc.py'), '--suite', 'math',
                           '--config', str(evidence / (comparison + '.json')), '--output', str(target),
                           '--replay-corpus', str(prepared), '--reference', reference,
                           '--samples', str(args.samples), '--warmups', '3', '--seed', '1972170',
                           '--require-provenance']
                commands.run(command)
                summary = json.loads((target / 'summary.json').read_text())
                measurements(target)
                confirmation = {}
                if comparison == 'aa':
                    suspects = [row['name'] for row in summary['workloads']
                                if (row['candidate_over_reference']['gd-aa']['ci95'][0] > 1.05 or
                                    row['candidate_over_reference']['gd-aa']['ci95'][1] < .95)]
                    if suspects:
                        confirm = command + [part for name in suspects for part in ('--workload', name)]
                        confirm[confirm.index('--output') + 1] = str(evidence / (preset + '-aa-confirmation'))
                        confirm[confirm.index('--seed') + 1] = '1972171'
                        commands.run(confirm)
                        measurements(evidence / (preset + '-aa-confirmation'))
                        confirmed = json.loads((evidence / (preset + '-aa-confirmation/summary.json')).read_text())
                        confirmation = {row['name']: row for row in confirmed['workloads']}
                reports[comparison] = dict(summary=summary, confirmation=confirmation)
            controls = {}
            for row in reports['aa']['summary']['workloads']:
                second = reports['aa']['confirmation'].get(row['name'], {})
                status = aa_status(row['candidate_over_reference']['gd-aa'],
                    second.get('candidate_over_reference', {}).get('gd-aa'))
                if row['parameters']['mode'] != 'one_shot' and (
                        not row.get('duration_target_met') or not all(row['duration_target_met'].values())):
                    status = 'inconclusive duration shortfall'
                controls[row['name']] = status
            raw = measurements(evidence / (preset + '-paired'))
            for row in reports['paired']['summary']['workloads']:
                def times(role):
                    return [sample['seconds'] for sample in sorted(raw, key=lambda item: item['round'])
                            if sample['phase'] == 'measurement' and sample['workload'] == row['name']
                            and sample['implementation'] == role]
                decisions.append(dict(preset=preset, name=row['name'], parameters=row['parameters'],
                    candidate_over_standard=row['candidate_over_reference']['gd-portable'],
                    portable_over_standard=row['candidate_over_reference']['howard-portable'],
                    candidate_over_portable=bench.paired_statistics(times('gd-portable'),times('howard-portable'),1972170),
                    aa_status=controls[row['name']], status=goal_status(row,controls[row['name']]),
                    duration_target_met=row.get('duration_target_met')))
        write(evidence / 'decisions.json', dict(advisory=True, results=decisions,
             goal='median<=0.90 and nominal CI upper<1 for substantive repeated classes',
             pending='separate instruction/allocation/peak-memory diagnostics; no aggregate pass'))
        summary_path = evidence / 'summary.md'
        summary_path.write_text(summary_markdown(decisions), encoding='utf-8')
        github_summary = os.environ.get('GITHUB_STEP_SUMMARY')
        if github_summary:
            with open(github_summary, 'a', encoding='utf-8') as output:
                output.write(summary_path.read_text(encoding='utf-8'))
        return 0
    except (CommandFailure, OSError, ValueError, bench.BenchmarkError) as error:
        write(evidence / 'failure.json', dict(error=str(error),status='hard failure; no acceptance'))
        print(str(error), file=sys.stderr)
        return 1
    finally:
        write(evidence / 'orchestration.json',dict(candidate=args.candidate,commands=commands.commands,
              broad=args.broad,samples=args.samples,duration_target=args.duration_seconds))
        for path in root.glob('command-*.log'):
            shutil.copyfile(path, evidence / path.name)
        for name in ('gd', 'aa', 'howard'):
            source = root / name
            target = evidence / name
            target.mkdir(exist_ok=True)
            for path in source.glob('*'):
                if path.is_file():
                    shutil.copyfile(path, target / path.name)
            if (source / 'logs').is_dir():
                shutil.copytree(source / 'logs', target / 'logs')


if __name__ == '__main__':
    sys.exit(main())
