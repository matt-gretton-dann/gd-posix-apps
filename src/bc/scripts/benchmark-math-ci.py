#!/usr/bin/env python3
"""Build native library roles and run calibrated, checked math comparisons.

All builds finish before timing. A/A uses a second build of the same GD source;
the standard and portable Howard libraries remain separately identified roles.
An exact GD base is rebuilt separately. Software counters and native CPU/RSS
wrappers are separate diagnostics, never elapsed-performance samples. Matched
base/candidate resource collection is explicitly opt-in.
"""
import argparse
import copy
import importlib.util
import json
import os
from pathlib import Path
import platform
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


AA_POLICY = load('benchmark-policy')


def acceptance_report(rows, samples):
    return AA_POLICY.acceptance_review(rows, platform.system(), 'math', samples)


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

    def run(self, argv, timeout=None):
        remaining = self.deadline - time.monotonic()
        if remaining <= 0:
            raise CommandFailure('native math experiment exhausted its total deadline')
        # Sampling stages use the declared experiment budget; preparation
        # callers can still request a shorter explicit command deadline.
        timeout = remaining if timeout is None else min(timeout, remaining)
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
    # Cover the default-scale shortcuts and high-precision logarithm.
    extra = {('a', 20), ('l', 20), ('l', 1000)}
    names.update(case['name'] for case in cases
                 if case['parameters']['mode'] == 'repeated' and
                 (case['parameters']['function'], case['parameters']['scale']) in extra)
    return [case['name'] for case in cases if case['name'] in names]


def embedded_library_metadata(entry, revision, source_digest):
    image = entry['metadata'].get('configuration', {}).get('preparsed_library_image_sha256')
    result = dict(kind='preparsed image' if image else 'embedded ordinary source',
                  source_revision=revision, source_sha256=source_digest)
    if image:
        result['image_sha256'] = image
    return result


def configurations(gd, aa, howard, library, aa_library, revision,
                   base=None, base_library=None, base_revision=None):
    helper = load('build-bc-comparators')
    digest = helper.digest(library)
    if helper.digest(aa_library) != digest:
        raise CommandFailure('independent GD builds do not contain identical portable source')
    if any(entry['metadata']['source_revision'] != revision for entry in (gd, aa)):
        raise CommandFailure('GD recipe source does not match the requested candidate')
    if howard['metadata']['source_revision'] != helper.HOWARD_REVISION:
        raise CommandFailure('Howard comparison is not the pinned source revision')
    images = [entry['metadata'].get('configuration', {}).get('preparsed_library_image_sha256')
              for entry in (gd, aa)]
    if images[0] != images[1]:
        raise CommandFailure('independent GD builds do not contain identical preparsed images')
    gd, aa, standard, portable = map(copy.deepcopy, (gd, aa, howard, howard))
    for entry, name in ((gd, 'gd-portable'), (aa, 'gd-aa')):
        entry.update(name=name, argv=entry['argv'] + ['-l'])
        entry['metadata']['library'] = embedded_library_metadata(entry, revision, digest)
    standard.update(name='howard-standard', role='reference', argv=standard['argv'] + ['-l'])
    standard['metadata']['library'] = dict(kind='pinned standard library',
                                           source_revision=helper.HOWARD_REVISION)
    portable.update(name='howard-portable', role='candidate',
                    argv=portable['argv'] + [str(library)])
    portable['metadata']['library'] = dict(kind='ordinary source file',
                                           source_revision=revision, source_sha256=digest)
    portable['input_artifacts'] = [dict(path=str(library), sha256=digest, purpose='ordinary math library')]
    result = dict(preparation=[gd, aa, standard, portable], paired=[standard, gd, portable], aa=[gd, aa])
    if base is not None:
        if base['metadata']['source_revision'] != base_revision:
            raise CommandFailure('GD base recipe does not match the requested baseline')
        baseline = copy.deepcopy(base)
        baseline.update(name='gd-base', role='candidate', argv=baseline['argv'] + ['-l'])
        baseline['metadata']['library'] = embedded_library_metadata(
            baseline, base_revision, helper.digest(base_library))
        result['preparation'].append(baseline)
        result['paired'].append(baseline)
        result['regression'] = [baseline, gd]
    return result


def aa_status(first, second=None):
    return AA_POLICY.aa_status(first, second)


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


def diagnostic_cases(bench, preset):
    """One cold invocation near scale100 for every regime, independent of timing."""
    chosen = {}
    for case in bench.corpus(preset, suite='math'):
        if case['parameters']['mode'] != 'one_shot':
            continue
        old = chosen.get(case['group'])
        if old is None or abs(case['parameters']['scale'] - 100) < abs(old['parameters']['scale'] - 100):
            chosen[case['group']] = case
    return list(chosen.values())


def regression_status(first, second, control, mode, duration):
    if first is None:
        return 'unavailable baseline library'
    if mode == 'one_shot':
        return 'one-shot latency observation'
    if control == 'confirmed A/A bias':
        return 'inconclusive control bias'
    if not duration or not all(duration.values()):
        return 'inconclusive duration shortfall'
    result = load('benchmark-ci').decision(first, second)
    return result if control == 'pass' or result.startswith('inconclusive') else result + '; A/A inconclusive'


def allocation_diagnostics(bench, profile, cases, directory, timeout=60):
    directory.mkdir()
    tool = copy.deepcopy(profile)
    if tool['metadata']['instrumented'] is not True:
        raise CommandFailure('allocation diagnostics require a declared instrumented binary')
    tool['argv'].append('-l')
    records = []
    previous = os.environ.get('GD_BC_PROFILE_OUTPUT')
    try:
        for case in cases:
            target = directory / (case['name'] + '.json')
            os.environ['GD_BC_PROFILE_OUTPUT'] = str(target)
            observed = bench.run_checked(tool, case, timeout)
            observed.pop('seconds')  # Instrumented elapsed time is never evidence.
            counters = json.loads(target.read_text(encoding='utf-8'))
            if counters.get('instrumented') is not True or counters['total']['live_bytes'] != 0:
                raise CommandFailure('invalid diagnostic report or unreleased tracked storage')
            (directory / (case['name'] + '.bc')).write_bytes(case['program'].encode('utf-8'))
            (directory / (case['name'] + '.expected')).write_bytes(bench.oracle_payload(case))
            records.append(dict(name=case['name'], parameters=case['parameters'],
                program_sha256=bench.hashlib.sha256(case['program'].encode('utf-8')).hexdigest(),
                oracle_sha256=bench.hashlib.sha256(bench.oracle_payload(case)).hexdigest(),
                counters=target.name, **observed))
    finally:
        if previous is None:
            os.environ.pop('GD_BC_PROFILE_OUTPUT', None)
        else:
            os.environ['GD_BC_PROFILE_OUTPUT'] = previous
    write(directory / 'manifest.json', dict(implementation=tool, instrumented=True,
        scope='candidate-only software allocation/event counts; not RSS or release elapsed time',
        observations=records, child_timeout_seconds=timeout))


def resource_diagnostics(bench, configs, cases, directory, timeout=60):
    directory.mkdir()
    profile = load('profile-bc')
    tools = bench.implementations(configs, require_provenance=True)
    records = []
    for case in cases:
        (directory / (case['name'] + '.bc')).write_bytes(case['program'].encode('utf-8'))
        (directory / (case['name'] + '.expected')).write_bytes(bench.oracle_payload(case))
        for tool in tools:
            target = directory / (case['name'] + '-' + tool['name'])
            target.mkdir()
            result = profile.rss_profile(bench, tool, case, target, timeout)
            records.append(dict(name=case['name'], parameters=case['parameters'],
                implementation=tool['name'], resource=result, checked=result.get('checked', False),
                program_sha256=bench.hashlib.sha256(case['program'].encode('utf-8')).hexdigest(),
                oracle_sha256=bench.hashlib.sha256(bench.oracle_payload(case)).hexdigest(),
                correctness='checked by adapter when invoked; unavailable adapters run no child'))
    write(directory / 'manifest.json', dict(
        scope='one cold invocation per regime/role; native CPU/RSS availability per observation; no paired elapsed inference',
        observations=records, implementations=tools, child_timeout_seconds=timeout))


def summary_markdown(decisions):
    lines = ['# Native ordinary-library comparisons', '',
             'Ratios below one favor GD. Each row retains its own regime, scale and mode.',
             'Intervals are the union of paired IID and circular-block intervals; small samples',
             'and nonstationary host noise remain limitations. This report is advisory.', '',
             '| Corpus | Workload | GD / standard [CI] | GD / portable [CI] | A/A | Observation | GD / base [CI] | Regression |',
             '|---|---|---|---|---|---|---|---|']
    def ratio(value):
        return 'unavailable' if value is None else '{:.4f} {}'.format(value['median_paired_ratio'], value.get('ci95'))
    for row in decisions:
        lines.append('| {} | `{}` | {} | {} | {} | {} | {} | {} |'.format(
            row['preset'], row['name'], ratio(row['candidate_over_standard']),
            ratio(row['candidate_over_portable']), row['aa_status'], row['status'],
            ratio(row.get('candidate_over_base')), row.get('regression_status', 'unavailable')))
    lines += ['', 'Whole-process samples include library loading, parsing and constant generation.',
              'Repeated cases share a frozen calibrated invocation count across every role.',
              'One-shot rows report latency without a substantive-duration speed target.',
              'No aggregate pass is inferred. Instruction, allocation and peak-memory diagnostics',
              'are separate diagnostics; a green workflow alone does not establish acceptance.', '']
    return '\n'.join(lines)


def gd_build_command(args, name, destination, cc, cxx):
    command = [sys.executable, str(SCRIPTS / 'build-gd-bc.py'), '--source',
               str(args.source.resolve()), '--revision', args.base if name == 'base' else args.candidate,
               '--split-point', '126', '--output', str(destination), '--name', name,
               '--c-compiler', cc, '--cxx-compiler', cxx]
    # Math A/A builds the same candidate source and must use the same mode.
    if name != 'base' and getattr(args, 'candidate_prepared_divisors', False):
        command += ['--prepared-divisors', 'ON']
    if name != 'base' and getattr(args, 'candidate_preparsed_library', False):
        command += ['--preparsed-library']
    if name != 'base' and getattr(args, 'candidate_prefix_results', False):
        command += ['--prefix-results', 'ON']
    return command



def profile_build_command(args, destination, cc, cxx, baseline=False):
    # Diagnostics use each role's own source and selected lifecycle policy.
    command = [sys.executable, str(SCRIPTS / 'build-gd-bc.py'), '--source',
               str(args.source.resolve()), '--revision', args.base if baseline else args.candidate,
               '--split-point', '126', '--output', str(destination), '--name',
               'gd-base-profile' if baseline else 'gd-profile',
               '--diagnostic-profile', '--c-compiler', cc, '--cxx-compiler', cxx]
    if not baseline and getattr(args, 'candidate_preparsed_library', False):
        command += ['--preparsed-library']
    if not baseline and getattr(args, 'candidate_prepared_divisors', False):
        command += ['--prepared-divisors', 'ON']
    if not baseline and getattr(args, 'candidate_prefix_results', False):
        command += ['--prefix-results', 'ON']
    return command

def main():
    gd_builder, bench = load('build-gd-bc'), load('bench-bc')
    helper = load('build-bc-comparators')
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', type=Path, required=True)
    parser.add_argument('--base', type=gd_builder.revision_value, required=True)
    parser.add_argument('--candidate', type=gd_builder.revision_value, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--broad', action='store_true')
    parser.add_argument('--candidate-prepared-divisors', action='store_true')
    parser.add_argument('--candidate-prefix-results', action='store_true')
    parser.add_argument('--compare-resources', action='store_true')
    parser.add_argument('--candidate-preparsed-library', action='store_true')
    parser.add_argument('--broad-preset', choices=('full', 'holdout'))
    parser.add_argument('--samples', type=bench.positive_integer, default=15)
    parser.add_argument('--duration-seconds', type=bench.positive_timeout, default=.1)
    parser.add_argument('--child-timeout-seconds', type=bench.positive_timeout, default=120)
    parser.add_argument('--total-seconds', type=bench.positive_timeout, default=4800)
    args = parser.parse_args()
    if args.samples < 3:
        parser.error('at least three paired samples are required')
    if args.broad_preset and not args.broad:
        parser.error('--broad-preset requires --broad')
    presets = ((args.broad_preset,) if args.broad_preset else
               ('full', 'holdout') if args.broad else ('full',))
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
                                                     candidate=args.candidate, base=args.base,
                                                     performance_acceptance=False))
            return 0
        entries = []
        cc, cxx = ('cl', 'cl') if os.name == 'nt' else ('cc', 'c++')
        for name in ('gd', 'aa', 'base'):
            destination = root / name
            commands.run(gd_build_command(args, name, destination, cc, cxx), timeout=1200)
            entry = json.loads((destination / 'implementations.json').read_text())['implementations'][0]
            if args.candidate_prepared_divisors and name != 'base':
                gd_builder.require_prepared_candidate(helper, entry)
            if args.candidate_prefix_results and name != 'base':
                gd_builder.require_prefix_candidate(helper, entry)
            entries.append(entry)
        destination = root / 'howard'
        commands.run([sys.executable, str(SCRIPTS / 'build-bc-comparators.py'), '--output',
                      str(destination), '--implementation', 'howard', '--cc', cc], timeout=1200)
        howard = json.loads((destination / 'implementations.json').read_text())['implementations'][0]
        if args.candidate_prepared_divisors:
            for entry in entries[:2]:
                commands.run([sys.executable, str(SCRIPTS.parent / 'test-divisor-reuse-cli.py'),
                              entry['argv'][0]], timeout=60)
        base_library = root / 'base/source/src/bc/math-library.bc'
        baseline = entries[2] if base_library.is_file() else None
        if baseline is None:
            write(evidence / 'baseline-unavailable.json', dict(base=args.base,
                reason='base revision predates the ordinary library; no candidate/base performance acceptance',
                performance_acceptance=False))
        configs = configurations(entries[0], entries[1], howard,
                                 root / 'gd/source/src/bc/math-library.bc',
                                 root / 'aa/source/src/bc/math-library.bc', args.candidate,
                                 base=baseline, base_library=base_library, base_revision=args.base)
        profile_root = root / 'profile'
        profile_command = profile_build_command(args, profile_root, cc, cxx)
        commands.run(profile_command, timeout=1200)
        profiled = json.loads((profile_root / 'implementations.json').read_text())['implementations'][0]
        if args.candidate_prefix_results:
            gd_builder.require_prefix_candidate(helper, profiled)
        if args.candidate_prepared_divisors:
            gd_builder.require_prepared_candidate(helper, profiled)
        if args.compare_resources:
            if baseline is None:
                raise CommandFailure('comparative resources require an available ordinary baseline library')
            base_profile_root = root / 'base-profile'
            commands.run(profile_build_command(args, base_profile_root, cc, cxx, baseline=True), timeout=1200)
            base_profile = json.loads((base_profile_root / 'implementations.json').read_text())['implementations'][0]
            load('compare-bc-resources').run_diagnostics(
                commands.run, dict(base=base_profile, candidate=profiled),
                dict(base=configs['regression'][0], candidate=configs['regression'][1]),
                presets, 'math', evidence, timeout=args.child_timeout_seconds)
        # All builds and correctness checks finish before elapsed measurements.
        commands.run([sys.executable, str(SCRIPTS.parent / 'test-math-library-cli.py'),
                      configs['paired'][1]['argv'][0], '--source',
                      str(root / 'gd/source/src/bc/math-library.bc'), '--other-bc',
                      howard['argv'][0]], timeout=600)
        libraries = evidence / 'source-libraries'
        libraries.mkdir()
        shutil.copyfile(root / 'gd/source/src/bc/math-library.bc', libraries / 'candidate.bc')
        if baseline:
            shutil.copyfile(base_library, libraries / 'base.bc')
        allocation_diagnostics(bench, profiled, diagnostic_cases(bench, presets[0]), evidence / 'allocation-diagnostics',
                               timeout=args.child_timeout_seconds)
        for name, entries in configs.items():
            write(evidence / (name + '.json'), dict(implementations=entries))
        decisions = []
        for preset in presets:
            prepared = evidence / (preset + '-prepared')
            command = [sys.executable, str(SCRIPTS / 'bench-bc.py'), '--suite', 'math',
                       '--config', str(evidence / 'preparation.json'), '--output', str(prepared),
                       '--preset', preset, '--calibrate-seconds', str(args.duration_seconds),
                       '--calibrate-only', '--timeout', str(args.child_timeout_seconds), '--require-provenance']
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
                           '--timeout', str(args.child_timeout_seconds), '--require-provenance']
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
            regression = {}
            regression_confirmation = {}
            raw = measurements(evidence / (preset + '-paired'))
            if baseline:
                for row in reports['paired']['summary']['workloads']:
                    def times(role):
                        return [sample['seconds'] for sample in sorted(raw, key=lambda item: item['round'])
                                if sample['phase'] == 'measurement' and sample['workload'] == row['name']
                                and sample['implementation'] == role]
                    regression[row['name']] = bench.paired_statistics(times('gd-portable'), times('gd-base'), 1972160)
                suspects = [row['name'] for row in reports['paired']['summary']['workloads']
                    if row['parameters']['mode'] != 'one_shot' and regression[row['name']]['ci95'][0] > 1.05]
                if suspects:
                    target = evidence / (preset + '-regression-confirmation')
                    confirm = [sys.executable, str(SCRIPTS / 'bench-bc.py'), '--suite', 'math',
                        '--config', str(evidence / 'regression.json'), '--output', str(target),
                        '--replay-corpus', str(prepared), '--reference', 'gd-base',
                        '--samples', str(args.samples), '--warmups', '3', '--seed', '1972161', '--timeout', str(args.child_timeout_seconds), '--require-provenance']
                    confirm += [part for name in suspects for part in ('--workload', name)]
                    commands.run(confirm)
                    measurements(target)
                    regression_confirmation = {row['name']: row['candidate_over_reference']['gd-portable']
                        for row in json.loads((target / 'summary.json').read_text())['workloads']}
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
                    duration_target_met=row.get('duration_target_met'),
                    candidate_over_base=regression.get(row['name']),
                    regression_confirmation=regression_confirmation.get(row['name']),
                    regression_status=regression_status(regression.get(row['name']),
                        regression_confirmation.get(row['name']), controls[row['name']],
                        row['parameters']['mode'], {role: (row.get('duration_target_met') or {}).get(role, False)
                            for role in ('gd-portable', 'gd-base')})))
        # Resource wrappers run after all elapsed stages, never concurrently.
        for preset in presets:
            resource_diagnostics(bench, evidence / 'paired.json', diagnostic_cases(bench, preset),
                                 evidence / (preset + '-resource-diagnostics'), timeout=args.child_timeout_seconds)
        write(evidence / 'acceptance.json', acceptance_report(decisions, args.samples))
        write(evidence / 'decisions.json', dict(advisory=True, results=decisions,
             aa_policy_sha256=AA_POLICY.source_sha256(),
             goal='median<=0.90 and nominal CI upper<1 for substantive repeated classes',
             pending='CPU/RSS availability and precision are per observation; allocation counts are candidate-only; no aggregate pass'))
        summary_path = evidence / 'summary.md'
        summary_path.write_text(summary_markdown(decisions), encoding='utf-8')
        github_summary = os.environ.get('GITHUB_STEP_SUMMARY')
        if github_summary:
            with open(github_summary, 'a', encoding='utf-8') as output:
                output.write(summary_path.read_text(encoding='utf-8'))
        return 0
    except (CommandFailure, helper.BuildError, OSError, ValueError, bench.BenchmarkError) as error:
        write(evidence / 'failure.json', dict(error=str(error),status='hard failure; no acceptance'))
        print(str(error), file=sys.stderr)
        return 1
    finally:
        write(evidence / 'orchestration.json',dict(candidate=args.candidate,base=args.base,baseline_library_available=bool(configs.get('regression')),commands=commands.commands,
              broad=args.broad,presets=list(presets),samples=args.samples,duration_target=args.duration_seconds,
              child_timeout_seconds=args.child_timeout_seconds,total_seconds=args.total_seconds,
              candidate_prepared_divisors=args.candidate_prepared_divisors,
              candidate_preparsed_library=args.candidate_preparsed_library,
              candidate_prefix_results=args.candidate_prefix_results,
              compare_resources=args.compare_resources))
        for path in root.glob('command-*.log'):
            shutil.copyfile(path, evidence / path.name)
        for name in ('gd', 'aa', 'base', 'profile', 'base-profile', 'howard'):
            source = root / name
            target = evidence / name
            target.mkdir(exist_ok=True)
            for path in source.glob('*'):
                if path.is_file():
                    shutil.copyfile(path, target / path.name)
            for policy_root in ('prepared-policy', 'prefix-policy'):
                if (source / policy_root).is_dir():
                    shutil.copytree(str(source / policy_root), str(target / policy_root))
            if (source / 'logs').is_dir():
                shutil.copytree(source / 'logs', target / 'logs')
        if args.compare_resources and configs.get('regression'):
            # All timed children have exited; inspect those exact ordinary
            # binaries without rebuilding or changing their flags.
            load('collect-bc-codegen').collect(configs['regression'], evidence / 'codegen')


if __name__ == '__main__':
    sys.exit(main())
