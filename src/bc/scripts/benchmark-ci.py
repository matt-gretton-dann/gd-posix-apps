#!/usr/bin/env python3
"""Build and compare native bc revisions on one host, with advisory decisions.

Python 3.6+; SPDX-License-Identifier: Apache-2.0
"""
import argparse
import importlib.util
import json
import os
from pathlib import Path
import platform
import shutil
import subprocess
import sys

SCRIPTS = Path(__file__).resolve().parent
# Deliberately accepted fixed-dispatch source anchor; promotion requires evidence.
ANCHOR = '2ddf749e8b313f620ad248d680e801c409bbd3f5'


def load(name):
    spec = importlib.util.spec_from_file_location(name.replace('-', '_'), str(SCRIPTS / (name + '.py')))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


AA_POLICY = load('benchmark-policy')


def acceptance_report(rows, samples):
    return AA_POLICY.acceptance_review(rows, platform.system(), 'arithmetic', samples)


def write(path, value):
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + '\n', encoding='utf-8')


def capture(argv):
    try:
        result = subprocess.run(argv, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                universal_newlines=True, timeout=30)
        return {'argv': argv, 'status': result.returncode,
                'stdout': result.stdout, 'stderr': result.stderr}
    except (OSError, subprocess.TimeoutExpired) as error:
        return {'argv': argv, 'unavailable': str(error)}


def host_info():
    commands = {'Linux': [['lscpu', '-J'], ['ldd', '--version']],
                'Darwin': [['sysctl', 'machdep.cpu', 'hw.model', 'hw.memsize'], ['sw_vers']],
                'Windows': [['powershell', '-NoProfile', '-Command',
                             'Get-CimInstance Win32_Processor | Select-Object Name,Architecture,NumberOfCores,NumberOfLogicalProcessors | ConvertTo-Json'],
                            ['cmd', '/c', 'ver'],
                            ['powershell', '-NoProfile', '-Command',
                             '(Get-Item ($env:WINDIR + "/System32/ucrtbase.dll")).VersionInfo | Select-Object FileVersion,ProductVersion | ConvertTo-Json']]}
    allowed = ('ImageOS', 'ImageVersion', 'RUNNER_OS', 'RUNNER_ARCH', 'GITHUB_RUN_ID',
               'GITHUB_RUN_ATTEMPT', 'GITHUB_EVENT_NAME')
    return {'platform': platform.platform(), 'machine': platform.machine(),
            'python': sys.version, 'runner': {key: os.environ.get(key) for key in allowed},
            'details': [capture(argv) for argv in commands.get(platform.system(), [])],
            'affinity': 'OS default; no CPU isolation or affinity pinning',
            'ISA': 'portable C++ only; no runtime ISA-specific kernel selected',
            'diagnostics': 'allocation counters are excluded from timing; CPU/RSS diagnostics are separate',
            'limitations': 'Hosted VM contention, frequency and temperature are uncontrolled'}


def decision(first, second=None, startup=False):
    """Advisory only: one bounded confirmation, never rerun until favorable."""
    bounds = first.get('ci95')
    if startup or not bounds:
        return 'inconclusive-noisy'
    if bounds[0] > 1.05:
        other = second.get('ci95') if second else None
        return 'confirmed regression' if other and other[0] > 1.05 else 'inconclusive-noisy'
    if bounds[1] <= 1.05:
        return 'pass'
    return 'inconclusive-noisy'


def aa_decision(first, second=None, startup=False):
    # Independent builds have no expected winning direction. Do not reuse the
    # candidate/base rule, which intentionally permits an arbitrarily fast head.
    if startup:
        return 'inconclusive-noisy'
    status = AA_POLICY.aa_status(first, second)
    return 'inconclusive-noisy' if status == 'inconclusive' else status


def needs_confirmation(ratio, comparison, startup=False):
    if startup:
        return False
    interval = AA_POLICY.bounds(ratio)
    if interval is None:
        return False
    return AA_POLICY.aa_direction(ratio) != 0 if comparison == 'aa' else interval[0] > 1.05


def selected_cases(bench, preset, broad):
    cases = bench.corpus(preset, listing=True, seed=197201 if preset == 'holdout' else 197200)
    if broad:
        return [case['name'] for case in cases]
    # Derive representatives from the corpus so newly added classes cannot
    # silently escape PR checks. Keep real full-size dimensions for timing.
    chosen = []
    groups = set()
    for case in cases:
        if case['group'] not in groups:
            chosen.append(case['name'])
            groups.add(case['group'])
    # Preserve the existing small-multiply winner and representative large,
    # sparse and imbalanced shapes, whose algorithmic behavior differs. Retain
    # large-scale and both tiny-fraction sqrt regimes rather than measuring only
    # the first, small sqrt representative. Keep the high-scale division
    # grid so native jobs exercise the large-quotient kernel.
    for case in cases:
        parameters = case['parameters']
        if (case['group'] == 'comparison' or case['group'] == 'sqrt_tiny' or
                (case['group'] == 'sqrt' and parameters.get('scale', 0) >= 5000) or
                (case['group'] == 'divide_multilimb' and parameters.get('scale', 0) >= 1000) or
                (case['group'] == 'square' and parameters.get('digits', 0) >= 10000) or
                (case['group'] == 'multiply' and parameters.get('left_digits', 0) >= 10000)):
            if case['name'] not in chosen:
                chosen.append(case['name'])
    return chosen


def sparse_duration_sufficient(row):
    """A millisecond process sample cannot qualify this supplemental cohort."""
    return all(row['implementations'][role]['median_seconds'] >= .25
               for role in ('base', 'aa', 'candidate'))


def qualify_sparse(builder, evidence, entries):
    """Fixed latency and throughput cohorts; never retry until a result passes.

    Retain full-size operands and their exact product oracle. Increase only the
    loop count for throughput, preserving the original latency shape separately.
    Every original broad report remains independent. All three
    GD roles share each randomized round, including the independently built A/A.
    """
    config = evidence / 'sparse-qualification.json'
    write(config, {'implementations': entries})
    reports = []
    for preset, digits, original in (('full', 10000, 1000), ('holdout', 11250, 875)):
        for kind, multiplier, samples in (('latency', 1, 101), ('long', 1000, 31)):
            count = original * multiplier
            name = 'multiply_sparse_{}d_x{}'.format(digits, count)
            label = preset + '-sparse-' + kind
            target = evidence / label
            command = [sys.executable, str(SCRIPTS / 'bench-bc.py'), '--config', str(config),
                       '--output', str(target), '--reference', 'base', '--preset', preset,
                       '--workload', name, '--samples', str(samples), '--warmups', '3',
                       '--seed', '197588', '--corpus-seed',
                       '197201' if preset == 'holdout' else '197200', '--require-provenance']
            if kind == 'long':
                command += ['--sparse-iterations', str(count)]
            builder.timeout = 1800
            builder.run(command)
            row, = json.loads((target / 'summary.json').read_text())['workloads']
            sufficient = kind == 'latency' or sparse_duration_sufficient(row)
            control = aa_decision(row['candidate_over_reference']['aa'])
            if not sufficient:
                control = 'inconclusive-duration-shortfall'
            for comparison, role in (('aa', 'aa'), ('paired', 'candidate')):
                ratio = row['candidate_over_reference'][role]
                status = control if comparison == 'aa' else decision(ratio)
                if not sufficient:
                    status = 'inconclusive-duration-shortfall'
                reports.append(dict(preset=label, comparison=comparison, name=name,
                                    ratio=ratio, confirmation=None, status=status,
                                    aa_control_status=control, candidate_over_howard=None,
                                    minimum_role_median_seconds=.25 if kind == 'long' else None,
                                    fixed_samples=samples))
    return reports


def gd_build_command(args, name, revision, directory, cc, cxx):
    command = [sys.executable, str(SCRIPTS / 'build-gd-bc.py'), '--source',
               str(args.source.resolve()), '--revision', revision, '--split-point',
               str(args.split_point), '--output', str(directory), '--name', name,
               '--c-compiler', cc, '--cxx-compiler', cxx]
    if name == 'candidate' and getattr(args, 'candidate_prepared_divisors', False):
        command += ['--prepared-divisors', 'ON']
    return command


def profile_build_command(args, name, revision, directory, cc, cxx):
    command = gd_build_command(args, name, revision, directory, cc, cxx)
    command[command.index('--name') + 1] = name + '-profile'
    return command + ['--diagnostic-profile']


def main():
    helper = load('build-bc-comparators')
    gd = load('build-gd-bc')
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', type=Path, required=True)
    parser.add_argument('--base', type=gd.revision_value, required=True)
    parser.add_argument('--candidate', type=gd.revision_value, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--broad', action='store_true')
    parser.add_argument('--sparse-qualification', action='store_true')
    parser.add_argument('--compare-resources', action='store_true')
    parser.add_argument('--candidate-prepared-divisors', action='store_true')
    parser.add_argument('--require-candidate-native', action='store_true')
    parser.add_argument('--samples', type=int, default=9)
    parser.add_argument('--split-point', type=gd.positive_point, default=126)
    args = parser.parse_args()
    if args.samples < 3:
        parser.error('at least three paired samples are required')
    root = args.output.resolve()
    root.mkdir(parents=True, exist_ok=False)
    evidence = root / 'evidence'
    evidence.mkdir()
    write(evidence / 'host.json', host_info())
    builder = helper.Builder(root, 900)
    cc, cxx = ('cl', 'cl') if os.name == 'nt' else ('cc', 'c++')
    entries = []
    profiles = {}
    howard = []
    try:
        # Independent A/A build measures source/build noise; no build runs
        # concurrently with any measured executable. No tuned-binary cache.
        for name, revision in (('base', args.base), ('candidate', args.candidate), ('aa', args.base)):
            directory = root / name
            builder.run(gd_build_command(args, name, revision, directory, cc, cxx))
            built = json.loads((directory / 'implementations.json').read_text())['implementations']
            if name == 'candidate' and args.candidate_prepared_divisors:
                for entry in built:
                    gd.require_prepared_candidate(helper, entry)
            if name == 'candidate' and args.require_candidate_native:
                for entry in built:
                    gd.require_native_candidate(helper, entry)
            entries.extend(built)
        comparator = root / 'howard'
        builder.run([sys.executable, str(SCRIPTS / 'build-bc-comparators.py'), '--output',
                     str(comparator), '--cc', cc])
        howard = json.loads((comparator / 'implementations.json').read_text())['implementations']
        if args.candidate_prepared_divisors:
            # All owned builds are finished; validate ordinary language guards
            # before starting any elapsed sample, including independent controls.
            for entry in entries:
                builder.run([sys.executable, str(SCRIPTS.parent / 'test-divisor-reuse-cli.py'),
                             entry['argv'][0]])
        if args.require_candidate_native:
            for entry in entries:
                for script in ('test-cli.py', 'test-vm-cli.py'):
                    builder.run([sys.executable, str(SCRIPTS.parent / script),
                                 entry['argv'][0], '--extensions', 'ON'])
        bench = load('bench-bc')
        base_digest = entries[0]['metadata'].get('binary_sha256')
        identical_binaries = bool(base_digest) and base_digest == entries[1]['metadata'].get('binary_sha256')
        configs = {'paired': [entries[0], entries[1]] + howard,
                   'aa': [entries[0], entries[2]]}
        for name, implementations in configs.items():
            write(root / (name + '.json'), {'implementations': implementations})
        if args.compare_resources:
            for name, revision in (('base', args.base), ('candidate', args.candidate)):
                directory = root / (name + '-profile')
                builder.run(profile_build_command(args, name, revision, directory, cc, cxx))
                profile = json.loads((directory / 'implementations.json').read_text())['implementations'][0]
                if name == 'candidate' and args.candidate_prepared_divisors:
                    gd.require_prepared_candidate(helper, profile)
                if name == 'candidate' and args.require_candidate_native:
                    gd.require_native_candidate(helper, profile)
                profiles[name] = profile
            load('compare-bc-resources').validate_roles(
                bench, profiles, dict(base=entries[0], candidate=entries[1]))
        reports = []
        for preset in (('full', 'holdout') if args.broad else ('full',)):
            names = selected_cases(bench, preset, args.broad)
            controls = {}
            for comparison in ('aa', 'paired'):
                target = evidence / (preset + '-' + comparison)
                command = [sys.executable, str(SCRIPTS / 'bench-bc.py'), '--config',
                           str(root / (comparison + '.json')), '--output', str(target),
                           '--reference', 'base', '--preset', preset, '--samples', str(args.samples),
                           '--warmups', '1', '--seed', '197216', '--corpus-seed',
                           '197201' if preset == 'holdout' else '197200', '--require-provenance']
                for name in names:
                    command += ['--workload', name]
                builder.timeout = 3600
                builder.run(command)
                summary = json.loads((target / 'summary.json').read_text())
                candidate = 'aa' if comparison == 'aa' else 'candidate'
                suspects = [row['name'] for row in summary['workloads']
                            if needs_confirmation(row['candidate_over_reference'][candidate], comparison,
                                                  startup=row['name'] == 'startup')]
                confirmation = {}
                if suspects:
                    confirm_target = evidence / (preset + '-' + comparison + '-confirmation')
                    confirm = command[:command.index('--workload')]
                    confirm[confirm.index('--output') + 1] = str(confirm_target)
                    confirm[confirm.index('--seed') + 1] = '197217'
                    for name in suspects:
                        confirm += ['--workload', name]
                    builder.run(confirm)
                    confirmation = {row['name']: row for row in json.loads(
                        (confirm_target / 'summary.json').read_text())['workloads']}
                raw = [json.loads(line) for line in (target / 'samples.jsonl').read_text().splitlines()]
                for row in summary['workloads']:
                    competitive = None
                    if comparison == 'paired':
                        def samples(name):
                            return [sample['seconds'] for sample in sorted(raw, key=lambda sample: sample['round'])
                                    if sample['phase'] == 'measurement' and sample['workload'] == row['name']
                                    and sample['implementation'] == name]
                        competitive = bench.paired_statistics(samples('candidate'), samples(howard[0]['name']),
                                                              seed=197216)
                    ratio = row['candidate_over_reference'][candidate]
                    second = confirmation.get(row['name'], {}).get('candidate_over_reference', {}).get(candidate)
                    status = (aa_decision(ratio, second, startup=row['name'] == 'startup')
                              if comparison == 'aa' else
                              decision(ratio, second, startup=row['name'] == 'startup'))
                    if comparison == 'aa':
                        controls[row['name']] = status
                    elif identical_binaries and status == 'confirmed regression':
                        # Identical bytes can still time differently under separate launch paths.
                        status = 'confirmed timing bias (identical binaries)'
                    reports.append({'preset': preset, 'comparison': comparison, 'name': row['name'],
                                    'ratio': ratio, 'confirmation': second,
                                    'status': status, 'aa_control_status': controls[row['name']],
                                    'candidate_over_howard': competitive})
        if args.sparse_qualification:
            reports.extend(qualify_sparse(builder, evidence, entries))
        # All owned builds precede sampling; wrappers follow all elapsed stages.
        if args.compare_resources:
            load('compare-bc-resources').run_diagnostics(
                builder.run, profiles, dict(base=entries[0], candidate=entries[1]),
                ('full', 'holdout') if args.broad else ('full',), 'arithmetic', evidence)
        write(evidence / 'acceptance.json', acceptance_report(reports, args.samples))
        write(evidence / 'decisions.json', {'advisory': True, 'results': reports,
              'aa_policy_sha256': AA_POLICY.source_sha256(),
              'policy': 'Raw signals retained; see acceptance.json for competitive-margin policy; startup uncalibrated',
              'pending': ['POSIX math library #217', 'cross-run A/A calibration and autocorrelation policy']})
        lines = ['# bc native paired results', '', 'Performance decisions are advisory; correctness, builds and deadlines are hard failures.',
                 '', 'Candidate is the exact PR head (not a synthetic merge), or checked push/main SHA.',
                 'A/A independently rebuilds the same base; its entire CI must lie in [0.95,1.05] to pass. Startup remains inconclusive until calibrated.',
                 'Paired status retains the raw A/B signal; the A/A qualifier limits attribution and competitive claims.', '',
                 'When base and candidate hashes match, a confirmed slowdown is labeled timing bias. '
                 'The original ratios and confirmation remain in the artifact.', '',
                 '| Preset | Comparison | Workload | Ratio [95%] | Advisory status | A/A qualifier |',
                 '| --- | --- | --- | --- | --- | --- |']
        for row in reports:
            ratio = row['ratio']
            lines.append('| {} | {} | `{}` | {:.3f} {} | {} | {} |'.format(
                row['preset'], row['comparison'], row['name'], ratio['median_paired_ratio'],
                ratio.get('ci95'), row['status'], row['aa_control_status'] if row['comparison'] == 'paired' else '-'))
        (evidence / 'summary.md').write_text('\n'.join(lines) + '\n', encoding='utf-8')
        if os.environ.get('GITHUB_STEP_SUMMARY'):
            with open(os.environ['GITHUB_STEP_SUMMARY'], 'a', encoding='utf-8') as stream:
                stream.write('\n'.join(lines) + '\n')
        return 0
    except (helper.BuildError, OSError, ValueError) as error:
        write(evidence / 'failure.json', {'error': str(error), 'status': 'inconclusive; hard failure'})
        print(str(error), file=sys.stderr)
        return 1
    finally:
        # Preserve exact recipes/logs without uploading cloned source/build trees.
        write(evidence / 'orchestration.json', {'base': args.base, 'candidate': args.candidate,
              'split_point': args.split_point, 'candidate_prepared_divisors': args.candidate_prepared_divisors,
              'require_candidate_native': args.require_candidate_native,
              'sparse_qualification': args.sparse_qualification, 'compare_resources': args.compare_resources,
              'commands': builder.commands})
        for path in root.glob('command-*.log'):
            shutil.copyfile(str(path), str(evidence / path.name))
        for name in ('base', 'candidate', 'aa', 'howard', 'base-profile', 'candidate-profile'):
            source = root / name
            target = evidence / name
            target.mkdir(exist_ok=True)
            for path in source.glob('*'):
                if path.is_file():
                    shutil.copyfile(str(path), str(target / path.name))
            if (source / 'prepared-policy').is_dir():
                shutil.copytree(str(source / 'prepared-policy'), str(target / 'prepared-policy'))
            if (source / 'native-policy').is_dir():
                shutil.copytree(str(source / 'native-policy'), str(target / 'native-policy'))
            if (source / 'logs').is_dir():
                shutil.copytree(str(source / 'logs'), str(target / 'logs'))
        # Every timed child has finished. Preserve every measured GD and Howard
        # image without rebuilding, relinking or changing compiler flags.
        load('collect-bc-codegen').collect(entries + howard, evidence / 'codegen')
        if profiles:
            load('compare-bc-resources').preserve_profiles(profiles, evidence / 'profile-images')


if __name__ == '__main__':
    sys.exit(main())
