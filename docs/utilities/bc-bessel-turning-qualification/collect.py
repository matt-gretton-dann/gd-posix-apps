#!/usr/bin/env python3
"""Run one fixed standalone Bessel qualification partition on a native host."""
# SPDX-License-Identifier: Apache-2.0
import argparse
import hashlib
import importlib.util
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
from zipfile import ZipFile

BASE = '19ef80161fd2bdb24bf9ba7e93942caa97ea6ea1'
CANDIDATE = '4fbcf17dbc687ef6679ee9db4b80c3dbcd0417ed'
HARNESS = '1f18996542cc3aac308fc1f37613ae02dc76f5f9'
PRESETS = ('full', 'holdout')
SCALES = (20, 100, 1000)
SUITES = ('arithmetic-full', 'arithmetic-holdout', 'startup') + tuple(
    f'math-{preset}-{scale}' for preset in PRESETS for scale in SCALES)


def write(path, data):
    path.write_text(json.dumps(data, indent=2, sort_keys=True) + '\n', encoding='utf-8')


def load(scripts, name):
    spec = importlib.util.spec_from_file_location(name, scripts / (name + '.py'))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def declared_math(scripts, preset, scale, target):
    inputs = load(scripts, 'math-qualification-inputs')
    bench = inputs.bench_module()
    names = {case['name'] for case in bench.math_corpus(preset)
             if case['parameters']['scale'] == scale}
    rows = [row for row in inputs.exposed(preset) if row['name'] in names]
    if len(names) != 20 or len(rows) != 20 or {row['name'] for row in rows} != names:
        raise ValueError('the fixed mathematical partition inventory changed')
    inputs.write(rows, 'exposed', preset, str(scale), 0, 1, target)


def command_plan(suite, source, output, temporary, scripts):
    common = ['--source', str(source), '--base', BASE, '--candidate', CANDIDATE]
    if suite.startswith('arithmetic-'):
        _, preset = suite.split('-')
        return [[sys.executable, str(scripts / 'benchmark-ci.py'), *common,
                 '--output', str(output / 'performance'), '--broad', '--partition', preset,
                 '--samples', '151', '--sparse-qualification', '--compare-resources']]
    if suite.startswith('math-'):
        _, preset, scale = suite.split('-')
        declared = output / 'declared'
        declared_math(scripts, preset, int(scale), declared)
        return [[sys.executable, str(scripts / 'benchmark-math-ci.py'), *common,
                 '--output', str(output / 'performance'), '--broad', '--broad-preset', preset,
                 '--input-corpus', str(declared), '--samples', '31', '--aa-samples', '151',
                 '--duration-seconds', '.25', '--calibration-seconds', '.5',
                 '--child-timeout-seconds', '600', '--total-seconds', '14400',
                 '--compare-resources']]
    builds = temporary / 'builds'
    cc, cxx = ('cl', 'cl') if os.name == 'nt' else ('cc', 'c++')
    commands = [
        [sys.executable, str(scripts / 'build-gd-bc.py'), '--source', str(source),
         '--revision', revision, '--output', str(builds / role), '--name', role,
         '--c-compiler', cc, '--cxx-compiler', cxx, '--split-point', '126']
        for role, revision in [('base', BASE), ('aa', BASE), ('candidate', CANDIDATE)]]
    commands.append([sys.executable, str(scripts / 'startup-calibration.py'),
                     '--config', str(output / 'startup-config.json'),
                     '--output', str(output / 'startup-results'),
                     '--total-seconds', '2400', '--child-timeout-seconds', '5'])
    return commands


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--suite', choices=SUITES, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--plan-only', action='store_true')
    args = parser.parse_args()
    if not args.plan_only and os.environ.get('GITHUB_RUN_ATTEMPT') != '1':
        parser.error('the original qualification must run once on a hosted runner')
    source = Path.cwd()
    output = args.output.resolve()
    output.mkdir(parents=True)
    for revision in (BASE, CANDIDATE, HARNESS):
        subprocess.run(['git', 'cat-file', '-e', revision + '^{commit}'], check=True)
    archive = subprocess.check_output(['git', 'archive', '--format=zip', HARNESS,
                                       'src/bc/scripts', 'src/bc/fixtures'])
    with tempfile.TemporaryDirectory(prefix='bc-turning-qual-') as directory:
        temporary = Path(directory)
        with ZipFile(io.BytesIO(archive)) as tree:
            files = {name: hashlib.sha256(tree.read(name)).hexdigest()
                     for name in tree.namelist() if not name.endswith('/')}
            tree.extractall(temporary)
        scripts = temporary / 'src/bc/scripts'
        if args.suite.startswith('math-'):
            # The archived controller invokes this path before calibration.
            # The bridge uses the candidate checkout's tests and fixtures.
            relative = 'src/bc/test-math-library-cli.py'
            bridge = Path(__file__).with_name('candidate-preflight.py').read_bytes()
            (temporary / relative).write_bytes(bridge)
            files[relative] = hashlib.sha256(bridge).hexdigest()
        commands = command_plan(args.suite, source, output, temporary, scripts)
        write(output / 'plan.json', dict(base=BASE, candidate=CANDIDATE, harness=HARNESS,
             workflow_source=subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip(),
             suite=args.suite, run_attempt=os.environ.get('GITHUB_RUN_ATTEMPT'),
             plan_only=args.plan_only, commands=commands, harness_files=files))
        if args.plan_only:
            return
        logs = output / 'driver'
        logs.mkdir()
        controller = load(scripts, 'benchmark-math-ci').Commands(logs, 18000)
        try:
            if args.suite == 'startup':
                roles = ('base', 'aa', 'candidate')
                builds = temporary / 'builds'
                for command in commands[:-1]:
                    controller.run(command, timeout=1200)
                entries = [json.loads((builds / role / 'implementations.json').read_text(
                    encoding='utf-8'))['implementations'][0] for role in roles]
                codegen = load(scripts, 'collect-bc-codegen')
                codegen.preserve_builds(builds, output, roles)
                codegen.collect(entries, output / 'startup-images', inspect=False)
                write(output / 'startup-config.json', dict(implementations=entries))
            controller.run(commands[-1])
        finally:
            write(logs / 'orchestration.json', dict(commands=controller.commands, budget_seconds=18000))


if __name__ == '__main__':
    main()
