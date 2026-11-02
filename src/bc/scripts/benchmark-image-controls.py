#!/usr/bin/env python3
"""Measure fixed byte-identical Windows deployments without claiming a code gain.

All images/classes and bounded confirmations are retained. The intervals describe
repeated launches of each fixed deployment, not a population of image layouts.
SPDX-License-Identifier: Apache-2.0
"""
import argparse
import copy
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import shutil
import sys


def load(name):
    path = Path(__file__).with_name(name + '.py')
    spec = importlib.util.spec_from_file_location('image_controls_' + name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def deploy(entry, output):
    """Copy immutable bytes to six distinct files; no relinking or rewriting."""
    bench = load('bench-bc')
    if len(entry.get('argv', [])) != 1 or entry.get('metadata', {}).get('instrumented') is not False:
        raise ValueError('deployment control requires one uninstrumented executable without extra arguments')
    original = Path(entry['argv'][0]).resolve(strict=True)
    expected = entry['metadata']['binary_sha256']
    if bench.digest_file(original) != expected:
        raise ValueError('source executable does not match its build recipe')
    output.mkdir(exist_ok=False)
    tools, files, identities = [], [], set()
    for index in range(6):
        path = output / ('bc-image-{:02d}.exe'.format(index))
        shutil.copyfile(original, path)
        if bench.digest_file(path) != expected:
            raise ValueError('deployment copy does not match source executable')
        stat = path.stat()
        identity = (stat.st_dev, stat.st_ino)
        if not stat.st_ino or identity in identities:
            raise ValueError('deployment copies need distinct regular file identities')
        identities.add(identity)
        tool = copy.deepcopy(entry)
        tool['name'] = 'image{}'.format(index)
        tool['argv'] = [str(path)]
        tool['metadata']['deployment'] = dict(index=index, source_binary_sha256=expected,
                                             bytes_unchanged=True, physical_copy=True)
        tools.append(tool)
        files.append(dict(index=index, path=path.name, sha256=expected,
                          file_identity_sha256=hashlib.sha256(repr(identity).encode()).hexdigest()))
    return tools, files


def freeze_corpus(output):
    bench = load('bench-bc')
    # Select exact historical names to prevent
    # unrelated new corpus entries silently expanding this diagnostic.
    selected = {
        'full': {'startup', 'loop_1000000', 'array_200000',
                 'compare_high_1000d_x100000', 'compare_equal_1000d_x100000',
                 'decimal_input_4000d_x100', 'binary_input_100000d_x3'},
        'holdout': {'startup', 'loop_875000', 'array_175000',
                    'compare_high_1125d_x87500', 'compare_equal_1125d_x87500',
                    'decimal_input_4500d_x88', 'binary_input_112500d_x3'}}
    for preset, names in selected.items():
        cases = [case for case in bench.corpus(preset, seed=197200 if preset == 'full' else 197201)
                 if case['name'] in names]
        if {case['name'] for case in cases} != names:
            raise ValueError('diagnostic corpus names changed')
        directory = output / preset
        workloads = directory / 'workloads'
        workloads.mkdir(parents=True)
        records = []
        for case in cases:
            payload = case['program'].encode('utf-8')
            oracle = bench.oracle_payload(case)
            (workloads / (case['name'] + '.bc')).write_bytes(payload)
            (workloads / (case['name'] + '.expected')).write_bytes(oracle)
            record = {key: case[key] for key in ('name', 'group', 'parameters')}
            record.update(program_sha256=hashlib.sha256(payload).hexdigest(),
                          oracle_sha256=hashlib.sha256(oracle).hexdigest())
            if case.get('input_mode'):
                record['input_mode'] = case['input_mode']
            records.append(record)
        bench.write_json(directory / 'manifest.json', dict(suite='arithmetic', preset=preset,
                                                          corpus=records))


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    gd = load('build-gd-bc')
    parser.add_argument('--source', required=True)
    parser.add_argument('--base', type=gd.revision_value, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args(argv)
    ci = load('benchmark-math-ci')
    bench = ci.load('bench-bc')
    policy = ci.load('benchmark-policy')
    probe = load('windows-image-probe')
    root = args.output.resolve()
    root.mkdir(exist_ok=False)
    evidence = root / 'evidence'
    evidence.mkdir()
    commands = ci.Commands(root, 6000)
    report = dict(schema=1, status='incomplete', replica=os.environ.get('BC_IMAGE_REPLICA'), base=args.base, controller_sha256=bench.digest_file(__file__),
                  probe_sha256=bench.digest_file(Path(__file__).with_name('windows-image-probe.py')),
                  policy_sha256=policy.source_sha256(), samples=9, warmups=3, deployments=6,
                  probe_launches_per_deployment=3, images=[], probes=[], results=[],
                  performance_acceptance=False,
                  scope='byte-identical deployment diagnostic; within-image intervals only',
                  policy='Six predetermined regular copies, same-length names, all outcomes retained. '
                         'One independent-seed confirmation per suspicious class. No favorable '
                         'reruns, binary mutation, pooling across CPUs or source-causal inference.')
    try:
        bench.write_json(evidence / 'host.json', ci.load('benchmark-ci').host_info())
        if not probe.native_windows_64():
            report.update(status='unavailable', reason='requires native 64-bit Windows Python')
            return 0
        build = root / 'build-source'
        commands.run([sys.executable, str(Path(__file__).with_name('build-gd-bc.py')),
                      '--source', args.source, '--revision', args.base, '--output', str(build),
                      '--name', 'source', '--split-point', '126'], timeout=1800)
        entry = json.loads((build / 'implementations.json').read_text())['implementations'][0]
        tools, report['images'] = deploy(entry, evidence / 'images')
        bench.write_json(evidence / 'implementations.json', dict(implementations=tools))
        for trial in range(3):
            for tool in (tools if trial % 2 == 0 else list(reversed(tools))):
                result = probe.probe(tool['argv'][0])
                report['probes'].append(dict(image=tool['name'], trial=trial, **result))
        report['module_summary'] = {tool['name']: dict(
            collected=sum(r['status'] == 'collected' for r in report['probes'] if r['image'] == tool['name']),
            bases=sorted({r['module_base'] for r in report['probes']
                          if r['image'] == tool['name'] and r['status'] == 'collected'}))
            for tool in tools}
        freeze_corpus(evidence / 'prepared')
        bench.write_json(evidence / 'decisions.json', report)
        # All build and module-probe processes have ended before elapsed data.
        for preset in ('full', 'holdout'):
            target = evidence / (preset + '-paired')
            command = [sys.executable, str(Path(__file__).with_name('bench-bc.py')),
                       '--config', str(evidence / 'implementations.json'), '--output', str(target),
                       '--reference', 'image0', '--replay-corpus', str(evidence / 'prepared' / preset),
                       '--samples', '9', '--warmups', '3', '--timeout', '120',
                       '--seed', str(19741000 if preset == 'full' else 19741001),
                       '--require-provenance']
            commands.run(command, timeout=1800)
            ci.measurements(target)
            rows = json.loads((target / 'summary.json').read_text())['workloads']
            suspicious = [row['name'] for row in rows if row['name'] != 'startup' and any(
                policy.aa_direction(row['candidate_over_reference'][tool['name']]) != 0
                for tool in tools[1:])]
            confirmations = {}
            if suspicious:
                destination = evidence / (preset + '-confirmation')
                confirm = command + [part for name in suspicious for part in ('--workload', name)]
                confirm[confirm.index('--output') + 1] = str(destination)
                confirm[confirm.index('--seed') + 1] = str(19742000 if preset == 'full' else 19742001)
                commands.run(confirm, timeout=1800)
                ci.measurements(destination)
                confirmations = {r['name']: r for r in json.loads(
                    (destination / 'summary.json').read_text())['workloads']}
            for row in rows:
                for tool in tools[1:]:
                    first = row['candidate_over_reference'][tool['name']]
                    second = confirmations.get(row['name'], {}).get(
                        'candidate_over_reference', {}).get(tool['name'])
                    report['results'].append(dict(preset=preset, name=row['name'], image=tool['name'],
                                                  first=first, confirmation=second,
                                                  status='inconclusive' if row['name'] == 'startup'
                                                  else policy.aa_status(first, second)))
            bench.write_json(evidence / 'decisions.json', report)
        for image in report['images']:
            if bench.digest_file(evidence / 'images' / image['path']) != image['sha256']:
                raise ValueError('deployment image changed during measurements')
        report['status'] = 'diagnostic complete; source adoption and causal attribution unproven'
        return 0
    except Exception as error:
        report.update(status='failed', failure=str(error))
        return 1
    finally:
        # Keep compiler/source/build provenance and the exact source image.
        source = root / 'build-source'
        if source.is_dir():
            for name in ('implementations.json', 'build-manifest.json', 'compile_commands.json',
                         'multiply-threshold.hh', 'standard-library-probe.cc'):
                if (source / name).is_file():
                    shutil.copyfile(source / name, evidence / ('source-' + name))
            for path in source.glob('command-*.log'):
                shutil.copyfile(path, evidence / ('source-' + path.name))
        for path in root.glob('command-*.log'):
            shutil.copyfile(path, evidence / ('controller-' + path.name))
        bench.write_json(evidence / 'decisions.json', report)
        bench.write_json(evidence / 'orchestration.json', dict(commands=commands.commands))


if __name__ == '__main__':
    sys.exit(main())
