#!/usr/bin/env python3
"""Partition all old controls and independently certified fresh validation.

No implementation is invoked and no elapsed observation is read.
SPDX-License-Identifier: Apache-2.0
"""
import argparse
from fractions import Fraction
import hashlib
import importlib.util
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def load(name):
    spec = importlib.util.spec_from_file_location(name, ROOT / 'scripts' / (name+'.py'))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


OLD = load('math-qualification-inputs')


def controls(preset):
    rows = OLD.exposed(preset)
    # Every formerly fresh #714 record is now an exposed holdout control.
    # Preserve its name, program, oracle and historical parameter metadata.
    if preset == 'holdout':
        rows += OLD.fresh(OLD.bench_module())
    assert len(rows) == (94 if preset == 'full' else 130)
    return rows


def validation(fresh=False):
    stem = 'math-rational-validation' if fresh else 'math-reconstruction-validation'
    cases_path = ROOT / 'fixtures' / (stem + '-cases.json')
    oracles_path = ROOT / 'fixtures' / (stem + '-oracles.json')
    cases = json.loads(cases_path.read_text(encoding='utf-8'))
    oracles = json.loads(oracles_path.read_text(encoding='utf-8'))
    assert oracles['case_file_sha256'] == hashlib.sha256(cases_path.read_bytes().replace(b'\r\n', b'\n')).hexdigest()
    assert oracles['python_flint_version'] == '0.9.0' and oracles['flint_version'] == '3.6.0'
    for name, key in (('generate-math-case-list-oracles.py', 'generator_sha256'),
                      ('generate-math-oracles.py', 'underlying_generator_sha256')):
        assert oracles[key] == hashlib.sha256((ROOT/'scripts'/name).read_bytes().replace(b'\r\n', b'\n')).hexdigest()
    references = {row['id']: row for row in oracles['cases']}
    assert len(references) == len(cases['cases']) == 38
    bench = OLD.bench_module()
    earlier = controls('full') + controls('holdout')
    if fresh:
        earlier += validation()
    old_keys = {(p['function'], tuple(map(Fraction, p['arguments'])), p['scale'])
                for row in earlier for p in (row['parameters'],)}
    rows = []
    for case in cases['cases']:
        reference = references[case['id']]
        assert all(case[key] == reference[key] for key in ('function', 'arguments', 'scale'))
        assert (case['function'], tuple(map(Fraction, case['arguments'])), case['scale']) not in old_keys
        assert Fraction(reference['lower']) < Fraction(reference['upper'])
        for mode, count in (('one_shot', 1), ('repeated', 2)):
            name = 'math_'+case['id'].replace('-', '_')+'_'+mode+'_x'+str(count)
            call = '{}({})'.format(case['function'], ','.join(case['arguments']))
            program = 'scale={}\n'.format(case['scale'])
            program += ('r='+call if count == 1 else 'for(i=0;i<{};i++) r={}'.format(count, call))
            program += '\nr\nscale\nibase\nscale(r)\n'
            oracle = dict(kind='decimal-ulp', scale=case['scale'], lower=reference['lower'],
                          upper=reference['upper'], reference_id=case['id'],
                          expected_state=[str(case['scale']), '10', str(case['scale'])])
            payload = bench.oracle_payload(dict(oracle=oracle)).decode('utf-8')
            rows.append(dict(name=name, group='math_'+case['id'].replace('-', '_'),
                program=program, oracle_payload=payload, corpus_seed=cases['seed'],
                program_sha256=hashlib.sha256(program.encode()).hexdigest(),
                oracle_sha256=hashlib.sha256(payload.encode()).hexdigest(),
                parameters=dict(function=case['function'], arguments=case['arguments'],
                    scale=case['scale'], iterations=count, mode=mode,
                    calibration='fixed count; not calibrated', regime=case['regime'],
                    exposure=('fresh seeded bounded-rational validation before first timing' if fresh else
                              'fresh seeded final-reconstruction validation before first timing'))))
    return rows


def write(rows, family, preset, partition, shard, shards, directory):
    # The old writer preserves exact program/oracle bytes and the declared
    # schema. Add provenance for this union rather than changing old fixtures.
    OLD.write(rows, 'exposed' if family == 'controls' else 'validation', preset,
              partition, shard, shards, directory)
    path = directory / 'manifest.json'
    manifest = json.loads(path.read_text(encoding='utf-8'))
    manifest.update(family=family, corpus_seed=rows[0]['corpus_seed'],
        origin_seeds=sorted({row['corpus_seed'] for row in rows}), generator_sha256=hashlib.sha256(
        Path(__file__).read_bytes().replace(b'\r\n', b'\n')).hexdigest(),
        scope=('All224 old records remain controls; historical exposure metadata is preserved.'
               if family == 'controls' else
               'Fresh successor workloads, both modes, declared before first timing.' if family == 'fresh' else
               'Previously exposed #717 validation workloads, both modes.'))
    path.write_bytes((json.dumps(manifest, indent=2, sort_keys=True)+'\n').encode('utf-8'))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--family', choices=('controls', 'validation', 'fresh'), required=True)
    parser.add_argument('--preset', choices=('full', 'holdout'), required=True)
    parser.add_argument('--partition', choices=OLD.BANDS, required=True)
    parser.add_argument('--shard', type=int, required=True)
    parser.add_argument('--shards', type=int, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    if args.family != 'controls' and args.preset != 'holdout':
        parser.error('fresh validation uses the holdout namespace')
    rows = controls(args.preset) if args.family == 'controls' else validation(fresh=args.family == 'fresh')
    write(OLD.select(rows, args.partition, args.shard, args.shards), args.family, args.preset,
          args.partition, args.shard, args.shards, args.output)


if __name__ == '__main__':
    main()
