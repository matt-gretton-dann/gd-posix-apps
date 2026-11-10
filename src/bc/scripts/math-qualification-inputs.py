#!/usr/bin/env python3
"""Declare disjoint exposed controls and fresh certified Bessel inputs.

This script never invokes an implementation or reads elapsed observations.
SPDX-License-Identifier: Apache-2.0
"""
import argparse
from fractions import Fraction
from functools import lru_cache
import hashlib
import importlib.util
import json
from math import factorial
from pathlib import Path
import random

ROOT = Path(__file__).resolve().parents[1]
MASTER = ROOT / 'fixtures/math-qualification-exposed-inputs.json'
# Frozen numerical source, independent of the later protocol publication SHA.
SEED = int('ca75c979cc3eba5ddacd01503da05ac9c4030937', 16) ^ 217714
BANDS = ('low', 'middle', 'high')


def bench_module():
    spec = importlib.util.spec_from_file_location('qualification_bench', ROOT / 'scripts/bench-bc.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def band(scale):
    return 'low' if scale <= 20 else 'middle' if scale <= 100 else 'high'


def pair_key(row):
    p = row['parameters']
    return row['group'], p['function'], tuple(p['arguments']), p['scale']


def exposed(preset):
    master = json.loads(MASTER.read_text(encoding='utf-8'))
    assert master['kind'] == 'exposed-math-inputs-v1'
    rows = [r for r in master['corpus'] if r['preset'] == preset]
    if preset not in ('full', 'holdout') or len(rows) != 94:
        raise ValueError('exposed controls require full or holdout')
    for row in rows:
        for key, digest in (('program', 'program_sha256'), ('oracle_payload', 'oracle_sha256')):
            if hashlib.sha256(row[key].encode('utf-8')).hexdigest() != row[digest]:
                raise ValueError('exposed input identity mismatch')
    return rows


@lru_cache(maxsize=32)
def enclosure(order, argument, scale):
    """Independent exact forward series with a geometric omitted-tail bound."""
    x = Fraction(argument)
    if order not in (0, 1, 2) or not 0 <= x <= 5 or not 0 <= scale <= 1000:
        raise ValueError('fresh qualification oracle domain exceeded')
    q = x * x / 4
    term = (x / 2)**order / factorial(order)
    total = term
    for k in range(1, 513):
        term *= -q / (k * (order + k))
        total += term
        omitted = abs(term) * q / ((k + 1) * (order + k + 1))
        contraction = q / ((k + 2) * (order + k + 2))
        if contraction < 1 and omitted / (1 - contraction) < Fraction(1, 10**(scale + 16)):
            tail, factor = omitted / (1 - contraction), 10**(scale + 14)
            lower, upper = (total - tail) * factor, (total + tail) * factor
            lower = Fraction(lower.numerator // lower.denominator, factor)
            upper = Fraction(-(-upper.numerator // upper.denominator), factor)
            assert 0 < upper - lower <= Fraction(3, factor)
            return str(lower), str(upper)
    raise ValueError('fresh qualification series did not converge')


def fresh(bench):
    rng = random.Random(SEED)
    specs = []
    for order in (0, 1):
        for scale, digits in ((0, 6), (1, 256), (20, 6), (20, 256),
                              (100, 6), (100, 256), (1000, 6), (1000, 64)):
            x = str(rng.randrange(1, 4)) + '.' + ''.join(str(rng.randrange(10)) for _ in range(digits))
            specs.append((order, x, scale, digits))
    for scale in (20, 100):
        x = '1.' + ''.join(str(rng.randrange(10)) for _ in range(6))
        specs.append((2, x, scale, 6))
    rows = []
    for index, (order, x, scale, digits) in enumerate(specs):
        lower, upper = enclosure(order, x, scale)
        for mode in ('one_shot', 'repeated'):
            count = 1 if mode == 'one_shot' else 2
            name = 'math_certified_validation_{}_scale{}_{}_x{}'.format(index, scale, mode, count)
            call = 'j({},{})'.format(order, x)
            program = 'scale={}\n'.format(scale)
            program += 'r=' + call if mode == 'one_shot' else 'for(i=0;i<{};i++) r={}'.format(count, call)
            program += '\nr\nscale\nibase\nscale(r)\n'
            oracle = dict(kind='decimal-ulp', scale=scale, lower=lower, upper=upper,
                reference_id='certified-validation-' + str(index), expected_state=[str(scale), '10', str(scale)])
            case = dict(oracle=oracle)
            raw = bench.oracle_payload(case)
            rows.append(dict(name=name, group='math_j_certified_validation_' + str(index),
                program=program, oracle_payload=raw.decode('utf-8'),
                program_sha256=hashlib.sha256(program.encode('utf-8')).hexdigest(),
                oracle_sha256=hashlib.sha256(raw).hexdigest(), corpus_seed=SEED,
                parameters=dict(function='j', arguments=[str(order), x], scale=scale,
                    iterations=count, mode=mode, calibration='fixed count; not calibrated',
                    input_digits=digits, exposure='fresh seeded validation before first timing')))
    assert len(rows) == 36
    return rows


def select(rows, partition, shard, shards):
    if partition not in BANDS or shards < 1 or not 0 <= shard < shards:
        raise ValueError('invalid fixed input partition')
    rows = [r for r in rows if band(r['parameters']['scale']) == partition]
    keys = sorted({pair_key(r) for r in rows})
    assignment = {key: index % shards for index, key in enumerate(keys)}
    result = [r for r in rows if assignment[pair_key(r)] == shard]
    if not result:
        raise ValueError('empty input partition')
    return result


def write(rows, family, preset, partition, shard, shards, directory):
    directory.mkdir(parents=True, exist_ok=False)
    work = directory / 'workloads'
    work.mkdir()
    records = []
    for row in rows:
        (work / (row['name'] + '.bc')).write_bytes(row['program'].encode('utf-8'))
        (work / (row['name'] + '.expected')).write_bytes(row['oracle_payload'].encode('utf-8'))
        records.append({key: row[key] for key in
            ('name', 'group', 'parameters', 'program_sha256', 'oracle_sha256')})
    fixture = ROOT / 'fixtures/math-benchmark-oracles.json'
    manifest = dict(kind='unmeasured-math-inputs-v1', suite='math', preset=preset,
        family=family, partition=partition, shard=shard, shards=shards,
        corpus_seed=SEED if family == 'validation' else rows[0]['corpus_seed'],
        generator_sha256=hashlib.sha256(Path(__file__).read_bytes().replace(b'\r\n', b'\n')).hexdigest(),
        reference_fixture_sha256=hashlib.sha256(fixture.read_bytes().replace(b'\r\n', b'\n')).hexdigest(),
        exposed_master_sha256=hashlib.sha256(MASTER.read_bytes().replace(b'\r\n', b'\n')).hexdigest(),
        corpus=records, scope=('Previously exposed controls; no transferred observations.' if family == 'exposed'
            else 'Fresh seeded validation declared before first elapsed observation.'))
    (directory / 'manifest.json').write_bytes((json.dumps(manifest, indent=2, sort_keys=True) + '\n').encode('utf-8'))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--family', choices=('exposed', 'validation'), required=True)
    parser.add_argument('--preset', choices=('full', 'holdout'), required=True)
    parser.add_argument('--partition', choices=BANDS, required=True)
    parser.add_argument('--shard', type=int, required=True)
    parser.add_argument('--shards', type=int, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    if args.family == 'validation' and (args.preset != 'holdout' or args.shards != 1):
        parser.error('fresh validation uses one fixed holdout shard per scale band')
    rows = fresh(bench_module()) if args.family == 'validation' else exposed(args.preset)
    write(select(rows, args.partition, args.shard, args.shards), args.family, args.preset,
          args.partition, args.shard, args.shards, args.output)


if __name__ == '__main__':
    main()
