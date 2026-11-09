#!/usr/bin/env python3
"""Generate the prospectively declared finite-Bessel experiment, without timing.

Refs #217 comment 6748002125 and #197. Historical payloads remain unchanged;
new programs use independent full/held RNGs and forward-series enclosures.
SPDX-License-Identifier: Apache-2.0
"""
from fractions import Fraction
from functools import lru_cache
import hashlib
import json
from math import factorial
from pathlib import Path
import random

SEEDS = {'full': 217709, 'holdout': 217710}
MIDPOINTS = ('1.52114405766876514815130187', '1.52114405766876514815130188')


def inputs(preset):
    if preset not in SEEDS:
        raise ValueError('Horner qualification requires full or holdout')
    rng = random.Random(SEEDS[preset])
    return [
        ('small_zero', 0, '{}.{:06d}'.format(rng.randrange(1, 4), rng.randrange(10**6))),
        ('small_one', 1, '1.{:06d}'.format(rng.randrange(10**6))),
        ('long_one', 1, '2.' + ''.join(str(rng.randrange(10)) for _ in range(256))),
        ('generic_argument', 1, '4.{:06d}'.format(rng.randrange(1, 10**6))),
        ('generic_order', 2, '1.{:06d}'.format(rng.randrange(10**6))),
    ]


@lru_cache(maxsize=64)
def enclosure(order, argument, scale):
    """Sum the infinite series forward and round its proven bounds outward.

    The runtime uses a finite backward polynomial. This oracle instead sums
    exact rational terms until a geometric majorant bounds all omitted terms.
    Outward decimal bounds keep serialized oracles small even for long inputs.
    """
    x = Fraction(argument)
    if order not in (0, 1, 2) or not 0 <= x <= 5 or not 0 <= scale <= 101:
        raise ValueError('qualification oracle domain exceeded')
    q = x * x / 4
    term = (x / 2)**order / factorial(order)
    total = term
    for index in range(1, 513):
        term *= -q / (index * (order + index))
        total += term
        omitted = abs(term) * q / ((index + 1) * (order + index + 1))
        contraction = q / ((index + 2) * (order + index + 2))
        if contraction < 1 and omitted / (1 - contraction) < Fraction(1, 10**(scale + 16)):
            tail = omitted / (1 - contraction)
            factor = 10**(scale + 14)
            lower, upper = (total - tail) * factor, (total + tail) * factor
            lower = Fraction(lower.numerator // lower.denominator, factor)
            upper = Fraction(-(-upper.numerator // upper.denominator), factor)
            assert lower < upper and upper - lower <= Fraction(3, factor)
            return str(lower), str(upper)
    raise ValueError('qualification forward-series bound did not converge')


def additional(preset):
    result = []
    declarations = [(name, order, x, scale, 'prospective seeded input')
                    for name, order, x in inputs(preset) for scale in (20, 100, 101)]
    declarations += [('midpoint_' + str(i), 0, x, 0, 'existing midpoint control')
                     for i, x in enumerate(MIDPOINTS)]
    for regime, order, x, scale, exposure in declarations:
        lower, upper = enclosure(order, x, scale)
        for mode in ('one_shot', 'repeated'):
            count = 1 if mode == 'one_shot' else (50 if scale <= 20 else 20)
            if preset == 'holdout' and mode == 'repeated':
                count = max(2, count - max(1, count // 8))
            name = 'math_horner_{}_{}_scale{}_{}_x{}'.format(preset, regime, scale, mode, count)
            call = 'j({},{})'.format(order, x)
            program = 'scale={}\n'.format(scale)
            program += 'r=' + call if mode == 'one_shot' else 'for(i=0;i<{};i++) r={}'.format(count, call)
            program += '\nr\nscale\nibase\nscale(r)\n'
            result.append(dict(name=name, group='math_j_horner_' + regime, program=program, expected=None,
                parameters=dict(function='j', arguments=[str(order), x], scale=scale,
                    iterations=count, mode=mode, calibration='fixed count; not calibrated',
                    horner_experiment=True, exposure=exposure, corpus_seed=SEEDS[preset]),
                oracle=dict(kind='decimal-ulp', scale=scale, lower=lower, upper=upper,
                    reference_id='horner-' + preset + '-' + regime,
                    expected_state=[str(scale), '10', str(scale)])))
    return result


def cases(bench, preset):
    inputs(preset)  # Reject unsupported presets before building historical cases.
    result = bench.corpus(preset, suite='math') + additional(preset)
    assert len(result) == len({case['name'] for case in result}) == 94
    return result


def partition(corpus, name):
    predicates = {'low': lambda s: s <= 20, 'middle': lambda s: 20 < s <= 100,
                  'high': lambda s: s > 100}
    if name not in predicates:
        raise ValueError('unknown Horner scale partition')
    return [case for case in corpus if predicates[name](case['parameters']['scale'])]


def write_partition(bench, preset, name, directory):
    """Declare inputs for initial calibration, then frozen-count replay."""
    selected = partition(cases(bench, preset), name)
    directory.mkdir(parents=True, exist_ok=False)
    workloads = directory / 'workloads'
    workloads.mkdir()
    records = []
    for case in selected:
        program, oracle = case['program'].encode('utf-8'), bench.oracle_payload(case)
        (workloads / (case['name'] + '.bc')).write_bytes(program)
        (workloads / (case['name'] + '.expected')).write_bytes(oracle)
        records.append(dict(name=case['name'], group=case['group'], parameters=case['parameters'],
            program_sha256=hashlib.sha256(program).hexdigest(),
            oracle_sha256=hashlib.sha256(oracle).hexdigest()))
    fixture = Path(__file__).resolve().parents[1] / 'fixtures/math-benchmark-oracles.json'
    manifest = dict(kind='unmeasured-math-inputs-v1', suite='math', preset=preset,
        corpus_seed=SEEDS[preset], partition=name,
        generator_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        reference_fixture_sha256=hashlib.sha256(fixture.read_bytes()).hexdigest(),
        corpus=records, scope='Prospective input snapshot before calibration or elapsed observations; '
            'historical references and fresh exact-rational oracle payloads are separately byte-bound.')
    (directory / 'manifest.json').write_text(json.dumps(manifest, indent=2, sort_keys=True) + '\n', encoding='utf-8')
    return selected
