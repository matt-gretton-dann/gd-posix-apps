#!/usr/bin/env python3
"""Prepare independent exact array-layout workloads for frozen paired replay.

This creates inputs, never elapsed measurements. Python 3.6+.
SPDX-License-Identifier: Apache-2.0
"""
import argparse
import hashlib
import json
import math
from pathlib import Path


def corpus(preset):
    """Keep full/holdout dimensions separate and quick cases correctness-only."""
    dimensions = {'quick': (128, 64, 128, 256, 512, 64, 4),
                  'full': (200000, 20000, 32768, 200000, 1000000, 4096, 64),
                  'holdout': (225000, 22500, 36864, 225000, 1125000, 4608, 56)}
    dense, sparse, clustered, overwrites, absent, snapshot, calls = dimensions[preset]
    multiplier = 104729 if preset != 'holdout' else 104723
    if math.gcd(multiplier, dense) != 1:
        raise ValueError('dense access must be a permutation')
    prime = 999999937 if preset != 'holdout' else 999999929
    def total(count):
        cycles, tail = divmod(count, 23)
        return cycles * sum(range(23)) + tail * (tail - 1) // 2
    cases = []
    def add(shape, program, expected, parameters):
        cases.append(dict(name='array_layout_' + shape + '_' + preset, group='array_' + shape,
                          program='scale=0\n' + program.rstrip() + '\n',
                          expected=[str(value) for value in expected], parameters=parameters))
    add('sequential', 'for(i=0;i<{n};i++) a[i]=i%23\ns=0\n'
        'for(i=0;i<{n};i++) s+=a[i]\ns'.format(n=dense), [total(dense)], dict(entries=dense))
    add('permuted_dense', 'for(i=0;i<{n};i++) a[(i*{p})%{n}]=i%23\ns=0\n'
        'for(i=0;i<{n};i++) s+=a[i]\ns'.format(n=dense,p=multiplier), [total(dense)],
        dict(entries=dense, permutation_multiplier=multiplier))
    # A prime modulus gives unique sparse keys. Calculate the absent-read oracle
    # from the actual final key/value map, including any accidental neighbors.
    values = {(i * multiplier) % prime: i % 23 for i in range(sparse)}
    probes = sum(values.get(((i * multiplier) % prime) + 1, 0) for i in range(sparse))
    add('sparse', 'for(i=0;i<{n};i++) a[(i*{p})%{m}]=i%23\ns=0\n'
        'for(i=0;i<{n};i++) s+=a[(i*{p})%{m}]\ns\ns=0\n'
        'for(i=0;i<{n};i++) s+=a[(i*{p})%{m}+1]\ns'.format(n=sparse,p=multiplier,m=prime),
        [total(sparse),probes], dict(entries=sparse, modulus=prime, multiplier=multiplier))
    add('clustered', 'for(i=0;i<{n};i++) a[1000000+(i%32)*10000000+i/32]=i%23\ns=0\n'
        'for(i=0;i<{n};i++) s+=a[1000000+(i%32)*10000000+i/32]\ns'.format(n=clustered),
        [total(clustered)], dict(entries=clustered, clusters=32))
    add('overwrite', 's=0\nfor(i=0;i<{n};i++) {{a[999999999]=i%23;s+=a[999999999]}}\n'
        's\na[999999999]'.format(n=overwrites), [total(overwrites),(overwrites-1)%23],
        dict(entries=1, iterations=overwrites, highest_index=999999999))
    add('absent', 'a[0]=5\ns=0\nfor(i=0;i<{n};i++) s+=a[999999999-i]\ns\na[0]'.format(n=absent),
        [0,5], dict(entries=1, absent_reads=absent))
    add('snapshot', 'define f(b[]) {{\n b[0]=7\n return(b[0]+b[{last}])\n}}\n'
        'for(i=0;i<{n};i++) a[i]=i%23\ns=0\nfor(i=0;i<{calls};i++) s+=f(a[])\ns\na[0]'.format(
            n=snapshot,last=snapshot-1,calls=calls), [calls*(7+(snapshot-1)%23),0],
        dict(entries=snapshot, calls=calls, mutations_per_call=1))
    large = 1234567890123456789012345678901234567890
    add('heap_values', 'for(i=0;i<{n};i++) a[i]={value}+i%23\ns=0\n'
        'for(i=0;i<{n};i++) s+=a[i]\ns'.format(n=clustered,value=large),
        [clustered*large+total(clustered)], dict(entries=clustered, coefficient_digits=40))
    return cases


def prepare(root, preset):
    root.mkdir(parents=True, exist_ok=False)
    (root / 'workloads').mkdir()
    rows = []
    for case in corpus(preset):
        program = case['program'].encode('utf-8')
        oracle = ('\n'.join(case['expected']) + '\n').encode('utf-8')
        (root / 'workloads' / (case['name'] + '.bc')).write_bytes(program)
        (root / 'workloads' / (case['name'] + '.expected')).write_bytes(oracle)
        rows.append(dict(name=case['name'], group=case['group'], parameters=case['parameters'],
                         program_sha256=hashlib.sha256(program).hexdigest(),
                         oracle_sha256=hashlib.sha256(oracle).hexdigest()))
    manifest = dict(schema=1, suite='arithmetic', preset=preset, corpus_seed=197209,
                    generator_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                    scope='array storage comparison; quick is correctness only', corpus=rows)
    (root / 'manifest.json').write_bytes((json.dumps(manifest, indent=2, sort_keys=True)+'\n').encode('utf-8'))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--preset', choices=('quick', 'full', 'holdout'), default='full')
    args = parser.parse_args()
    prepare(args.output.resolve(), args.preset)


if __name__ == '__main__':
    main()
