#!/usr/bin/env python3
"""Reproduce prospective source/input hashes without running implementations.
SPDX-License-Identifier: Apache-2.0
"""
import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
import tempfile

ROOT=Path(__file__).resolve().parents[3]
DIRECTORY=ROOT/'docs/utilities/bc-reconstruction-qualification'


def module(name):
    spec=importlib.util.spec_from_file_location(name,ROOT/'src/bc/scripts'/(name+'.py'))
    result=importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result


def digest(path):
    return hashlib.sha256(path.read_bytes().replace(b'\r\n',b'\n')).hexdigest()


def declaration():
    old=json.loads((ROOT/'docs/utilities/bc-certified-bessel/qualification-hashes.json').read_text())
    files=set(old['files'])
    files.update([
        '.github/workflows/bc-reconstruction-qualification.yml',
        'docs/utilities/bc-bounded-rational-kernels.md',
        'src/bc/test-rational-kernel-proof.py','src/bc/test-math-oracle.py',
        'src/bc/fixtures/.gitattributes',
        'src/bc/fixtures/math-rational-kernel-cases.json',
        'src/bc/fixtures/math-rational-kernel-oracles.json',
        '.github/workflows/bc-bessel-zero-correctness.yml',
        'docs/utilities/bc-reconstruction-qualification/qualification.md',
        'docs/utilities/bc-portable-math.md','docs/utilities/bc-native-functions.md',
        'docs/utilities/bc-function-slot-storage.md','docs/utilities/bc-performance-acceptance.md',
        'src/bc/function-slot.hh','src/bc/test-function-slot.cc','src/bc/test-vm-lifetime.cc',
        'src/bc/integer-recurrence.hh','src/bc/integer-recurrence.cc',
        'src/bc/native-expression.hh','src/bc/native-expression-arm64.cc','src/bc/native-expression-x64.cc',
        'src/bc/test-integer-recurrence.cc','src/bc/test-native-expression.cc','src/bc/test-number.cc',
        'src/bc/test-math-budget-proof.py','src/bc/test-reconstruction-inputs.py',
        'src/bc/test-integer-qualification-inputs.py',
        'src/bc/test-reconstruction-declaration.py','src/bc/test-reconstruction-hosted-limits.py',
        'src/bc/fixtures/math-reconstruction-validation-cases.json',
        'src/bc/fixtures/math-reconstruction-validation-oracles.json',
        'src/bc/fixtures/math-rational-validation-cases.json',
        'src/bc/fixtures/math-rational-validation-oracles.json',
        'src/bc/scripts/reconstruction-qualification-inputs.py',
        'src/bc/scripts/integer-qualification-inputs.py',
        'src/bc/scripts/generate-reconstruction-declaration.py',
        'src/bc/scripts/generate-math-case-list-oracles.py','src/bc/scripts/generate-math-oracles.py',
        'src/bc/scripts/verify-bessel-zero-evidence.py','src/bc/test-bessel-zero-evidence.py'])
    files.update(['src/bc/scripts/math_oracle.py','src/bc/scripts/math_library_coverage.py',
                  'src/bc/test-math-library-coverage.py','src/bc/fixtures/math-library-decisions.json'])
    files.update(['src/bc/scripts/benchmark-ci.py', 'src/bc/test-benchmark-ci.py',
                  'src/bc/scripts/compare-bc-resources.py', 'src/bc/test-resource-comparison.py'])
    inputs=module('reconstruction-qualification-inputs')
    integers=module('integer-qualification-inputs')
    selected={}
    with tempfile.TemporaryDirectory() as tmp:
        for family,preset,shards in (('controls','full',6),('controls','holdout',6),
                                      ('validation','holdout',3),('fresh','holdout',3)):
            rows=inputs.controls(preset) if family=='controls' else inputs.validation(fresh=family == 'fresh')
            for band in inputs.OLD.BANDS:
                for shard in range(shards):
                    name='{}-{}-{}'.format(preset if family=='controls' else family,band,shard)
                    path=Path(tmp)/name
                    chosen=inputs.OLD.select(rows,band,shard,shards)
                    inputs.write(chosen,family,preset,band,shard,shards,path)
                    selected[name]=dict(manifest_sha256=digest(path/'manifest.json'),
                        records=len(chosen),names=[r['name'] for r in chosen])
        for label, successor in (('validation', False), ('fresh', True)):
            name = 'arithmetic-integer-' + label
            path = Path(tmp) / name
            integers.write(path, successor=successor)
            cases = integers.cases(successor=successor)
            selected[name] = dict(manifest_sha256=digest(path/'manifest.json'),
                records=len(cases), names=[r['name'] for r in cases])
    assert len(selected)==56
    arithmetic = module('benchmark-ci')
    bench = module('bench-bc')
    for preset in ('full', 'holdout'):
        names = arithmetic.selected_cases(bench, preset, True)
        selected['arithmetic-' + preset] = dict(records=len(names), names=names)
    selected['startup'] = dict(records=0, null_blocks=60, pairs=151)
    assert sum(v['records'] for k,v in selected.items()
               if not k.startswith('arithmetic-') and k != 'startup') == 376
    return dict(schema=2,base='b2b98ec1629c9aa98a3abbde01744f4bb95ac444',
        source_identity='Exact immutable PR head recorded by each original job',
        files={name:digest(ROOT/name) for name in sorted(files)},selected_inputs=selected,
        mathematical_controls=224,previous_validation_math_records=76,fresh_math_records=76,previous_validation_integer_workloads=5,fresh_integer_workloads=5,
        declared_jobs=177,math_samples=dict(aa=151,paired=31),arithmetic_samples=151,
        scope='Prospective canonical hashes only; excludes this manifest to avoid self-reference.')


if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output',type=Path,default=DIRECTORY/'qualification-hashes.json')
    parser.add_argument('--check',action='store_true')
    args=parser.parse_args()
    raw=(json.dumps(declaration(),indent=2,sort_keys=True)+'\n').encode('utf-8')
    if args.check:
        # Compare canonical text while preserving raw checkout bytes in evidence.
        assert args.output.read_bytes().replace(b'\r\n',b'\n')==raw,'prospective declaration differs from source or inputs'
    else:
        args.output.write_bytes(raw)
    print('Verified prospective source and 59 input partitions; no implementation executed')
