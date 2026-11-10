#!/usr/bin/env python3
"""Certify explicit function/scale cases without a scale cross product.

Reuse the pinned enclosure implementation without changing the generator
identity recorded by existing reference files. FLINT is a test-only tool.
SPDX-License-Identifier: Apache-2.0
"""
import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
import sys


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('cases', type=Path)
    parser.add_argument('output', type=Path)
    args = parser.parse_args()
    if hasattr(sys, 'set_int_max_str_digits'):
        sys.set_int_max_str_digits(1000000)
    import flint
    if flint.__version__ != '0.9.0' or flint.__FLINT_VERSION__ != '3.6.0':
        parser.error('regeneration requires python-flint 0.9.0 with FLINT 3.6.0')
    original = Path(__file__).with_name('generate-math-oracles.py')
    spec = importlib.util.spec_from_file_location('math_generator', original)
    generator = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(generator)
    rows, identifiers = [], set()
    for case in json.loads(args.cases.read_text(encoding='utf-8'))['cases']:
        if case['id'] in identifiers:
            raise ValueError('duplicate reference case identifier')
        identifiers.add(case['id'])
        bound = generator.enclosure(case['function'], case['arguments'],
                                    case['scale'], 16, 65536)
        rows.append(dict(case, **bound))
    report = dict(schema=1, python_flint_version=flint.__version__,
                  flint_version=flint.__FLINT_VERSION__, guard_digits=16,
                  max_precision_bits=65536,
                  generator_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                  underlying_generator_sha256=hashlib.sha256(original.read_bytes()).hexdigest(),
                  case_file_sha256=hashlib.sha256(args.cases.read_bytes()).hexdigest(),
                  endpoint_format='exact rational outward dyadic bounds', cases=rows)
    args.output.write_bytes((json.dumps(report, indent=2, sort_keys=True)+'\n').encode('utf-8'))
    print('Certified {} explicit function/scale references'.format(len(rows)))


if __name__ == '__main__':
    main()
