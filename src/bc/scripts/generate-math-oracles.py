#!/usr/bin/env python3
"""Generate exact rational FLINT/Arb reference enclosures for bc math tests.

Install the pinned test-only requirements in an isolated environment. Arb
propagates exact-rational input uncertainty; exported dyadic endpoints are
outward bounds. Refine until their ACTUAL width meets the requested budget.
"""
import argparse
from fractions import Fraction
import hashlib
import json
from pathlib import Path
import sys

from math_oracle import decimal_fraction, decimal_ulp


def enclosure(function, arguments, scale, guard_digits, max_bits):
    from flint import arb, ctx, fmpq

    rational = [decimal_fraction(x) for x in arguments]
    if function not in ('s', 'c', 'a', 'l', 'e', 'j'):
        raise ValueError('unknown mathematical function')
    if len(rational) != (2 if function == 'j' else 1):
        raise ValueError('wrong reference argument count')
    if function == 'l' and rational[0] <= 0:
        raise ValueError('logarithm input outside its real domain')
    if function == 'j' and rational[0].denominator != 1:
        raise ValueError('Bessel order must be an exact integer')
    target_width = decimal_ulp(scale) / 10 ** guard_digits
    # This is an initial precision estimate only, not an error certificate.
    magnitude = max([0] + [x.numerator.bit_length() - x.denominator.bit_length()
                           for x in rational])
    bits = max(128, 4 * (scale + 32) + magnitude)
    saved = ctx.prec
    try:
        while bits <= max_bits:
            ctx.prec = bits
            values = [arb(fmpq(x.numerator, x.denominator)) for x in rational]
            if function == 'j':
                result = values[1].bessel_j(int(rational[0]))
            else:
                method = {'s': 'sin', 'c': 'cos', 'a': 'atan',
                          'l': 'log', 'e': 'exp'}[function]
                result = getattr(values[0], method)()
            if result.is_finite():
                lower = Fraction(str(result.lower().fmpq()))
                upper = Fraction(str(result.upper().fmpq()))
                if lower <= upper and upper - lower <= target_width:
                    return {'lower': str(lower), 'upper': str(upper), 'precision_bits': bits}
            bits *= 2
    finally:
        ctx.prec = saved
    raise ValueError('reference enclosure exceeds the configured precision budget')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('cases', type=Path)
    parser.add_argument('output', type=Path)
    parser.add_argument('--guard-digits', type=int, default=16)
    parser.add_argument('--max-bits', type=int, default=65536)
    args = parser.parse_args()
    if not 1 <= args.guard_digits <= 1000 or not 128 <= args.max_bits <= 1000000:
        parser.error('reference resource limits are outside their supported range')
    if hasattr(sys, 'set_int_max_str_digits'):
        sys.set_int_max_str_digits(1000000)
    import flint
    if flint.__version__ != '0.9.0' or flint.__FLINT_VERSION__ != '3.6.0':
        parser.error('regeneration requires python-flint 0.9.0 with FLINT 3.6.0')
    source = json.loads(args.cases.read_text(encoding='utf-8'))
    rows = []
    identifiers = set()
    for case in source['cases']:
        if case['id'] in identifiers:
            raise ValueError('duplicate reference case identifier')
        identifiers.add(case['id'])
        for scale in source['scales']:
            bound = enclosure(case['function'], case['arguments'], scale,
                              args.guard_digits, args.max_bits)
            rows.append(dict(case, scale=scale, **bound))
    report = {'schema': 1, 'python_flint_version': flint.__version__,
              'flint_version': flint.__FLINT_VERSION__, 'guard_digits': args.guard_digits,
              'max_precision_bits': args.max_bits,
              'generator_sha256': hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
              'case_file_sha256': hashlib.sha256(args.cases.read_bytes()).hexdigest(),
              'endpoint_format': 'exact rational outward dyadic bounds', 'cases': rows}
    # Explicit bytes keep certified artifacts identical on Windows.
    args.output.write_bytes((json.dumps(report, indent=2, sort_keys=True) + '\n').encode('utf-8'))
    print('Certified {} function/scale reference enclosures'.format(len(rows)))


if __name__ == '__main__':
    main()
