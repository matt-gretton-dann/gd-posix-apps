#!/usr/bin/env python3
"""Generate outward decimal enclosures from exact Bessel-series remainders."""
import argparse
from fractions import Fraction as F
import hashlib
import json
from math import factorial
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
LOWER = '1.5211440576687651481513018730625235342837878902941129591913817089'
UPPER = '1.5211440576687651481513018730625235342837878902941129591913817090'
ARGUMENTS = ('0', '1', LOWER[:-1] + '8', LOWER, LOWER + '5', UPPER,
             UPPER[:-1] + '1', '2', '3', '8', '8.00000000000000000001')


FIRST_LOWER = '1.2067184630059078866565349810721299413472200414880472421508483780'
FIRST_UPPER = '1.2067184630059078866565349810721299413472200414880472421508483781'
SECOND_LOWER = '2.4881550851691471311579234121812087972033545580535080099003329207'
SECOND_UPPER = '2.4881550851691471311579234121812087972033545580535080099003329208'
# Endpoints and interior points exercise each proposed generic fallback.
ORDER_ONE_ARGUMENTS = (FIRST_LOWER, FIRST_LOWER+'5', FIRST_UPPER,
                       SECOND_LOWER, SECOND_LOWER+'5', SECOND_UPPER,
                       '.1', '1.5', '3', '8', '8.00000000000000000001')
CASES = [(0, argument, 0) for argument in ARGUMENTS]
CASES += [(1, argument, 0) for argument in ORDER_ONE_ARGUMENTS]
CASES += [(-1, '1.5', 0), (1, '-1.5', 0), (-1, '-1.5', 0), (2, '2', 0)]


def generate():
    cases = []
    denominator = 10**100
    for index, (order, argument, scale) in enumerate(CASES):
        n = abs(order)
        x = abs(F(argument))
        term = total = (x/2)**n/factorial(n)
        q = x*x/4
        for k in range(1, 100):
            term *= -q/(k*(n+k))
            total += term
        following = -term*q/(100*(n+100))
        assert q/(100*(n+100)) < 1
        lower, upper = sorted((total, total+following))
        if ((order < 0) != (F(argument) < 0)) and n % 2:
            lower, upper = -upper, -lower
        # Integer floor/ceiling enlarge the exact interval. No decimal or
        # binary floating-point approximation is used to certify these bytes.
        lo = F(lower.numerator*denominator//lower.denominator, denominator)
        hi = F(-((-upper.numerator*denominator)//upper.denominator), denominator)
        assert lo <= lower <= upper <= hi and hi-lo <= F(2, denominator)
        cases.append(dict(id='j-zero-classification-'+str(index), function='j',
                          arguments=[str(order), argument], scale=scale, lower=str(lo), upper=str(hi)))
    # Provenance and canonical JSON comparisons normalize native newlines.
    # Original native artifacts separately retain/hash the physical bytes.
    return dict(method='Exact 100-term alternating series plus first omitted term; outward 100-place rational bounds',
                generator_sha256=hashlib.sha256(Path(__file__).read_text(encoding='utf-8').encode('utf-8')).hexdigest(),cases=cases)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--check', action='store_true')
    args = parser.parse_args()
    target = ROOT/'fixtures/math-bessel-classification-oracles.json'
    raw = (json.dumps(generate(), indent=2, sort_keys=True)+'\n').encode()
    if args.check:
        if target.read_text(encoding='utf-8') != raw.decode('utf-8'):
            parser.error('classification enclosures need regeneration')
    else:
        target.write_bytes(raw)
