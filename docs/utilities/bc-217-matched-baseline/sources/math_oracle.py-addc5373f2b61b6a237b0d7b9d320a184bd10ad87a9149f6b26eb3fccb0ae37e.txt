"""Exact decimal-ULP decisions against certified rational enclosures.

This module uses only the standard library. FLINT belongs to the separate
reference generator, never the bc runtime or the decision arithmetic.
"""
from fractions import Fraction
import re

_DECIMAL = re.compile(r'-?(?:[0-9]+(?:\.[0-9]+)?|\.[0-9]+)\Z')
MAX_SCALE = 100000


def decimal_fraction(text):
    """Decode numeric bc output without binary floating-point conversion."""
    if not isinstance(text, str) or not _DECIMAL.fullmatch(text):
        raise ValueError('expected a finite plain decimal value')
    return Fraction(text)


def decimal_ulp(scale):
    if type(scale) is not int or not 0 <= scale <= MAX_SCALE:
        raise ValueError('scale exceeds the exact-oracle resource limit')
    return Fraction(1, 10 ** scale)


def classify(candidate, lower, upper, scale):
    """Return pass, fail, or refine; equality at one ULP is accepted.

    Pass bounds the error at BOTH endpoints. Fail requires the distance from
    the ENTIRE enclosure to exceed one ULP. Other cases need a tighter oracle;
    an interval crossing the decision boundary is not a proven failure.
    """
    if not all(isinstance(x, Fraction) for x in (candidate, lower, upper)):
        raise TypeError('decision operands must be exact rational values')
    if lower > upper:
        raise ValueError('reversed oracle enclosure')
    ulp = decimal_ulp(scale)
    if max(abs(candidate - lower), abs(candidate - upper)) <= ulp:
        return 'pass'
    distance = max(lower - candidate, candidate - upper, Fraction(0))
    return 'fail' if distance > ulp else 'refine'


def nearest_decimal(lower, upper, scale):
    """Construct a nearby test value, not a runtime mathematical function."""
    decimal_ulp(scale)
    if not all(isinstance(x, Fraction) for x in (lower, upper)):
        raise TypeError('bounds must be exact rational values')
    if lower > upper:
        raise ValueError('reversed oracle enclosure')
    coefficient = round((lower + upper) * 10 ** scale / 2)
    sign = '-' if coefficient < 0 else ''
    digits = str(abs(coefficient)).rjust(scale + 1, '0')
    return sign + (digits[:-scale] + '.' + digits[-scale:] if scale else digits)
