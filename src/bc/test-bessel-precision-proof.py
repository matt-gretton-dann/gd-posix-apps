"""Certify positive-scale guard bounds with exact bc truncation models."""
from fractions import Fraction as F
from math import factorial
import hashlib
import json
from pathlib import Path


def truncate(value, precision):
    denominator = 10**precision
    if value < 0:
        return -F((-value.numerator*denominator)//value.denominator, denominator)
    return F((value.numerator*denominator)//value.denominator, denominator)


def reference(order, x, threshold):
    term = (x/2)**order/factorial(order)
    total = term
    q = x*x/4
    for k in range(1, 3000):
        following = -term*q/(k*(order+k))
        if q/(k*(order+k)) < 1 and abs(following) < threshold:
            return tuple(sorted((total, total+following)))
        total += following
        term = following
    raise AssertionError('reference enclosure did not converge')


def check_model(order, argument, scale):
    assert order >= 0 and scale > 0
    x = F(argument)
    assert x >= 0
    parts = argument.split('.')
    fractional = len(parts[1]) if len(parts) == 2 else 0
    coefficient = max(len(''.join(parts).lstrip('0')), 1)
    magnitude = max(coefficient-fractional, 1)
    growth = int(7*x/16)+1
    total = scale+growth+magnitude+1
    precision = scale+growth+magnitude+2*len(str(total))+32
    old = scale+growth+coefficient+2*len(str(scale+growth+coefficient+1))+32
    unit = F(1, 10**precision)
    assert precision+1 <= 19*total and order < 7*(precision+1)
    clipped = truncate(x, precision)
    assert 0 <= x-clipped < unit
    q = -truncate(truncate(clipped*clipped, precision)/4, precision)
    assert 0 <= clipped*clipped/4-abs(q) < 2*unit
    term = F(1)
    initial_steps = 0
    for i in range(1, order+1):
        # The source truncates the multiplication before doing the division.
        term = truncate(truncate(term*clipped, precision)/(2*i), precision)
        initial_steps += 1
        if not term:
            break
    value = term
    k = 1
    series_steps = 0
    while term:
        term = truncate(truncate(term*q, precision)/(k*(order+k)), precision)
        value += term
        k += 1
        series_steps += 1
        assert series_steps < 8*(precision+1)
    bound = 2*10**6*(precision+1)**2*10**growth*unit
    lower, upper = reference(order, x, bound/100)
    assert max(abs(value-lower), abs(value-upper)) < bound
    assert bound < F(1, 2*10**scale)
    half = F(1, 2*10**scale)
    result = truncate(value+(half if value >= 0 else -half), scale)
    assert max(abs(result-lower), abs(result-upper)) <= F(1, 10**scale)
    return dict(order=order, argument=argument, scale=scale,
                coefficient_digits=coefficient, integer_magnitude_digits=magnitude,
                old_work_scale=old, model_work_scale=precision,
                input_clipped=(x != clipped), initial_steps=initial_steps,
                series_steps=series_steps, certified_error_bound=True,
                output_within_one_ULP=True, production_execution=False)


def cases():
    arguments = ('1.'+'23456789'*32, '3.'+'9'*256)
    result = [(n, argument, scale) for n in (0, 1, 5) for argument in arguments
              for scale in (1, 20, 100)]
    result += [(5, '12.'+'34567890'*32, 20), (300, '300', 100)]
    result += [(0, '0.'+'0'*255+'1', 20), (1, '0.'+'0'*255+'1', 20)]
    return result


def certify():
    # Exact upper bound for e and integer inequality establish exp(x)<10^G.
    partial = sum((F(1, factorial(i)) for i in range(11)), F())
    assert partial+F(1, factorial(11))/(1-F(1, 12)) < F(87, 32)
    assert 87**16 < 10**7*32**16
    assert F(2*19**2, 10**25) < F(1, 2)
    source = (Path(__file__).parent/'math-library.bc').read_text(encoding='utf-8')
    # Bind the modeled budget to the implemented positive-scale guard; the
    # caller-state tests independently exercise its actual arithmetic paths.
    assert 'h=length(x)\n  if (s>0) {\n    h-=scale(x)\n    if (h<1) h=1' in source
    assert 'w=s+g+h+2*length(s+g+h+1)+32' in source
    reports = [check_model(n, argument, scale) for n, argument, scale in cases()]
    return dict(kind='Exact rational truncation model; no production elapsed observations',
                source_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                cases=len(reports), reports=reports,
                math_source_normalized_sha256=hashlib.sha256(source.encode('utf-8')).hexdigest(),
                qualified=False)


if __name__ == '__main__':
    print(json.dumps(certify(), indent=2))
