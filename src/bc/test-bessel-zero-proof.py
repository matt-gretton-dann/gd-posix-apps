"""Certify zero-scale J0/J1 cells with exact alternating remainders."""
from fractions import Fraction as F
import json
from pathlib import Path

BRACKETS = (
    (0, '1.5211440576687651481513018730625235342837878902941129591913817089',
     '1.5211440576687651481513018730625235342837878902941129591913817090', False),
    (1, '1.2067184630059078866565349810721299413472200414880472421508483780',
     '1.2067184630059078866565349810721299413472200414880472421508483781', True),
    (1, '2.4881550851691471311579234121812087972033545580535080099003329207',
     '2.4881550851691471311579234121812087972033545580535080099003329208', False))


def enclosure(order, x, terms=80):
    term = F(1) if order == 0 else x/2
    total = term
    q = x*x/4
    for k in range(1, terms+1):
        term *= -q/(k*(order+k))
        total += term
    following = -term*q/((terms+1)*(order+terms+1))
    # Subsequent ratios decrease, so the first omitted term encloses the tail.
    assert q/((terms+1)*(order+terms+1)) < 1
    return tuple(sorted((total, total+following)))


def certify():
    for order, lower, upper, increasing in BRACKETS:
        a, b = F(lower), F(upper)
        assert b-a == F(1, 10**64) and 0 < a < b < 3
        al, au = enclosure(order, a)
        bl, bu = enclosure(order, b)
        if increasing:
            assert au < F(1, 2) < bl
        else:
            assert bu < F(1, 2) < al

    # J0'=-J1, J1'=J0-J1/x imply decreasing J0^2+J1^2 for x>0.
    j0, j1 = enclosure(0, F(3)), enclosure(1, F(3))
    energy = max(map(abs, j0))**2 + max(map(abs, j1))**2
    assert energy < F(182588, 10**6) < F(1, 4)
    # On [0,2], J1>=x/2*(1-q/2)>0 and J0>=1-q>=0.
    # On [2,3], P3<=J0<=P2, P3 decreases and P2<=1/4.
    q = F(9, 4)
    p3 = 1-q+q*q/4-q**3/36
    assert p3 == -F(77, 256) > -F(1, 2)
    # J1''/x is bounded above by its fourth alternating partial sum.
    # The derivative of this polynomial is positive: its discriminant is
    # negative and its leading coefficient positive. Its endpoint is <0.
    p4 = -F(3, 8)+F(5, 24)*q-F(7, 192)*q*q+q**3/320
    discriminant = F(7, 96)**2-4*F(5, 24)*F(3, 320)
    tail_ratio = q/F(5*6)*F(11, 9)*F(10, 8)
    assert p4 == -F(1131, 20480) < 0
    assert discriminant == -F(23, 9216) < 0 and tail_ratio == F(11, 96) < 1
    assert enclosure(1, F(3, 2))[0] > F(1, 2)
    assert 0 < j1[0] < j1[1] < F(1, 2)
    # Concavity puts J1 above its nonnegative endpoint chord on [0,3].
    # The alternating P2 bounds it below one on [0,2], and by 7/8 on [2,3].
    assert F(3, 2)*F(7, 12) == F(7, 8) < 1
    assert -F(1, 2)+F(9, 4)/6 < 0
    # Test-first J1 proofs precede its implementation. Bind the already
    # implemented J0 constants here; the implementation adds the J1 binding.
    source = (Path(__file__).parent/'math-library.bc').read_text(encoding='utf-8')
    assert 'if (x<'+BRACKETS[0][1]+')' in source
    assert 'if (x>'+BRACKETS[0][2]+')' in source
    return dict(endpoint_brackets=3, terms=80, energy_at_three_upper='0.182588',
                j0_lower_on_two_to_three=str(p3), j1_concavity_upper=str(p4),
                coefficient_derivation='docs/utilities/bc-zero-scale-bessel.md',
                scope='Exact rational certificates; no elapsed or adoption result.', qualified=False)


if __name__ == '__main__':
    print(json.dumps(certify(), indent=2))
