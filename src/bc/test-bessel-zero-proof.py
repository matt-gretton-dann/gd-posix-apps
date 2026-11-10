"""Certify zero-scale J0 thresholds from exact alternating remainders."""
from decimal import Decimal,localcontext,ROUND_FLOOR
from fractions import Fraction as F
import json


def enclosure(n,x,k=80):
    term=F(1) if n==0 else x/2
    total=term
    q=x*x/4
    for i in range(1,k+1):
        term *= -q/(i*(n+i))
        total += term
    following=term*(-q/((k+1)*(n+k+1)))
    assert abs(following)<abs(term)
    assert q/((k+2)*(n+k+2))<1
    return min(total,total+following),max(total,total+following)


def dec_j0(x):
    term=total=Decimal(1);q=x*x/4
    for k in range(1,160):
        term *= -q/(k*k);total+=term
    return total


with localcontext() as c:
    c.prec=100
    lo=Decimal('1.5');hi=Decimal('1.6')
    for _ in range(330):
        mid=(lo+hi)/2
        if dec_j0(mid)>Decimal('.5'):lo=mid
        else:hi=mid
    unit=Decimal(10)**-64
    lower=lo.quantize(unit,rounding=ROUND_FLOOR)
    upper=lower+unit
    # Decimal locates candidates only; exact rational endpoint proof below.
    a,b=F(lower),F(upper)
    al,au=enclosure(0,a);bl,bu=enclosure(0,b)
    assert al>F(1,2) and bu<F(1,2)
    j0=enclosure(0,F(3));j1=enclosure(1,F(3))
    abs0=max(abs(v) for v in j0);abs1=max(abs(v) for v in j1)
    energy=abs0*abs0+abs1*abs1
    assert energy<F(1,4)
    # On [0,2], J1 >= x/2*(1-x*x/8)>0 for x>0.
    # Thus J0'=-J1 makes the threshold crossing unique there.
    # On [2,3], q in [1,9/4], alternating P3<=J0<=P2.
    # P3'= -1+q/2-q*q/12 <0 (negative discriminant).
    # P2=(1-q/2)^2 <=1/4 across that interval.
    q=F(9,4);p3=1-q+q*q/4-q**3/36
    assert p3>F(-1,2)
    # Series identities J0'=-J1 and J1'=J0-J1/x yield
    # (J0^2+J1^2)'=-2*J1^2/x<=0 for x>0.
    # Certified energy(3)<1/4 therefore establishes |J0(x)|<1/2
    # on the remaining ray, without using asymptotic approximations.
    # Pin the portable source's proposed strict comparison constants. These
    # decimal candidates are certified above using rational arithmetic; no
    # floating-point result is used as evidence.
    expected_lower='1.5211440576687651481513018730625235342837878902941129591913817089'
    expected_upper='1.5211440576687651481513018730625235342837878902941129591913817090'
    assert str(lower)==expected_lower and str(upper)==expected_upper
    # Bind the mathematical endpoint proof to the constants actually used by
    # the portable implementation, rather than certifying unused literals.
    from pathlib import Path
    source=(Path(__file__).parent/'math-library.bc').read_text()
    assert 'if (x<'+expected_lower+')' in source
    assert 'if (x>'+expected_upper+')' in source
    result=dict(kind='Exact rational feasibility only; no runtime or elapsed result',
        lower=str(lower),upper=str(upper),endpoint_terms=80,
        lower_above_half=float(al-F(1,2)),upper_below_half=float(F(1,2)-bu),
        energy_at_three_upper=float(energy),p3_minimum=str(p3),
        coefficient_derivation='docs/utilities/bc-zero-scale-bessel.md',
        qualified=False)
    print(json.dumps(result,indent=2))
