"""Certify finite Bessel tail and modeled decimal truncation bounds."""
import argparse
from fractions import Fraction as F
from math import factorial
from pathlib import Path
import json


def trunc(value, scale):
    factor=10**scale
    magnitude=abs(value.numerator)*factor//value.denominator
    return F(magnitude if value>=0 else -magnitude,factor)


def polynomial(q,n,k):
    value=F(1)
    for j in range(k,0,-1):
        value=1-q*value/(j*(n+j))
    return value

# Exact scale-independent coefficients for the proposed evaluation order.
# q truncation <=4u; Horner local error <=2u, weight sum <64.
q_error=F(4);polynomial_error=2*64+64*q_error
j1_error=2*polynomial_error+F(64,2)+F(1,2)+1
assert polynomial_error==384 and j1_error<1024
# Bound exp(4), which bounds the recurrence weights and q derivative.
partial=sum((F(4**j,factorial(j)) for j in range(17)),F())
assert partial+F(4**17,factorial(17))/(1-F(4,18))<64
for entry_scale in range(101):
    last=16+(entry_scale+1)//2
    tail=F(2*4**(last+1),factorial(last+1)**2)/(1-F(4,(last+2)**2))
    assert tail<F(1,10**(entry_scale+12))
    assert tail+F(1024,10**(entry_scale+16))<F(1,10**(entry_scale+11))
rows=[]
points=['0','0.0000000000000000001','1','1.125','2','3.8317059702075123156144358863081607665645452742878','3.99999999999999999999999999999999999999999999999999','4']
for scale in [0,1,2,20,100]:
    k=16+(scale+1)//2;w=scale+16;unit=F(1,10**w)
    tail=F(2*4**(k+1),factorial(k+1)**2)/(1-F(4,(k+2)**2))
    for n in [0,1]:
        for literal in points:
            original=F(literal);x=trunc(original,w)
            q=trunc(trunc(x*x,w)*F(1,4),w)
            result=F(1)
            for j in range(k,0,-1):
                result=1-trunc(trunc(q*result,w)/(j*(n+j)),w)
            exact=polynomial(original*original/4,n,k)
            if n==1:
                result=trunc(trunc(result*x,w)/2,w)
                exact=exact*original/2
            assert abs(result-exact)<=1024*unit
            assert tail+1024*unit<F(1,10**(scale+11))
            quantum=F(1,10**scale);margin=quantum/F(10**11)
            shifted=abs(result)+quantum/2
            rounded=trunc(shifted,scale)
            remainder=shifted-rounded
            accepted=margin<remainder<quantum-margin
            if accepted:
                signed=rounded if result>=0 else -rounded
                assert max(abs(signed-(exact-tail)),abs(signed-(exact+tail)))<=quantum
            rows.append({'scale':scale,'order':n,'input':literal,'K':k,'W':w,'rounding_window_accepts':accepted,'arithmetic_error_bound_checked':True,'ULP_enclosure_checked_if_accepted':accepted})
report={'scope':'Prospective exact-rational model only, not production bc semantics, runtime test coverage, performance, resource or adoption proof. Exposed diagnostic inputs, not fresh qualification holdouts.','cases':len(rows),'accepted_window_cases':sum(r['rounding_window_accepts'] for r in rows),'rows':rows}
parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--output',type=Path)
args=parser.parse_args()
if args.output:
    args.output.write_text(json.dumps(report,indent=2)+'\n')
print(len(rows),'exact-rational model cases checked; no bc process or elapsed clock used')
