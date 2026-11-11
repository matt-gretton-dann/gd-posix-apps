# Bounded ordinary math kernels

This candidate replaces iterative kernels at entry scales 1 through 20. It
keeps only the six public ordinary bc functions, their public dependencies,
caller state, existing range reductions and generic algorithms. There is no
VM recognition of math names. Scale zero, higher precision and inputs outside
an explicitly bounded domain retain the earlier algorithms. The earlier unit
arctangent literals, exponential underflow guard, Bessel zero classifications
and Hankel selector remain first.

These are error certificates and correctness evidence, not elapsed results or
adoption. The frozen #717 cohort remains unqualified: its failures, competitive
misses, missing Linux images/startup and inconclusive macOS startup are not
repaired by this source change. A new exact-source hosted cohort is required.

## Rational residual certificate

All coefficient vectors below are ascending. For a diagonal degree-m ratio
P/Q, normalize Q(0)=1 and form the exact Taylor coefficients of Q*F-P. They
vanish through degree 2m. With |z|<=Z<1 and |a_k|<=1 for the selected Taylor
series, the absolute residual is bounded by

    sum(k=0..64, |residual_k| Z^k)
      + sum(j, |q_j|) Z^65/(1-Z).

Divide by a positive lower bound on Q. For atan, sin, cos and log use z=x*x;
sin, atan and log also multiply the ratio by x. The coefficient functions are
(-1)^k/(2k+1), (-1)^k/(2k+1)!, (-1)^k/(2k)!, and 1/(2k+1), respectively.
Exp uses z=x and coefficients 1/k!. The proof test recomputes cancelled
prefixes and finite/geometric tails using exact rational arithmetic. Degrees
were selected against the worst S=20 tail budget, without elapsed data.

For the integer-coefficient Horner evaluations, let u=10^-W, d=1/(1-Z), and
C=Q_integer(0). Each signed polynomial has absolute evaluation error <=d*u.
A normalized denominator lower bound L is obtained by subtracting all negative
coefficient magnitudes at Z from 1. The proof checks d*u<C*L/2. Quotient
rounding, polynomial errors and a squared-argument perturbation of at most u
are bounded using

    2d/(C*L) + 2 P_abs(Z) d/(C*L^2) + 1
      + (P_abs'(Z) Q_abs(Z)+P_abs(Z) Q_abs'(Z))/L^2

working units. Multiplication by the leading x adds one unit and scales the
previous bound by |x|. Every selected kernel has total kernel error <4u.
Actual bc expressions evaluate the quotient before multiplying by x; changing
that association requires a new roundoff argument. Additions are exact at the
selected decimal scale. Signed multiplications/divisions truncate toward zero.

## Arctangent

Keep reciprocal, near-one and half-angle reduction at the original working
precision. Their documented reduced argument bound is |x|<=1/9, reconstruction
multiplier <=8, and inherited argument/constant error <=34u_old. Afterwards
set W=S+14, check scale readback and clip the reduced argument once. Because
W<=W_old, inherited error is <=34u. Clipping adds at most 8u after
reconstruction; the kernel adds at most 32u. The conservative 114u budget
covers their sum. The common sign, reciprocal, offset and rounding footer is
unchanged.

For |x|<=.01, the [2/2] numerator is [945,735,64] and denominator
[945,1050,225]. Otherwise use [4/4]:

    P = [3828825,6831825,3738735,638055,16384]
    Q = [3828825,8108100,5675670,1455300,99225].

Both denominator polynomials are positive on z>=0. At S=20 their worst
reconstructed tails are <.000118 and <.003571 output units, respectively.
Thus final error is bounded by .5U+114u+8*tail <.505U, U=10^-S.

## Exponential

Keep the existing coercion, halving, magnitude precision guard, negative
inversion before squaring, and final rounding. For at most seven halvings,
M=2^k<=128 and the actual reduced x is in [0,1/8]. The original argument can
exceed M*x by less than 2M*u; therefore it is <17, and the positive decimal
magnitude guard g=floor(floor(original_x)/2)+1 is at most nine. The original
working precision is at least S+g+14; negative arguments use g=0.

The [8/8] coefficients are

    C = [518918400,259459200,60540480,8648640,831600,55440,2520,72,1].

Evaluate E from the even coefficients at x*x, O as x times the odd polynomial,
and r=(E+O)/(E-O). Positive, downward-rounded E and O have E>=C[0] and
E-O>.9*C[0]. Differentiate this ratio with respect to E, O and x*x to bound
its kernel error below 4u; the exact proof test checks those bounds. The
rounded r is >=1. The squared reconstruction can overestimate; M*(4u+tail)
is below .001, so its exponential amplification is below two (also bounded
by the corresponding geometric series). Conservative full errors are

    positive: M*10^g*(12u+2*tail),
    negative: M*(8u+tail).

These include input/halving and reconstruction rounding. Inversion precedes
negative reconstruction. At S=20 the worst positive amplified tail is below
.002821U. Including final rounding, every stated scale/guard/halving budget
is <.505U. Larger k or S retain the dynamic series. Zero keeps its old path.

## Sine and cosine

Keep the original magnitude-dependent phase precision, public a(1) call,
period reduction and scaling by 1/5 or 1/8. Check the *actual* scaled argument
before lowering only kernel precision to W=S+14. The domains are |x|<=.64 for
sin(x)/x and |x|<=.4 for cos(x). A public redefinition of a may yield a different
period; out-of-domain results retain the old generic series and call semantics.
If that callback changes ibase, retain the generic path too: its later literal
interpretation is observable ordinary bc behavior, even though it no longer
represents the standard mathematical function.

Degree-five coefficients are:

    sin P = [363275871831577908403200,-54839355237791393068800,
             2120649063015013090560,-31712777908498486800,
             203385425766914820,-481959816488503]
    sin Q = [363275871831577908403200,5706623400804924998400,
             44454031219351353600,219578286347980560,
             707177798947620,1230626892363]
    cos P = [280669698675663897600,-135355106332109664000,
             9249069176002656000,-204260164593685200,
             1767200224481340,-5342316580775]
    cos Q = [280669698675663897600,4979743005722284800,
             44369900711136000,254519795209680,
             969954677340,2045322787].

The inherited phase error, using the unchanged magnitude guard and argument
scaling, is <=1.69e-13 U. Sine keeps its quintuple polynomial with conservative
amplification 160 and 64u local reconstruction error. Cosine keeps three double
angle steps, amplification 125 and 62u error. Intermediate magnitudes stay
below 1.01: the initial errors are tiny and each exact reconstruction stays
in [-1,1], while each perturbed double-angle step has derivative magnitude
below five. Clipping adds at most 8u. Thus

    .5U + phase + amplification*tail + (4*amplification+72)u <.505U.

The proof checks every entry scale and signed fixed-point kernels; CLI tests
cover the custom-period domain boundary as well as ordinary reductions.
Large coefficients may add cost. No speed benefit is inferred from degree.

## Logarithm

Keep normalization, square roots, public l(10), the stored offset h and the
original ratio t=(x-1)/(x+1) at their old precision. At S=1..20, nonzero t and
root multiplier k<=1024, clip t once at W=S+14, then evaluate t*P(t*t)/Q(t*t):

    P = [305540235,-698377680,552473922,-175855680,19225635,-327680]
    Q = [305540235,-800224425,758107350,-312161850,52026975,-2401245].

The original root interval gives |t|<=1/9. The atanh derivative there is at
most 81/80; clip and kernel errors after reconstruction are therefore bounded
by 2048*(81/80+4)u. Retain the documented inherited phase bound .00074U and
the exact stored offset. The tail and final rounding keep the full error below
.505U. Larger multipliers/scales and the existing high-scale block algorithm
retain their generic behavior.

The multiplier guard is defensive: its false outcome is infeasible under the
unchanged normalizer at S=1..20. An unnormalized input has decimal exponent
m with |m|<64+floor(S/2)<=74; a normalized mantissa lies in [1,10). Thus
|log(x)|<75*log(10)<175 before roots. The existing root-error certificate
bounds the accumulated logarithmic perturbation by .001. Ten roots suffice
because 175/1024+.001<2/11<log(1.2), and also <2/9<log(1.25)=-log(.8).
The two lower bounds follow from the first positive term of the atanh
identity; exp(7/3)>10 bounds log(10). The exact proof test checks these
inequalities. Decision coverage records this one infeasible outcome separately
rather than claiming it was executed; no feasible outcome is waived.

## Bessel weighted finite Horner evaluation

After the existing zero guards, symmetries and Hankel attempt, use this path
only for S=1..20 and 0<x<=128. Put y=trunc_W(x/2), Q=trunc_W(y*y), prepare the
positive leading factor t from 1 using a multiply and divide at working
precision, t=trunc_W(trunc_W(t*y)/i), and evaluate backwards

    R_N=1,
    R_(k-1)=1-Q*R_k/(k*(n+k)), k=N,...,1,
    result=trunc_W(t*R_0).

This is the finite form of the [defining Bessel series](https://dlmf.nist.gov/10.2.E2).
For an upper argument endpoint A, the earlier first zero guard leaves
n<=n_max=max(ceil(3A),4(S+1))-1. Use E=10^ceil(7A/16), W=S+ceil(7A/16)
+ceil(log10(2*n_max+3))+4. A positive Taylor enclosure proves exp(16/7)<10,
so exp(x)<E. The sum of absolute weighted series coefficients is <=exp(x):
it is a subset of the positive terms in exp(y)*exp(y).

The leading factor is downward-rounded and nonnegative, and Q<=y*y.
The identity (n+k)!>=i!*(n-i+k)! bounds each of n leading-factor errors by
2E*u after applying the remaining weights. Horner local errors total at most
2E*u; perturbing Q adds at most E*u. There is no loose product of independent
leading-factor and polynomial error bounds. The final multiply and argument
coercion leave a conservative combined arithmetic/argument budget

    ((2*n+3)*E+4)u.

The argument loss is <2u. Differentiating the
[integer-order real integral](https://dlmf.nist.gov/10.9.E2) gives |J_n'|<=1,
so this loss is covered by the displayed allowance.

For A/2<N+2 the first omitted positive term is largest at n=0, since its ratio
as n increases is at most (A/2)/(n+N+2)<1. Subsequent terms contract by at
most (A*A/4)/(N+2)^2. The complete absolute tail is therefore bounded by

    (A*A/4)^(N+1) / ((N+1)!^2 * (1-(A*A/4)/(N+2)^2)).

The compact degree rule is N=floor((h*S+p)/d):

| A | h | p | d |
|---|---:|---:|---:|
| .5 | 5 | 30 | 14 |
| 1 | 7 | 53 | 17 |
| 2 | 8 | 64 | 15 |
| 4 | 8 | 92 | 13 |
| 8 | 9 | 166 | 13 |
| 16 | 14 | 393 | 17 |
| 32 | 16 | 748 | 17 |
| 64 | 19 | 1563 | 18 |
| 128 | 15 | 2430 | 14 |

Every one of the 180 band/scale cases has tail <.004U and combined error
including final rounding <.505U. Generic zero/high-scale/out-of-domain paths
remain available; the earlier Hankel call order preserves public dependencies.

### Turning-region order bound

For 16<x<=128 and n>=x, a shorter table uses bands (L,A] with n>=L.
At a fixed omitted index k=N+1, the absolute term increases with x and
decreases with n throughout these bands. In particular, increasing n
multiplies the term by (x/2)/(n+k+1), which is at most one. The first
omitted term and following contraction therefore give the bound

    (A/2)^L * (A*A/4)^(N+1) /
      ((N+1)! * (L+N+1)! * (1-(A*A/4)/((N+2)*(L+N+2)))).

Use the same N=floor((h*S+p)/d) representation.

| L | A | h | p | d |
|---:|---:|---:|---:|---:|
| 16 | 24 | 10 | 275 | 11 |
| 24 | 32 | 17 | 558 | 18 |
| 32 | 48 | 15 | 665 | 14 |
| 48 | 64 | 11 | 595 | 10 |
| 64 | 96 | 10 | 848 | 9 |
| 96 | 128 | 15 | 1542 | 13 |

For y=x/2, the normalized absolute polynomial and its derivative with
respect to Q are bounded by exp(Q/(n+1)). Each remaining leading-factor
product is bounded by exp(y), since its consecutive denominators are
at least the corresponding factorial. Thus exp(y+y*y/(n+1)) bounds the
growth of the absolute series and each weighted arithmetic perturbation.
Since n>=x, y+y*y/(n+1)<=3x/4. Use E=10^(floor(21*x/64)+1) in the same
roundoff budget ((2*n+3)*E+4)u, and compute W=S+floor(21*x/64)+1+
length(2*n+3)+4 at scale zero. The arithmetic allowance is below .0002U,
and the exact proof checks the .004U tail allowance for every band and
entry scale. The argument-coercion and final-rounding bounds still apply.

For j(20,20) at scale 20, the bounds select 43 Horner steps and 33 working
places. The general bounded table selects 62 steps and 41 places for the
same input. These are counts derived from the source. Elapsed qualification
requires a prospective hosted comparison.

## Validation and remaining qualification

`test-rational-kernel-proof.py` checks exact residuals, positive denominators,
all error budgets, signed fixed-point rational kernels and weighted finite
Bessel roundoff. Independent FLINT 3.6.0 / python-flint 0.9.0 enclosures in
`math-rational-kernel-oracles.json` cover 4,917 explicit boundary cases,
including all Bessel scales/bands, signs, degree/precision boundaries, zero,
long coefficients and generic fallbacks. Their generator/source hashes,
outward dyadic endpoints and width are verified without requiring FLINT at
normal test time. Regenerate them with the pinned test environment and
the following command from the repository root:

```sh
python src/bc/scripts/generate-math-case-list-oracles.py \
  src/bc/fixtures/math-rational-kernel-cases.json \
  src/bc/fixtures/math-rational-kernel-oracles.json
```

The ordinary and embedded source CLI checks also cover caller bases/state,
public redefinitions and clamped-scale failures. Portable external execution,
cross-platform hosted correctness, branch coverage, allocation failures,
startup/resources and exact-source paired qualification remain required.

The regenerated interpreted decision ledger checks 5,787 certified numerical
results, two undefined-domain guard calls, four certified custom-period calls
and two observable input-base cases. It observes 349 of 366 outcomes directly;
independent working-scale clipping models bring the covered feasible total to
363. The three explicitly infeasible outcomes are the two existing negative
exponential-result branches and the proven low-scale logarithm multiplier
limit. All 14 working-scale setters are independently fault-modeled. This is
interpreted-library decision evidence, not compiler MC/DC or elapsed evidence.
