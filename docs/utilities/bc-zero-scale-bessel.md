# Certified zero-scale Bessel classification

Refs #217 and #197. This experiment starts from accepted main `3beebb14`.
It is independent of the rejected finite Horner candidate in PR #712 and
of the function-storage experiment in PR #713. No elapsed improvement or
adoption is established. The total recorded goal-time cap remains 864,000
seconds; previous failed and inconclusive outcomes remain part of the record.

For normalized order zero and entry scale zero, round J0 to the nearest
integer by classifying its value around one half. Retain the accepted series
inside the uncertain threshold bracket, for other orders/scales, and for
arguments larger than eight. Symmetry makes J0 even. The restriction to
arguments at most eight is necessary for language semantics: the accepted
Hankel selector requires x > max(8*(n*n+1), w), so the admitted range never
calls the user-redefinable a/c/s functions. Larger arguments must retain
those ordinary public calls, even though the mathematical bound below
extends further.

## Mathematical argument

Use the absolutely convergent series, uniformly convergent with their
termwise derivatives on every bounded interval:

J0(x) = sum[k>=0] (-1)^k x^(2k) / (4^k (k!)^2)

J1(x) = sum[k>=0] (-1)^k x^(2k+1) / (2*4^k k! (k+1)!)

The ratio of successive magnitudes tends to zero, also after
differentiation; this establishes the uniform convergence assertion on
bounded intervals. Comparing coefficients gives J0' = -J1:
2(k+1)/(4^(k+1)((k+1)!)^2) = 1/(2*4^k k!(k+1)!).
Likewise J1' + J1/x = J0, since the coefficient multiplier is
(2k+2)/(2*4^k k!(k+1)!) = 1/(4^k(k!)^2).

On 0 < x <= 2, let q=x*x/4 <= 1. Alternating remainders give
J1(x) >= x/2*(1-q/2) > 0 and J0(x) >= 1-q >= 0. Thus J0 strictly
decreases from one there. Exact rational sums of 81 terms and the first
omitted term certify J0(L)>1/2 and J0(U)<1/2 at:

L = 1.5211440576687651481513018730625235342837878902941129591913817089

U = 1.5211440576687651481513018730625235342837878902941129591913817090

There is exactly one half crossing on [0,2]. Decimal bisection merely
locates these proposed endpoints; only the exact rational inequalities
certify them. Strict comparisons leave both endpoints and everything
between them to the generic series.

On [2,3], q lies in [1,9/4]. Alternating tails after the first term give
P3 <= J0 <= P2, where P3=1-q+q*q/4-q*q*q/36 and
P2=(1-q/2)^2. P3'=-((q-3)^2+3)/12<0, so its minimum is
P3(9/4)=-77/256>-1/2. Also -1/8<=1-q/2<=1/2, so P2<=1/4.
Thus this entire interval lies strictly inside the zero rounding cell.

For x>0 the coefficient identities give
(J0^2+J1^2)'=-2*J1^2/x<=0. Exact alternating enclosures at three certify
J0(3)^2+J1(3)^2 < 0.182588 < 1/4. Consequently |J0(x)|<1/2
for x>=3. No asymptotic formula, floating-point threshold or external
special-function library is needed for this conclusion.

Therefore the admitted range returns one below L and zero above U,
with an unchanged generic fallback in the bracket. Zero-scale results have
scale zero, and the caller's input base must be restored on each return.
No additional public function or global variable is introduced.

## Verification and limits

`test-bessel-zero-proof.py` certifies the proposed endpoints, alternating
bounds and energy bound with exact fractions. CLI tests independently use
alternating enclosures around the endpoints, inside the fallback bracket,
and around the scope boundary, with signed arguments and caller bases
2/8/16. They exercise embedded and ordinary source loading. A large-argument
redefinition test requires all three public a/c/s calls to remain observable.
These checks pass against the accepted baseline before implementation.

Correctness, interpreted decision coverage, generated-source identity and
hosted native loader/platform checks remain required for the implementation.
Any elapsed study must be separately declared after all original PR #712
observations have been retained and reviewed, with its exposed cases kept
as controls and the approved regression/Howard/startup rules unchanged.
