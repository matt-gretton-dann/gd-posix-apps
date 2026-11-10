# Certified zero-scale Bessel classification

Refs #217 and #197. This experiment starts from accepted main `3beebb14`.
It is independent of the rejected finite Horner candidate in PR #712 and
of the function-storage experiment in PR #713. No elapsed improvement or
adoption is established. The total recorded goal-time cap remains 864,000
seconds; previous failed and inconclusive outcomes remain part of the record.

For normalized orders zero/one and entry scale zero, classify their nearest
integer using the certified half-crossing brackets below. Retain the accepted series
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
The portable source now implements those strict comparisons; the uncertain
bracket and every excluded regime continue through the accepted code.

Eight focused local CTests pass, including 628 certified instrumented results.
All ten outcomes of the five new decisions are observed; overall coverage is
203/214 observed, 212/214 observed or fault-modeled, with the same two
mathematically infeasible exponential outcomes. Eleven additional compact
outward enclosures are reproducible using the committed rational generator.
The new boundary/state and public-binding tests also pass with Howard bc
7.0.3 loading this exact source as ordinary input. These are functional
checks, with no local production elapsed benchmark or CPU sampling.
Hosted native loader/platform checks remain required for the implementation.
Any elapsed study must be separately declared after all original PR #712
observations have been retained and reviewed, with its exposed cases kept
as controls and the approved regression/Howard/startup rules unchanged.

## Correctness-only publication

The dedicated native workflow covers Linux, macOS and Windows with image,
source fallback, profile and native-function modes, supported threaded modes
and a Linux sanitizer build. It retains the tested executable bytes, generated
source/image data, selected public inputs, actual cache/compiler commands and
asserted test logs. The original uploaded archive is downloaded and checked
against its recorded hashes; missing or partial builds cannot pass retention.
Synthetic fixture tests cover missing/corrupt bytes and source/path identity.
This is a retention check, not a complete build-input closure or elapsed proof.
The three ordinary timing workflows exclude only this exact topic branch,
so the draft starts no replacement performance observations.

The first published head `83a0168c` retains all 44 original checks: 38
successful and six Windows failures, with 33 archives and seven decoded logs
reviewed. The only failing CTest is enclosure regeneration: checkout conversion
changes both the generator self-hash and raw JSON bytes. A test-first regression
reproduces all four generator/fixture LF/CRLF combinations (three failures).
The generator now records normalized source identity and compares normalized
fixture text; the eleven rational cases and their bounds are unchanged. Native
archives still retain and hash the actual physical bytes separately. This
correction requires its own hosted checks and establishes no elapsed benefit.

## Order-one cells

The order-one implementation uses two strict brackets:

L1 = 1.2067184630059078866565349810721299413472200414880472421508483780

U1 = 1.2067184630059078866565349810721299413472200414880472421508483781

L2 = 2.4881550851691471311579234121812087972033545580535080099003329207

U2 = 2.4881550851691471311579234121812087972033545580535080099003329208

Exact alternating enclosures put J1(L1)<1/2<J1(U1) and
J1(U2)<1/2<J1(L2). On 0<x<=3, differentiating the convergent series twice
and writing q=x*x/4 gives the alternating upper bound
J1''(x)/x <= P(q)=-3/8+5q/24-7q*q/192+q*q*q/320.
The omitted derivative ratios decrease and are at most 11/96 after the
fourth term for q<=9/4. P' has positive leading coefficient and negative
discriminant -23/9216, so P increases. Its maximum is
P(9/4)=-1131/20480<0. Thus J1 is strictly concave on this interval.
J1(0)=0, J1(1.5)>1/2 and 0<J1(3)<1/2 certify exactly two half crossings.

Concavity keeps J1 nonnegative on [0,3]. The alternating upper partial sum
x/2*(1-q/2+q*q/12) is below one on [0,2]. On [2,3] its bracket decreases
from 7/12, while x/2<=3/2, so the upper bound is at most 7/8.
The existing energy bound gives |J1|<1/2 beyond three. Consequently in the
series-only scope x<=8, J1 rounds to one strictly between U1 and L2 and
to zero below L1 or above U2. Both inclusive uncertain brackets retain the
accepted generic evaluation. Normalized odd-order parity determines the
sign of the nonzero return; zero is independent of parity.

Test-first certificates and independent CLI cases cover all four endpoints,
both uncertain interiors, adjacent decimal inputs, signs of both order and
argument, caller bases/state and the scope boundary. Additional exact
reference cases include the excluded order two. Large-argument binding tests
require the ordinary a/c/s calls for orders zero, one and minus one.

## Positive-scale magnitude guards: proof and tests first

The accepted ascending budget includes D=length(x), which counts the entire
fractional coefficient. That can make every recurrence operation much wider
than its output needs. For positive entry scale S, use
M=max(length(x)-scale(x),1) and T=S+G+M+1, where G=floor(7*x/16)+1.
The proposed W=S+G+M+2*d(T)+32 retains the same growth and quadratic guards.
At scale zero retain the old coefficient-length formula, preserving the
accepted arithmetic inside the certified-cell fallback brackets. The Hankel
selector and its precision formula remain unchanged.

This substitution does not weaken the accepted step-count argument. After
the tiny-result branches, n<max(3*x,4*(S+1))<7*(W+1). Also
x<16*W/7, so the same contracting-tail argument gives fewer than 8*(W+1)
ascending steps. None of those bounds requires the original fractional
coefficient length; M>=1 gives a positive T and W+1<=19*T as before.

For completeness, argument clipping can use a coarser series-derived bound
without depending on correct final rounding. The absolute Jn series is a
subset of exp(x/2)*exp(x/2), so |Jn(x)|<=exp(x) for nonnegative x.
Termwise coefficient identities give J0'=-J1 and, for n>=1,
Jn'=(J(n-1)-J(n+1))/2: the two coefficient numerators add as
(n+k)+k=n+2*k. Thus |Jn'|<=exp(x). Clipping downward by less than
u=10^-W costs at most exp(x)*u; recurrence arithmetic at the clipped input
has the accepted bound 10^6*(W+1)^2*exp(x)*u. Their sum is below
2*10^6*(W+1)^2*exp(x)*u.

The exact inequalities e<87/32 and 87^16<10^7*32^16 imply exp(x)<10^G.
With T<10^d(T), M>=1 and W+1<=19*T, the pre-rounding error in output
units is below 2*19^2*10^-27<1/2. Final rounding adds at most half an output
unit. This proves the same one-decimal-ULP contract for the proposed budget;
correct rounding of arbitrary near-midpoint positive-scale inputs is not
claimed. Working-scale readback and ordinary resource errors remain required.

The test-first rational model truncates both multiplication and division at
each source operation, accounts for clipped inputs and squared coefficients,
checks the step bounds, and compares against independent exact alternating
enclosures. Twenty-two cases include 256-place inputs at S=1/20/100,
leading fractional zeros and a large-order cancellation case. CLI tests check
both signs, caller bases, scale and unchanged caller values without creating
an extra user alias to the argument. Reproducible dense-input reference cases
are added before the production formula changes. Reference values are reused
across scales in the test generator; every exact enclosure remains identical.
Expanded exact tests use bounded deadlines appropriate to those larger vectors;
no benchmark acceptance threshold changes.
