# Portable bc math library candidate

Refs #217, #13, #200, #216 and #197. `src/bc/math-library.bc` is ordinary
POSIX bc source. CMake embeds its normalized text mechanically; the driver
loads it through the existing parser and VM before user files and stdin.
There are no math opcodes, callbacks, function-name interception, privileged
precision registers or hidden caches. The identical file can be supplied as
input to another conforming bc without that implementation's `-l` library.

This is a correctness candidate. Performance acceptance and the complete
competitive matrix remain outstanding. Imported-program adapters and
interpreted decision evidence are described below. Passing finite vectors
alone is not the accuracy argument.

## Interface, state and namespace

The chosen language contract is POSIX.1-2017, Issue 7 (2018 edition), checked
against the supplied standards archive. `-l` initializes scale to 20. The six
functions are sine `s`, cosine `c`, arctangent `a`, natural logarithm `l`,
exponential `e` and integer-order Bessel `j`. Angles are radians. Result scale
and the scale register on return equal the caller's entry scale. Input base is
saved and restored; output base is untouched. Every work value is an automatic
variable or parameter. Actual argument values and user scalar bindings survive.

Only these six function names are defined. Calls between them are ordinary
visible bc bindings: `s`/`c` use `a` when reducing arguments larger than three radians, and the
large-argument `j` path uses
`a`/`s`/`c`. Redefining a public dependency therefore has normal program effects;
there is no private function namespace or retained implementation that bypasses
redefinition. There are no additional reserved helper names or global caches.
The accuracy argument below assumes the supplied definitions are in place.

The ordinary source initializes decimal scale with `2*A`, so loading it after
a caller's nondecimal-base file still selects 20 and preserves both bases.
Each function sets `ibase=A` before evaluating multi-digit constants. A single
`A` has its base-independent value ten. Saving/restoring the caller's base uses
values, not literals interpreted in the caller's base. Tests cover input/output
bases 2, 8 and 16, nested calls, parameter mutation and user redefinition.

The logarithm requires a positive argument and Bessel's order must be an
integer; POSIX leaves calls outside the mathematical domain undefined. The
source guards nonpositive log inputs to avoid nontermination, and normalizes
integer order values using scale-zero division. No accuracy or portable error
result is promised for noninteger orders or other out-of-domain calls. These
choices are not presented as POSIX-mandated truncation or diagnostics.

## Precision convention and elementary rounding

At entry scale S, the required absolute error is at most U = 10^-S. This is the
stronger project contract from #217, not a claim that POSIX requires correct
rounding. Internal operations use a magnitude-dependent working scale W;
write u = 10^-W. Ordinary decimal division and multiplication that truncate
have absolute error below u when their result scale is at least W. Operations
retaining more operand precision have a smaller bound. Addition/subtraction
of these stored coefficients is exact at their selected scale. Builtin sqrt
returns an exact decimal floor with error below u at precision at least W.

The dynamic algorithms add or subtract U/2 at working precision, then divide
by one at entry scale. This rounds the guarded approximation, with ties away
from zero. If its pre-rounding error is below U/2, final error is below U. Correct rounding
of every possible near-midpoint input is not claimed. Rounding avoids placing
ordinary results just beside a one-ULP certification boundary; certified oracle
intervals still decide acceptance independently. The bounded unit-arctangent
path uses the same final rounding step and an independently certified literal.

Let D = length(x) and F = scale(x), before changing x. D is at least one
and can be smaller than F for numbers with leading fractional zeros.
Let d(T) be the decimal digit count of a positive integer T. The formulas below use nonnegative integer quantities computed at
scale zero. For T >= 1, W <= 27T for the 24-digit formulas (and <= 35T for the
32-digit formulas). Thus the two extra d(T) terms dominate quadratic working-
precision factors: T < 10^d(T). The remaining 24 or 32 digits leave a large
explicit margin for the conservative constants below. They are not a fixed
S+10 accuracy assumption.

## Arctangent

For magnitudes in [0.8, 1.25] other than one, use
atan(x) = atan(1) + atan((x-1)/(x+1)). The transformed magnitude is at
most 1/9, reducing the work before the alternating series. Apply the
original sign after the correction. The transform and correction use the
general working precision. Through W = 144, use the certified lower bound
below, truncated at W. The enclosure and literal have the same floor at W,
so the correction error is less than one working ULP. Above that limit use
the recursive unit evaluation. All other magnitudes retain the existing
reciprocal and half-angle paths. Exact rational enclosures cover both
signs and inputs at and around the two reduction thresholds.

For exact |x| = 1 and entry S <= 144, use an ordinary 160-decimal-place lower
bound. Tests enclose atan(1) with exact rational alternating remainders using
atan(1) = 4 atan(1/5) - atan(1/239). Both enclosure endpoints yield the same
integer floor after multiplication by 10^160, and the stored literal has error
strictly below 10^-160. At working scale S+1, add the exact half decimal unit and
truncate at S. The final error is at most U/2 + 10^-160 < U. Negating before
truncation preserves nearest rounding for -1. Tests check the literal and both
reference endpoints round identically at every entry scale through 144.

At the POSIX default S = 20, return the independently certified rounded
literal directly, applying the original sign. The enclosure test certifies
the literal at that scale. Other bounded unit inputs use a working scale
of at most 145. All other inputs and S >= 145
retain the dynamic calculation and its working-scale readback guard. Literal identity, threshold neighbors, representations
and caller bases are checked independently. No retained state or VM recognition
is introduced.

Use odd symmetry, and for |x| > 1 use pi/2 - atan(1/|x|). Compute pi/2 as
2*a(1) at working precision; the recursive argument one needs no reciprocal,
so this is a finite dependency. Repeated half-angle reduction
`x/(1+sqrt(1+x*x))` takes arguments at most one into [0,.1] in at most three
steps. The reconstruction multiplier is at most eight. Each transform is
well-conditioned on this interval; reciprocal input has derivative at most
one. Input is rounded to W before the series.

Evaluate atan(t) with its alternating odd-power recurrence. The stored squared
ratio is below .011. Term propagation contracts; each multiplication/division
introduces less than u, and summation is exact. The loop takes fewer than W+2
terms. Allowing one additional working ULP for the truncated near-one constant,
a conservative pre-rounding bound is 301*(N+1)*u. This includes transforms,
constant reconstruction and N terms. Use W = S + 2*d(S+F+D+1) + 24. The bound
is far below U/2 by the digit-count inequality above. Term disappearance is a
termination event under this contraction bound, not the accuracy justification.

### Low-scale rational candidate (#639)

The separate candidate evaluates `x*P(z)/Q(z)`, `z=x*x`, with positive
degree-six integer polynomials. Dividing their coefficients by `Q(0)` gives
a [6/6] Padé approximation to `atan(sqrt(z))/sqrt(z)`: the exact convolution
with `sum((-z)^j/(2*j+1))` matches through degree twelve. This is a proposed
ordinary-source kernel for entry scales through 20; it does not replace the
higher-scale series, public function binding, reductions or final rounding.

The reduced stored argument has magnitude at most 1/9. Positive half-angle
steps at least halve an argument at most one, so the conservative reconstruction
bound is 16. Put `h=1/81` and `q_j=Q_j/Q_0`. Because `Q(z)/Q_0>=1`, the absolute
approximation error after reconstruction is at most

`(16/9)*h^13/(1-h)*sum(q_j/(2*(13-j)+1), j=0..6) < 1.284e-25`.

All coefficients are positive. Both exact and finite-decimal Horner
denominators retain their positive constant `Q_0`. Each Horner error is less
than `u/(1-h)` for working ULP `u=10^-W`. Squaring the argument adds less than
`u` to its squared-value error. Positive coefficient sums and their derivative
sums at one bound the resulting rational sensitivity; including both Horners,
the numerator multiply and the final division gives a conservative kernel
arithmetic bound below `1024u`, before reconstruction. No floating-point
estimate or sampled agreement establishes these uniform bounds.

Retaining the existing reduction/constant bound conservatively, the candidate's
pre-rounding budget is `301*(W+3)*u + 16*1024u + 1.284e-25`. For every entry
scale 0 through 20 and `W>=S+26`, this is below half an output ULP. The expression
`(W+3)*10^-W` decreases over that domain, so its smallest W is the worst case.
Exact-Fraction tests check coefficient cancellation, the full interval budget
and a separate truncating decimal model against alternating-series enclosures.
Full ordinary-source integration, certified end-to-end cases, fallback/state
tests, interpreted decision coverage and hosted performance remain required.

## Exponential

Use x = |argument|, halve until the reduced argument is at most .125, evaluate
the positive Taylor recurrence, then square the result for every halving.
For negative arguments, take the reciprocal of the reduced result before
squaring. Its stored and exact magnitudes stay at most one. The series contracts by at least .125
per step after the first; fewer than 2W+2 terms suffice. Per-term truncation
propagates geometrically. Reconstruction and argument-reduction errors are
bounded conservatively by `2^k * exp(x) * (3N+6) * u` before taking a reciprocal.
For x > .125, 2^k < 16x; otherwise k=0.

Let T = S + F + floor(x) + 1. For positive arguments use
W = S + G + 2*d(T) + 24, where G = floor(floor(x)/2) + 1. Since e < 3,
e^2 < 10, and G > x/2, we have exp(x)*10^-G < 1. The remaining
reconstruction factor is at most 16T. With N < 2W+2 and W+1 <= 29T,
the two digit-count guards and 24 extra places keep the error below U/2.

For negative arguments use W = S + 2*d(T) + 24. Before inversion, the reduced Taylor error and
argument-reduction error total at most (3N+6)u: halving truncations sum
to less than 2u at the reduced argument, and exp on [0,.125] has derivative
below two. Both reduced exponential values are at least one, so inversion
does not amplify that error and adds at most u. The squaring map on [0,1]
is 2-Lipschitz, giving total error at most 2^k*(3N+8)u after squaring.
Here 2^k is at most 16T, N < 2W+2, and W+1 <= 29T. As T < 10^d(T),
the two digit-count guards and 24 extra places keep the error below U/2,
even if a stored value underflows to zero. Final rounding adds at most
another U/2.

For a negative argument with x >= 3*(S+1), return zero directly.
`exp(-3*(S+1)) < 10^-S` certifies that branch, independent of a vanishing term.
It also avoids unnecessary reduction and squaring for very large magnitudes.

## Logarithm

Repeated square roots put a positive x in [.8,1.2], recording the power-of-two
reconstruction multiplier. Evaluate `2*atanh((x-1)/(x+1))` using its odd-power
series. At requested scale 1000 and above, reduce further to [.95,1.05]
to trade additional roots for fewer full-precision series terms. In both
regimes the reduced ratio has magnitude at most 1/9 and its square is
below .013, so fewer than W+2 terms suffice. Near one, this evaluates the small
logarithm directly rather than subtracting large independently rounded logs.

At working scale W <= 64, an argument in [1.6,2.4] can enter the
root-free band by dividing by two and adding ln(2) to the final result.
The quotient truncation changes its logarithm by less than 2u since the
reduced argument is at least .8. The stored 80-place lower bound is
certified by the positive series 2*atanh(1/3), bounding the tail by the
first omitted term times 9/8. Truncation of this constant at W adds less
than u. The combined error is covered by the existing budget below.
Other arguments and precisions retain square-root reduction.

At scale 1000 and above, let t be the reduced ratio and q = t².
Cache q⁰ through q¹⁵ and evaluate blocks of 16 consecutive terms.
For a block starting at term j, multiply the sum of q^v/(2j+2v+1)
for v = 0 through 15 by t*q^j. Advance that signed power using q¹⁶.
This reduces full-precision multiplications inside the series. The
power recurrences propagate prior rounding error with a factor below
.013. Each cached power differs from the exact power of the rounded q
by less than 2u. Each block has at most 16 truncated divisions, so
its polynomial error is below 48u. Including error in the signed
leading power and the final product, each block contributes less than
100u of error. When the computed leading power reaches zero, the
omitted exact leading power is within a few u, and the tail is
geometrically bounded. The existing logarithm error budgets below
cover these errors and the initial rounding of t and q.

For positive x, its signed decimal exponent is m = D - F - 1. If
abs(m) is at least 64 + floor(S/2), normalize to [1,10) using the exact
integer power 10^abs(m). The cutoff avoids paying for the additional
logarithm when ordinary root reduction is cheaper at the requested scale.
For negative m, integer multiplication preserves the fractional operand
scale, then division by one truncates to the working scale. For positive m,
division by the integer power truncates directly. The normalized argument
stays at least one, where the derivative of log is at most one. Its
truncation therefore adds less than one working unit of error.

Let T = S + F + D + 1. Use W = S + 2*d(T) + 24 on the normalized path.
The square-root reduction of an argument in [1,10) needs at most six
roots, including at high precision. Each root truncation is below u and later roots on [1,10)
contract the perturbation. Add m*l(10) evaluated at scale W after
reconstruction. The recursive l(10) uses the ordinary path because its
decimal exponent is one. Its returned error is at most u, so this term
adds at most abs(m)*u. The series and reduction cost at most
10^6*(W+10)*u with the bounded root count. These bounds leave less than
U/2 before final rounding.

The ordinary path uses W = S + F + D + 2*d(T) + 24 and retains the original
input precision through reduction. When x is tiny, the first exact root
is at least 10^(-F/2). Its truncated value is at least half that bound
because W includes F. Subsequent roots of small arguments stay above that
same lower bound. Taking the logarithm of each truncated root therefore
introduces at most 2*10^(F/2)*u error. The reconstruction multiplier is at
most 128*(D+F+1), using the tighter reduction interval for the bound. The geometric sum of intermediate multipliers is below
twice the final multiplier, so the root errors and the bounded atanh series
together cost at most 10^7*(W+10)*(D+F+1)*10^F*u. The two digit-count guards
cover the quadratic factor even when F exceeds D. The remaining margin
keeps the pre-rounding error below U/2, including near one.

## Sine and cosine

Let M = max(D-F,1) and T = S+M+F+1. Use
W = S + M + 2*d(T) + 24. Since abs(x) < 10^M, the integer magnitude,
rather than the stored fractional width, determines the reduction budget.
If F>W, truncate the input once to W; this changes its exact value by less
than u. Sine and cosine are globally one-Lipschitz, so the resulting
function error is below u. Shorter exact inputs retain their original scale.

After sign normalization, arguments at most three radians need no period
construction because three is below pi. For
larger arguments, compute the period as 8*a(1) at W using the ordinary public
binding. There is no fixed period literal. Remove an integer multiple at
scale zero, then center the reduced argument. An adjacent quotient at a
reduction boundary changes only the period representative. The period and
phase error is bounded by 16*10^M*u, including that adjacent quotient.
Working-scale readback still protects both the initial and reduction setters.

Evaluate sine at one fifth of the centered angle and cosine at one
eighth. The finite decimal factors incur at most one working ULP u of
truncation, and the scaled arguments have magnitude below one. The
alternating recurrences retain the conservative bound
4000*(W+10)*u for series and arithmetic error. Include one additional u
for the scaling error using the one-Lipschitz bound.

Reconstruct sine with y*(5 + y²*(16*y² - 20)). Reconstruct cosine by
applying 2*y² - 1 three times. Each exact intermediate cosine lies in
[-1,1]. On [-1.01,1.01] the sine polynomial derivative is bounded in
magnitude by 160, and the cosine map derivative by 5. Each exact
reconstruction step can therefore amplify the input approximation by
at most these factors. In the sine polynomial, the square's truncation
is amplified by less than 60 and the final two products contribute
less than 4u. For cosine each application adds less than 2u.
Together the reconstruction and scaled series errors are bounded by
10^6*(W+10)*u. This bound also keeps all reconstructed cosine
intermediates within the derivative interval.

Here W<=27T. Dividing the aggregate bound, including input coercion and
period reduction, by U gives at most
`[16*10^M + 10^6*(27T+10) + 1] * 10^(-M-2*d(T)-24)`.
Since M>=1, T>=2 and T<10^d(T), this is below 10^-18. The final rounding
step adds at most U/2, preserving the one-decimal-ULP contract for arbitrary
supported magnitudes and fractional widths. Odd/even symmetry handles
negative input exactly. The certified +/-3 boundary fixtures and equivalent
long-fraction representations exercise both sides of the new decisions.

## Bessel J

Normalize valid integer orders at scale zero. Use
`J(-n,x)=(-1)^n*J(n,x)` and `J(n,-x)=(-1)^n*J(n,x)` to reduce to n,x >= 0.

First, if n >= 3x and n >= 4*(S+1), return zero. The ascending absolute-series
bound and `n! >= (n/e)^n` give
`abs(J_n(x)) <= (e*x/(2*n))^n * exp(x*x/(4*(n+1)))`.
Under these conditions this is at most `(e/6)^n * exp(n/36)`, below 10^-S.
This branch avoids unbounded work on a huge order whose result is certified tiny.

After that fast path, an additional regime returns scaled zero for positive
integer n with n >= 16x and n >= S. Test
n >= S before the extra ratio comparison, avoiding that work when high
requested precision already excludes this regime.
The ascending absolute-series bound and factorial estimate above give
`abs(J_n(x)) <= (e*x/(2*n))^n * exp(x*x/(4*(n+1)))`.
Here `x*x/(4*(n+1)) < n/1024` and `e*x/(2*n) < 3/32`.
Since `exp(1/1024) <= 1024/1023`, the bound is below
`(96/1023)^n < 10^-n <= 10^-S`. This is an absolute error certificate,
independent of a vanishing coefficient. For x=0 and positive n the result
is exactly zero; n=0 is excluded, preserving J_0(0)=1. Sign normalization
precedes the test, and zero is independent of the recorded parity. The
ordinary decimal product 16*x is exact because multiplication retains x's
fractional scale. No transcendental bound calculation runs in the interpreter.

For x > max(8*(n*n+1), W), attempt the real large-argument expansion from
[NIST DLMF 10.17](https://dlmf.nist.gov/10.17). Coefficients satisfy
`b_0=1; b_k=b_(k-1)*(4*n*n-(2*k-1)^2)/(8*k*x)`.
Split their alternating even/odd sums and reconstruct with phase
`x-(n/2+1/4)*pi` and amplitude `sqrt(2/(pi*x))`.
DLMF 10.17.14-15 bounds the Hankel remainder by
`2*abs(b_k)*exp(abs(n*n-1/4)/x)`, less than `3*abs(b_k)` in this regime.
Taking the real part preserves that bound.

Use W = S + D + F + 2*d(S+D+F+length(n)+1) + 32. The W cutoff avoids
building a long asymptotic prefix when working precision approaches the
argument. It only selects a series. Before each Hankel coefficient,
check its exact recurrence ratio against one half. If contraction is lost
before the budget is met, abandon this regime and use the ascending series.
While contracted, truncation propagation is below 2u. When a stored next
coefficient vanishes its exact magnitude is below 2u, giving remainder below
6u (amplitude is below one). Sum and reconstruction errors are polynomial in
W; phase precision includes D and covers multiplication by the order. A
conservative aggregate bound `10^6*(W+1)^2*10^D*u` remains below U/2. No
unbounded divergent series is treated as convergent merely because a term grew.

Otherwise, evaluate the ascending recurrence, including the initial
`(x/2)^n/n!`, with G = floor(7*x/16) + 1 and
W = S + G + D + 2*d(S+G+D+1) + 32. Truncate the argument only if it has more
than W fractional places. Differentiating the real integer-order
[Bessel integral](https://dlmf.nist.gov/10.9.E2) gives `|J_n'(x)| <= 1`,
so truncating the argument adds less than one working unit of error.
The extra G covers cancellation and propagation through the absolute
recurrence. Write a = x/2. An error introduced at initial coefficient j
reaches ascending term k with absolute weight at most
`a^(n-j+2*k)/(k!*(n-j+k)!)`. The bound follows by replacing the product
of integers from j+1 through n+k by (n-j+k)!. Summing over k gives a
subset of the nonnegative terms of `exp(a)*exp(a) = exp(x)`. An error
introduced within the ascending loop propagates across k more steps with
weight at most `a^(2*k)/(k!)^2`, bounded in sum by the same product.

The squared argument is multiplied by the exact decimal factor .25.
This preserves a short coefficient when the product is exact at fewer
than W places, so later multiplications need fewer limbs. Both products
truncate at W only when their exact result needs more places. The stored
magnitude of q is at most Q = a^2 and differs from it by at most 2u. To bound this perturbation, differentiate the absolute series
in Q, holding the exact initial coefficient `a^n/n!` fixed. For term k
the factor k cancels against k!, and dropping the last factor of (n+k)!
bounds the derivative sum by
`sum_(h>=0) a^(n+2*h)/(h!*(n+h)!) <= exp(x)`, with h = k-1.
Thus the q perturbation also costs at most 2u*exp(x). Each recurrence
step adds at most two working units of local truncation error.
The [ascending definition](https://dlmf.nist.gov/10.2.E2) gives these
absolute weights independently of the signs or cancellation in the result.

After the tiny-result branches the initial factor takes at most
7*(W+1) steps. After k exceeds x, term ratios are at most one quarter.
Each stored term is bounded by its exact absolute term, which is at most
exp(x). Since x < 16*W/7, the term loop takes fewer than 8*(W+1) steps
before truncation makes its term zero.
The tail and all truncation propagation are bounded by
`10^6*(W+1)^2*exp(x)*u`. The exponential series gives
`e < 87/32`, and `87^16 < 10^7 * 32^16`. Thus
`exp(x)*10^(-G) < 1` for the nonnegative argument. The remaining
guards keep the total error below half an output unit. With
`T = S + G + D + 1 >= 2`, `W+1 <= 19*T` and `T < 10^d(T)`.
The bound in output units is below `19^2*10^-25`, leaving room for the
argument truncation error. Rounding to the requested scale adds at
most half an output unit, so the total is below one decimal ULP.
An underflowed initial factor or term is therefore
covered by the error budget even if later exact recurrence terms would grow.
The forward recurrence reuses terms; it does not rebuild powers/factorials.

## Resource limits and validation scope

Working precision is a value in ordinary bc. Requests exceeding GD's
supported scale/number limits fail through its normal arithmetic/assignment
checks. Every working-scale assignment also checks readback for hosts that
silently clamp requests; mismatch raises an ordinary division-by-zero error
before any uncertified result is returned. This must be an execution-time
error: POSIX quit acts while reading and would stop library preload even
inside a conditional function body. Portable test-side source rewriting
exercises the scale fallback without a production VM hook. Very large
turning-region Bessel inputs may require prohibitive ascending-series guards;
the implementation has no hidden native fallback. General allocation-limit
handling remains #186; physical resource exhaustion is not a numerical result.

The committed certified references use exact decimal arguments and independent
FLINT/Arb enclosures. Tests require both endpoint errors <= 10^-S and distinguish
an indeterminate enclosure from a proven numerical failure. Scales include
0,1,2,20,100,1000,2000, with zero/one cases, signs, near-one logs, tiny/huge
arguments, trig reduction boundaries and several Bessel regimes. Finite tests
support the arguments above. A separate crossover fixture adds 28 regimes at
five scales (140 enclosures), on both sides of the reciprocal, half-angle,
halving, log-reduction and Hankel entry boundaries, plus negative-order Hankel
symmetry and an underflowing initial Bessel coefficient. These join the original
336 enclosures; a separate trigonometric fixture adds 60 enclosures around
positive and negative three radians. A Bessel zero-bound fixture adds 80
enclosures across ratio and scale boundaries, signs and the complementary old
bound. All 616 are independently certified; regeneration uses the same pinned
optional FLINT environment.

## Imported programs and interpreted decisions

`bc/math-imported` runs the imported Howard exponent, log, pi, arctangent,
sine, cosine and Bessel programs, generated atan calls, and the ordinary
function-replacement script. It redirects only their mathematical call
sites to ordinary test-side wrappers. Production definitions and their internal
calls remain unchanged. The wrappers record each actual argument, entry scale
and result; the original expressions and loops still execute. All 1581 calls
are accounted for: 1073 certified numerical results, 507 undefined-domain
inputs (eleven nonpositive logarithms and 496 noninteger Bessel orders), and
one ordinary user replacement of `e` by the identity function. Undefined-domain
calls and the replacement are not counted as numerical accuracy evidence.

The committed outward enclosures certify exact reference arguments. Computed
trigonometric inputs use the imported program's own approximation to pi;
the adapter widens the enclosure by the exact argument difference using
the global one-Lipschitz bounds for sine, cosine and arctangent. It never
assumes a particular final digit of pi or uses a binary floating-point
tolerance. Other inputs must agree exactly. Normalized source hashes and
call counts prevent silently replacing or dropping imported cases.

The legacy exact-output GDPAT entries remain excluded because their expected
text encodes a different library's rounding. Their mathematical calls are now
exercised by the numerical adapter. Bessel and atan generation uses the
imported scripts with pinned Howard bc; their committed generated input and
generator hashes are checked. The generated Howard `halt` terminator is
removed in favor of harness EOF. The identity replacement's bare `return x`
is parenthesized as `return(x)` for strict POSIX mode; its original source hash
is still checked. The definition retains ordinary binding, so the next wrapper
call resolves the user replacement in both extension configurations.

`bc/math-library-decisions` transforms only test-side source. Each original
condition is evaluated once. Automatic array flags distinguish loop exhaustion
from `break`; `return` skips unreachable probes. A behavioral check covers
skipped/exhausted/broken loops, return, nested decisions and a condition with a
side effect before the production fixture runs. It certifies the numerical
results from the instrumented definitions against the same exact enclosures.
Calls outside the logarithm domain exercise its guard and are counted
separately from the numerical results.

The [decision fixture](../../src/bc/fixtures/math-library-decisions.json)
records the exact source digest, counts, and per-condition outcomes. A
portable-host model independently clips each working-scale setter to exercise
its defensive error branch. Two outcomes require a negative exponential
coefficient and are infeasible because the series and reconstruction preserve
nonnegativity. The automated check compares the report with the fixture and
requires every outcome to be observed, modeled, or explicitly accounted for.
This interpreted-library evidence is separate from compiler line, branch, and
MC/DC coverage for the C++ build in each extension configuration.

The first [correctness and coverage archive](bc-217-library-validation/README.md)
retains the exact interpreted source, validation logs, separate LLVM 23 ON/OFF
profiles/reports and the existing arithmetic native control. Its measured
revisions are explicit checkpoints; later source changes require fresh checks.

## Build-time math library image

`ENABLE_BC_PREPARSED_LIBRARY=ON` builds a native generator that parses the
same embedded POSIX source and encodes the ordinary instruction operands
in a compact, build-local byte image.
The loader executes initialization and other top-level items in source order.
It registers an isolated function definition as a slice of the immutable image.
The first call decodes and verifies that slice before installing an owned body
with its own literal caches. User definitions replace deferred or decoded
functions through normal registration. Item boundaries and source locations
are preserved. Native builds outside Windows enable the image by default.
Windows defaults to the source loader after the all-native default
regressed sparse-array execution in the native benchmark. Windows builds
can opt in explicitly. Cross builds always load the source through the
parser, and native coverage builds default to the source loader. Any native
build can select source with `-DENABLE_BC_PREPARSED_LIBRARY=OFF`. The image
test checks deterministic regeneration when the image is enabled.

An exploratory Apple Clang arm64 Release comparison used 30 paired process
launches per case after warmup. All outputs matched the source loader.
The preload-only runtime ratio was 0.967 [0.956, 0.975] and the cold sine
and Bessel ratios were 0.972 and 0.975. The no-library ratio was 0.986,
and this run had no separate A/A control. The executable grew from
567232 to 735296 bytes. These initial measurements preceded the compact encoding and deferred
function decoding.

The compact encoding reduced the executable to 583296 bytes and the
instruction image to 10089 bytes. In a second 30-pair comparison with both
image implementations and the source loader, the compact preload ratio was
0.962 [0.957, 0.970]. The generated-statement ratio was 0.960 [0.955, 0.969].
The compact no-library ratio was 0.988 [0.978, 1.008]. Both image builds
matched the source loader's output on the timed cases. Both language modes
also passed the math CLI checks and regenerated the compact image exactly.
