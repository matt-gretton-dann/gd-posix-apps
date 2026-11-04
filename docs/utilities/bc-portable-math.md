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

Let D = length(x) and F = scale(x), before changing x. In the supported decimal
model D >= F and D >= 1. Let d(T) be the decimal digit count of a positive
integer T. The formulas below use nonnegative integer quantities computed at
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
general working precision, and all other magnitudes retain the existing
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

The shortcut uses a working scale of at most 145. All other inputs and S >= 145
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
terms. A conservative pre-rounding bound is 300*(N+1)*u, including transforms,
constant reconstruction and N terms. Use W = S + 2*d(S+F+D+1) + 24. The bound
is far below U/2 by the digit-count inequality above. Term disappearance is a
termination event under this contraction bound, not the accuracy justification.

## Exponential

Use x = |argument|, halve until the reduced argument is at most .125, evaluate
the positive Taylor recurrence, then square the result for every halving.
Negative arguments use the reciprocal. The series contracts by at least .125
per step after the first; fewer than 2W+2 terms suffice. Per-term truncation
propagates geometrically. Reconstruction and argument-reduction errors are
bounded conservatively by `2^k * exp(x) * (3N+6) * u` before taking a reciprocal.
For x > .125, 2^k < 16x; otherwise k=0.

Use W = S + floor(x) + 2*d(S+F+floor(x)+1) + 24. The reconstruction factor is
controlled because `(1+x)*exp(x)*10^-floor(x) < 6`: split x into integer and
fractional parts and use exp(1) < 3. Consequently `2^k*exp(x)*10^-floor(x)`
is below 128. Taking the negative-argument reciprocal multiplies the small
error by at most four and adds one division unit. These bounds, with the
quadratic digit-count margin, remain below U/2.

For a negative argument with x >= 3*(S+1), return zero directly.
`exp(-3*(S+1)) < 10^-S` certifies that branch, independent of a vanishing term.
It also avoids constructing an enormous positive exponential just to invert it.

## Logarithm

Repeated square roots put a positive x in [.8,1.2], recording the power-of-two
reconstruction multiplier. Evaluate `2*atanh((x-1)/(x+1))` using its odd-power
series. The reduced magnitude is at most 1/9 and the squared ratio is below
.013, so fewer than W+2 terms suffice. Near one, this evaluates the small
logarithm directly rather than subtracting large independently rounded logs.

Use W = S + F + D + 2*d(S+F+D+1) + 24. The extra F+D protects tiny original
arguments during square-root reduction. The reconstruction multiplier is at
most 32*(D+F+1). Bounding the chain of root perturbations and the final series
conservatively gives `10^6*(W+10)*(D+F+1)^2*10^F*u`. Since F <= D,
`(D+F+1)^2*10^-D <= 1` for D >= 1. The remaining digit-count/24-digit margin
puts the bound below U/2, including cancellation near one. Original input
precision is retained through the reductions; a tiny value is not rounded to
zero before taking roots.

## Sine and cosine

Let M = max(D-F,1) and T = S+M+F+1. Use
W = S + M + 2*d(T) + 24. Since abs(x) < 10^M, the integer magnitude,
rather than the stored fractional width, determines the reduction budget.
If F>W, truncate the input once to W; this changes its exact value by less
than u. Sine and cosine are globally one-Lipschitz, so the resulting
function error is below u. Shorter exact inputs retain their original scale.

After sign normalization, arguments at most three radians need no period
construction: three is below pi and already within the series radius. For
larger arguments, compute the period as 8*a(1) at W using the ordinary public
binding. There is no fixed period literal. Remove an integer multiple at
scale zero, then center the reduced argument. An adjacent quotient at a
reduction boundary changes only the period representative. The period and
phase error is bounded by 16*10^M*u, including that adjacent quotient.
Working-scale readback still protects both the initial and reduction setters.

Use alternating term recurrences with the squared reduced argument and
successive factorial factors. Its magnitude is below four. After the first
few terms, recurrence ratios are at most one half; fewer than 4W+32 terms
suffice. The absolute-series bound exp(4) < 100 bounds early amplification
and cancellation. Conservative series/rounding error is below
4000*(W+10)*u. Include the input-coercion error as well as the phase error.

Here W<=27T. Dividing the aggregate bound by U gives at most
`[16*10^M + 4000*(27T+10) + 1] * 10^(-M-2*d(T)-24)`.
Since M>=1, T>=2 and T<10^d(T), this is below 10^-20. The final rounding
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
`(x/2)^n/n!`, with G = floor(7*x/8) + 1 and
W = S + G + D + 2*d(S+G+D+1) + 32. Truncate the argument only if it has more
than W fractional places. Differentiating the real integer-order
[Bessel integral](https://dlmf.nist.gov/10.9.E2) gives `|J_n'(x)| <= 1`,
so truncating the argument adds less than one working unit of error.
The extra G covers cancellation and
propagation through the absolute series, bounded by exp(2x). After the tiny-
result branch, the initial factor needs at most 7*(W+1) steps; the term loop
needs at most 8*(W+1). The [ascending definition](https://dlmf.nist.gov/10.2.E2) and the
[integer-order modified-Bessel integral](https://dlmf.nist.gov/10.32.E3)
bound absolute series weights by exp(x). After k exceeds x, term ratios are
at most one quarter. Absolute tail/rounding propagation is bounded by
`10^6*(W+1)^2*exp(2x)*u`. The exponential series gives
`e < 87/32`, and `87^16 < 10^7 * 32^16`. Thus
`exp(2x)*10^(-G) < 1` for the nonnegative argument. The remaining
guards keep the total error below half an output unit. With
`T = S + G + D + 1 >= 2`, `W+1 <= 19*T` and `T < 10^d(T)`.
The bound in output units is below `19^2*10^-25`, leaving room for
the argument truncation error. Rounding to the requested scale adds at
most half an output unit, so the total is below one decimal ULP.
An underflowed initial factor or term is therefore
covered by the error budget even if later exact recurrence terms would grow.
The forward recurrence reuses terms; it does not rebuild powers/factorials.

The ascending path groups four terms for scale-zero arguments of magnitude
at least 32. Let `q=-x^2` and let `d,f,g,h` be the four consecutive
denominators `4*k*(n+k)`. Given the previous term `t`, the block adds
`t*q*(f*g*h+q*(g*h+q*(h+q)))/(d*f*g*h)` and advances the term to
`t*q^4/(d*f*g*h)`. All coefficients are integers, and multiplication
of `t` by an integer preserves its working scale. The two divisions
therefore introduce at most two working units of error per block.
Each term error propagates through a suffix of the same absolute series,
whose sum is at most `exp(x)` because later denominators are no smaller.
If the final term rounds to zero, the omitted suffix has that same bound
times the accumulated term error. The existing aggregate error budget
also covers the three extra positions of a final block. Small or
fractional-scale arguments use the single-term recurrence.

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
side effect before the production fixture runs. It certifies all 616 numerical
results from the instrumented definitions against the same exact enclosures.

The source contains 78 decisions and 156 outcomes. The expected fixture requires
145 normal outcomes, including two explicitly undefined-domain log guard calls. Nine
separate portable-host models independently clamp a working-scale setter and
exercise its defensive error branch without returning a numerical value.
Together the ordinary and modeled runs must reproduce 154 outcomes. The two
remaining outcomes require a
negative exponential coefficient: positive Taylor terms, positive squaring
and a positive reciprocal preserve nonnegativity, so they are infeasible.
The committed `src/bc/fixtures/math-library-decisions.json` report records
each source line, expression, normal and
modeled observations, and this exception. Any other missing outcome fails
the automated check. This is interpreted-library decision evidence, kept
separate from C++ line/branch/compiler MC/DC reports and their ON/OFF totals.

The first [correctness and coverage archive](bc-217-library-validation/README.md)
retains the exact interpreted source, validation logs, separate LLVM 23 ON/OFF
profiles/reports and the existing arithmetic native control. Its measured
revisions are explicit checkpoints; later source changes require fresh checks.
