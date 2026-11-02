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
visible bc bindings: `s`/`c` use `a`, and the large-argument `j` path uses
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

For exact |x| = 1 and entry S <= 64, use an ordinary 80-decimal-place lower
bound. The committed a-one/S100 certified enclosure has identical floors after
multiplication by 10^80 at both endpoints. The stored literal is exactly that
integer divided by 10^80, so its error is strictly below 10^-80. Add the exact
half decimal unit, then truncate at S as in the dynamic path. Since S <= 64,
the final error is at most U/2 + 10^-80 < U. Negating before truncation preserves
this absolute bound and the existing nearest-rounding behavior for -1. Tests
also verify that the literal and both reference endpoints round identically at
every entry scale through 64.

This is bounded source data, not a constant claimed valid at arbitrary
precision. The precision readback guard still executes before it, ensuring
that the half unit is represented exactly. All other inputs and S >= 65 retain
the dynamic calculation. Literal identity, threshold neighbors, representations
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

Use W = S + D + F + 2*d(S+D+F+1) + 24. Compute the period as 8*a(1) at W;
there is no fixed pi literal. Remove an integer multiple at scale zero, then
place the reduced argument in [-pi,pi]. A quotient off by one at a reduction
boundary merely changes the period representative. Its phase error is bounded
by 16*10^D*u, including the period error and the possible adjacent quotient.
The explicit D guard protects arbitrarily large supported arguments.

Use alternating term recurrences with the squared reduced argument and
successive factorial factors. The magnitude is below four. After the first
few terms, recurrence ratios are at most one half; fewer than 4W+32 terms
suffice. An absolute-series bound exp(4) < 100 bounds early amplification and
cancellation. Conservative series/rounding error is below 4000*(W+10)*u;
combine it with the phase bound above. Both contributions are far below U/2
under the selected W. Odd/even symmetry handles negative input exactly.

## Bessel J

Normalize valid integer orders at scale zero. Use
`J(-n,x)=(-1)^n*J(n,x)` and `J(n,-x)=(-1)^n*J(n,x)` to reduce to n,x >= 0.

First, if n >= 3x and n >= 4*(S+1), return zero. The ascending absolute-series
bound and `n! >= (n/e)^n` give
`abs(J_n(x)) <= (e*x/(2*n))^n * exp(x*x/(4*(n+1)))`.
Under these conditions this is at most `(e/6)^n * exp(n/36)`, below 10^-S.
This branch avoids unbounded work on a huge order whose result is certified tiny.

For x > 8*(n*n+1), attempt the real large-argument expansion from
[NIST DLMF 10.17](https://dlmf.nist.gov/10.17). Coefficients satisfy
`b_0=1; b_k=b_(k-1)*(4*n*n-(2*k-1)^2)/(8*k*x)`.
Split their alternating even/odd sums and reconstruct with phase
`x-(n/2+1/4)*pi` and amplitude `sqrt(2/(pi*x))`.
DLMF 10.17.14-15 bounds the Hankel remainder by
`2*abs(b_k)*exp(abs(n*n-1/4)/x)`, less than `3*abs(b_k)` in this regime.
Taking the real part preserves that bound.

Use W = S + D + F + 2*d(S+D+F+length(n)+1) + 32. Before each coefficient,
check its exact recurrence ratio against one half. If contraction is lost
before the budget is met, abandon this regime and use the ascending series.
While contracted, truncation propagation is below 2u. When a stored next
coefficient vanishes its exact magnitude is below 2u, giving remainder below
6u (amplitude is below one). Sum and reconstruction errors are polynomial in
W; phase precision includes D and covers multiplication by the order. A
conservative aggregate bound `10^6*(W+1)^2*10^D*u` remains below U/2. No
unbounded divergent series is treated as convergent merely because a term grew.

Otherwise, evaluate the ascending recurrence, including the initial
`(x/2)^n/n!`, with W = S + 2*floor(x) + D + F +
2*d(S+2*floor(x)+D+F+1) + 32. The extra 2*floor(x) covers cancellation and
propagation through the absolute series, bounded by exp(2x). After the tiny-
result branch, the initial factor needs at most 7*(W+1) steps; the term loop
needs at most 8*(W+1). The [ascending definition](https://dlmf.nist.gov/10.2.E2) and the
[integer-order modified-Bessel integral](https://dlmf.nist.gov/10.32.E3)
bound absolute series weights by exp(x). After k exceeds x, term ratios are
at most one quarter. Absolute tail/rounding propagation is bounded by
`10^6*(W+1)^2*exp(2x)*u`. Since `exp(2x)*10^(-2*floor(x)) < 8`, the remaining
guards put this below U/2. An underflowed initial factor or term is therefore
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
336 enclosures; regeneration uses the same pinned optional FLINT environment.

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
side effect before the production fixture runs. It certifies all 476 numerical
results from the instrumented definitions against the same exact enclosures.

The source contains 69 decisions and 138 outcomes. The expected fixture requires
127 normal outcomes, including two explicitly undefined-domain log guard calls. Nine
separate portable-host models independently clamp a working-scale setter and
exercise its defensive error branch without returning a numerical value.
Together the ordinary and modeled runs must reproduce 136 outcomes. The two remaining outcomes require a
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
