# Exact coefficient-prefix square roots

Issue #212 evaluates significant-digit precision growth without changing the
builtin square root's decimal floor contract. The original integer-coefficient
Newton path remains the generic small-root fallback. The initial candidate
selects staging at 128 root-coefficient digits; that crossover is experimental,
not an adopted performance result.

For input coefficient A and scale S, result scale is T=max(requested,S). Keep
the existing working scale W (T plus optional guards, without overflow), and
represent the exact integer N=A*10^(2W-S) symbolically. Compute its exponent
and coefficient width in the checked storage-index domain rather than narrowing
them into the public scale/length result. Construct only the coefficient needed
by each Newton request; no full shifted radicand is retained between requests.
Existing zero, negative-input and resource/error paths remain intact.

For q>=0, let Nq=floor(N/10^(2q)). Then
`floor(sqrt(Nq)) = floor(sqrt(N)/10^q)`: both sides select the unique integer r
with `r*r*10^(2q) <= N < (r+1)*(r+1)*10^(2q)`. Thus a short prefix root is
exact, without a floating-point estimate or an assumed number of guard digits.
Start with a positive power-of-ten seed, then lift each exact prefix root when
extending precision. The integer Newton first-step bound and subsequent strict
decrease above the floor retain the existing stop-on-nondecrease rule, including
adjacent-integer cycles. At q=0 the result is the exact integer floor sqrt(N).
Truncating W-T digits finally gives the exact decimal floor at T.

Plan stages backward from the final coefficient width by ceiling-halves, then
execute them forward. The first stage has at most eight digits; every following
extension is at most twice its predecessor. A fixed stack array bounded by the
storage-index bit width holds the plan, without an extra heap allocation. This
avoids a penultimate stage that is almost as large as the final one.
A coefficient-copy helper cancels decimal growth/truncation first. Shrinking
copies only surviving high limbs and applies the remaining scalar decimal
divisor. Growing sizes storage once, copies the original coefficient at its
whole-limb offset and handles the partial-limb carry. One spare limb avoids
capacity doubling. Every Newton request constructs its exact prefix directly
from the original input, without retaining an oversized prefix owner.
Allocation traffic, retained capacity and process RSS still require measurement.
In particular, exact powers and near-square inputs may already have an excellent
old seed and must not lose their fast path merely to exercise
staging. Compare the original coefficient A with the square of the old
power-of-ten seed, expressed as 10^e in that same coefficient domain. When
the residual is zero or its decimal width establishes at least half the
required root precision, preserve the original Newton path. This is a work
selection rule, never a returned approximation or a different stopping test;
both paths retain exact floor convergence. The legacy fallback also requires
its original input-width domain. Development counters retain both favorable
and unfavorable cases separately from elapsed acceptance evidence.

`sqrt_iterations` counts every actual Newton request in either path.
`sqrt_working_scale_sum` retains the fractional scale of those requests (zero
for integer-prefix iterations). `sqrt_precision_stages` counts exact prefix
stages, and `sqrt_root_digits_sum` sums their root-coefficient widths once per
Newton iteration. `sqrt_precise_seed_fallbacks` counts large roots routed back
to the original path by the exact-residual selector. Division request/operand
dimensions and allocator counters
remain independently recorded. These are logical work and storage diagnostics,
not CPU time or RSS.

Validation retains NumberTraits8 precision/guard-boundary tests, focused wider
integer-domain checks, independent Python integer-root inequalities, snapshots,
adjacent squares, tiny fractions and large requested scales. The interpreted
math library is also checked as ordinary source after changing builtin sqrt.
Native paired full/held-out evidence is required before adoption; no speedup or
completed issue is claimed by the initial correct prototype.

## Public and internal width policies

The public coefficient length keeps its existing trait limit and converts and
clamps in the public result type. The internal width query uses storage-width
arithmetic. Both share one scan with explicit result/clamp policies; the public
assertion is selected by policy rather than by assuming NumType differs from
size_t on every host. Zero remains one digit through both interfaces. No public
language limit is enlarged by the internal query.

This preserves the original public clamp ordering while investigating the
prototype's native Windows regression. It is a generated-code hypothesis, not
an accuracy fix or an established performance mechanism. Dedicated component
checks cover valid trait-domain lengths, zero/padded zero and wide internal
queries; the complete exact-root and ordinary-source math oracles remain
required. Native full/held-out results and code inspection decide adoption.

The prefix cutoff uses working **root-coefficient digits**, rather than the
requested output scale. For `sqrt(2)` with the native nine-digit radix and ten
guard places, the root coefficient has `S+11` decimal digits. Requested scales
116, 117 and 118 therefore exercise 127, 128 and 129 working digits. The
profiling fixture computes these scales from the reported radix, independently
checks the exact output floor inequalities and result scale, and observes the
selected path on both sides of the cutoff. Earlier elapsed cases named
`crossover_120/124/128/132` are retained with their original names and programs;
they measure nearby sizes but do not establish that they straddle this cutoff.


The outlined-prefix checkpoint requests a call boundary for the large helper:
MSVC uses `__declspec(noinline)`, compilers advertising `gnu::noinline` use that
attribute, and other compilers retain the identical generic function without
an annotation. See the [MSVC declaration documentation](https://learn.microsoft.com/en-us/cpp/cpp/noinline?view=msvc-170)
and [GCC common attributes](https://gcc.gnu.org/onlinedocs/gcc/Common-Attributes.html).
No arithmetic, seed, cutoff, precision ladder or fallback selection changes.
This is an isolated code-generation experiment under #212/#213, not a proved
explanation of the mixed hosted Windows timings. The prior candidate adds
5,120 raw PE code bytes over current main and has retained input regressions;
its successful broad batch does not erase those losses. Compare complete
full/held-out classes and actual executables before adopting this boundary.


The fixed Windows CPU-coverage workflow launches three predetermined independent
jobs for an explicit checkpoint. It keeps every host model, A/A qualifier and
paired outcome, with exact base/head and replica identity. It does not select a
CPU, filter unfavorable results, retry for a preferred number or pool different
hardware. Actual EPYC 7763 coverage must be checked because a successful newer
EPYC/Xeon batch cannot establish coverage for the earlier failing model. If none
of the fixed jobs obtains that CPU, coverage remains unavailable. The workflow
runs on its own PR-file changes or manual dispatch; ordinary PRs do not gain
an automatic three-job Windows cohort after that workflow is accepted.
