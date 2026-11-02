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
arithmetic. The current experiment deliberately retains a separate copy of the
scan for the internal query, preserving the exact previously accepted public
length function rather than routing it through a shared template. The internal
query does not assert the public trait limit, including when NumType and size_t
have equal width. Zero remains one digit through both interfaces. No public
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

## Translation-unit isolation experiment

The noinline-only candidate still has confirmed input-conversion regressions
on the Windows EPYC 7763 cohort (run 41475043315, replica 3), including both
full and held-out decimal/binary input cases. The other two predetermined
replicas ran on EPYC 9V74 and 9V45 and did not confirm losses. Those outcomes
are retained separately; the successful jobs do not establish acceptance.

The next candidate emits the native 16-bit and 32-bit prefix-root entry points
in `number-sqrt.cc`. Their declarations keep the large implementation out of
VM compilation, and retain the existing noinline annotation where supported.
Other trait instantiations, including the exhaustive 8-bit model, use the same
header-defined generic algorithm. No arithmetic, cutoff, scale, seed selection
or iteration condition changes. Without compiler attribute support the external
specializations remain valid C++, with ordinary optimization rules.

This is a code-generation hypothesis. It does not establish that code layout
caused the regression, and it requires fresh native checks and all predetermined
Windows CPU cohort outcomes before adoption. IPO may otherwise change call
boundaries; actual compiler flags and native binary evidence remain relevant.

The local translation-unit candidate passed 247 ASan/UBSan component cases
(1,233,081 assertions), including 25 root cases with 22,367 assertions,
26 driver checks, 15 VM differential checks, the activation-lifetime check,
616 ordinary-source math references, and 23 Release profiling checks. Leak
Sanitizer is unavailable on this host. Release object inspection shows the VM
references the external 32-bit entry without defining either prefix helper;
the kernel object defines both 16-bit and 32-bit entries and implementations.
This confirms local compilation isolation, not a performance improvement or
an MSVC/IPO result. Native validation remains pending.

## Preserve the accepted public length code path

Translation-unit isolation alone did not clear native losses at checkpoint
426905d. Run 41485521321 and the complete three-replica run 41485521233 retain
ten confirmed Windows losses: input cases on EPYC 7763 and a loop on Intel
Xeon Platinum 8573C. Nine corresponding controls pass; one is inconclusive.
Linux/macOS have no confirmed loss, with substantial macOS control noise.

The next refinement restores the public `BasicDigits::length()` function
verbatim from accepted runtime 21b3638. Its internal wide counterpart has a
separate scan. The small duplication intentionally preserves the established
public call/inlining path while keeping enlarged prefix-root dimensions wide.
The prefix arithmetic, cutoff, external kernels and exact-floor tests remain
unchanged. This is one further code-generation hypothesis, not a proved cause
or accepted performance fix. Keep all earlier checkpoints and every native
cohort outcome; successful jobs cannot waive their measured losses.

## Isolate the complete native root

The public-length restoration at `c866b5f` also retained eight confirmed
Windows conversion losses across ordinary run 41491302951 and the complete
fixed-replica run 41491302365. Its Windows code payload matches `426905d`.
Restoring that public helper therefore did not change the retained native
code or establish a fix. All of those outcomes remain part of the evidence.

The next experiment moves the original complete root body into a private
generic implementation without changing its arithmetic. Native 16-bit and
32-bit `sqrt` entry points call it from `number-sqrt.cc`, using the same
supported noinline declarations as the existing prefix entries. Both the
small-root fallback and the staged prefix driver are consequently compiled
outside the VM translation unit. Other traits retain the identical generic
header implementation, including the primary exhaustive 8-bit test model.

Zero and negative handling, guard-scale bounds, public coefficient length,
prefix cutoff, exact Newton termination and final rescaling are unchanged.
The source extraction checks the moved body byte-for-byte before formatting.
This tests compilation isolation rather than a new arithmetic algorithm;
actual native compile commands, executable bytes, broad/held-out results and
all three predetermined Windows replicas are still required before adoption.
It does not waive any earlier loss or complete #212/#197.

Local validation of complete-root isolation passes 247 ASan/UBSan cases
(1,233,112 assertions), seven CLI/malformed-stream/lifetime/signal checks,
616 identical ordinary-source math references and 23 separate Release
profiling checks. LeakSanitizer is unavailable. In that local profiling
build, the VM object has only an undefined native 32-bit `sqrt` reference;
the kernel object defines both native entries and their generic bodies.
This establishes the observed local compilation boundary, without claiming
an uninstrumented timing gain or MSVC/IPO behavior. Native evidence remains
required, including every previously losing workload and CPU-cohort result.

## Complete-root checkpoint and current-source integration

Checkpoint `535420b` passed all twenty current checks and all six predetermined
native comparisons: ordinary run 41512282021 plus the three Windows replicas
in 41512281980. The 33,620 checked executions, eighteen exact timing binaries
and 36 native inspection outputs are verified. No confirmed candidate loss
was observed. Windows includes Intel Xeon Platinum 8370C, two EPYC 9V74 jobs
and EPYC 7763; all earlier rejected checkpoints remain retained.

The expanded full/held-out native corpus includes scales 5,000/5,625 and
10,000/11,250, tiny fractional radicands and large integer coefficients.
For the full scale-10,000 case, candidate/base medians are about 0.211 on Linux,
0.210 on macOS and 0.218 on ordinary Windows, with reported intervals below
one. Small scale-100 timings remain near baseline, with macOS noise limiting
qualification. Reported intervals are not independently bootstrap-recomputed;
CPU models and batches are not pooled. Huge integer roots still lose to Howard
in some classes, so the broader #200/#197 competitive goal remains open.

Separate current-source diagnostics retain 56 checked logical profiles and
126 checked local RSS observations over all fourteen full/held-out root
classes. Logical profiles use local Release profiling builds; RSS uses the
exact uninstrumented macOS-15 arm64 timing binaries on the local M1 host.
Those are local process-memory observations, not memory measurements from the
hosted runner. All owned builds/tests finish first, logical collection finishes
before RSS, and CPU/elapsed resource readings do not support speed claims.

For one scale-5,000 root, the recorded divisor-limb sum falls from 7,798 to
2,519 and requested digit peak from 20,248 to 11,344 bytes. Scale 10,000 uses
28 smaller Newton steps instead of fifteen full-width steps: divisor-limb sum
16,695 to 5,163 and requested digit peak 40,260 to 35,764 bytes. More iterations
at smaller dimensions explain the measurement being taken; an iteration count
alone is not a performance score.

Process RSS remains mixed. Three-trial local medians for the original batched
scale-5,000 workload rise from 2,146,304 to 2,277,376 bytes, and scale 10,000
from 2,179,072 to 2,326,528. The tiny scale-4,096 median falls from 2,146,304 to
2,080,768. The macOS executable grows from 535,728 to 552,656 file bytes.
These code/allocator/process costs are retained alongside the lower requested
storage and native speed gains; no universal memory win is claimed. The first
RSS attempt could not read a sandboxed kernel clock counter and is preserved
as failed/unavailable. Only the missing RSS phase was then collected with the
required read-only resource access; completed logical observations were kept.

The accepted bare-condition fix #413 is integrated before the next adoption
comparison. The combined source passes 248 ASan/UBSan cases (1,233,119
assertions), nine CLI/verifier/lifetime/signal/controller checks, 23 Release
profiling checks and 616 ordinary-source math references. Its own complete
native evidence is still required before adopting the combined source.


## Accepted staged-root implementation

PR #405 merged as `66cbf5d` after all twenty checks passed on final head
`a9a2d591`. Ordinary native run 41531532145 and fixed cohort 41531532108 retain
33,600 checked samples, eighteen exact timing executables and thirty-six native
inspection outputs. There are no confirmed candidate regressions. The three
predetermined Windows replicas include two EPYC 7763 hosts; no CPU model or
unfavorable batch was removed. Scale-5000/10000 paired ratios are respectively
0.232–0.272 and 0.208–0.222 across the six hosts, with small-scale results near
baseline. A/A inconclusive intervals remain visible rather than being called
universal passes.

The final source includes accepted parser condition fix #413. It retains the
same generic algorithm for narrow/custom traits, original exact-floor contract,
precise-seed/small-root fallbacks, and external native 16/32-bit entries described
above. Earlier sections document experiments chronologically; their rejected
sources remain rejected. Acceptance follows the final source's own complete
matrix and does not infer a cause for the old Windows regressions.

The [retained archive](bc-native-controls-and-kernels/README.md) includes all
final native ZIPs, previous losses, logical work counters and the separate RSS
retry. The earlier `535420b` local memory observations remain under that exact
source: lower tracked digit peaks coexist with higher RSS and executable size.
Huge integer square roots still lose to Howard. This completes the precision-
growth evaluation in #212; broader arithmetic and competitiveness work remains
under #197/#200.
