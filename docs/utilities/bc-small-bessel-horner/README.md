# Finite small-argument Bessel experiment

Refs #217 and #197. This is an unqualified ordinary-source algorithm
experiment based on accepted main `1281233d822f090668a3327760018bc0fd5b6c39`.
It changes the Bessel evaluation order, rather than repeating PR692's
guard-only reduction. It does not alter frozen PR708 or reuse its elapsed
results. No performance gain or adoption is established.

For normalized integer order zero or one, absolute argument at most four
and entry scale at most 100, evaluate the finite power series backward
with `K=16+ceil(S/2)` and `W=S+16`. The original input is retained until
the rounding decision is unambiguous. Outside this domain, or within
`10^(-S-11)` of a rounding midpoint, use the accepted generic series.
Order symmetry, exact special cases, caller scale/base and argument
ownership retain their existing language behavior.

The [numerical argument](../bc-portable-math.md#finite-backward-series-for-small-orders)
bounds the omitted tail and every truncation in the stated evaluation
order. The exact rational proof checks all 101 admitted scales and 80
modeled arithmetic/rounding cases. A separate forward infinite-series
oracle checks 162 CLI cases, including both sides of the order, argument
and precision boundaries, long exact decimals and negative symmetries.
Four exact midpoint cases retain observable rounding choices.

The source-bound interpreted inventory has 228 outcomes: 215 observed,
11 additional modeled working-scale failures, and the same two infeasible
nonnegative-exponential outcomes. The previous inventory had 204 outcomes.
Every new working-scale assignment is clipped independently in the
test-side fault model. This is interpreted decision evidence, not compiler
MC/DC or a complete conformance proof.

These are correctness checks, not elapsed benchmarks. Native Linux,
Windows and macOS preflight and a prospective independent qualification
cohort remain required before adoption. The additional predicates,
saved input, rounding-window work, executable bytes and library loading
may cost more than the shorter arithmetic saves. Both one-shot latency
and repeated work must remain visible, along with allocations and RSS.

The correctness workflow covers 12 configurations: ordinary, profile and
generic execution on all three platforms, Linux Clang ASan/UBSan, and
Linux/macOS threaded execution. It checks actual cache/compile definitions,
source and generated library data, and retains executable bytes and logs,
including available partial results after failure. This topic alone skips
the ordinary arithmetic, math and threaded timing workflows; threaded
correctness remains in the dedicated matrix. Main, manual, scheduled and
unrelated branch timing behavior is unchanged. This preflight does not
declare or qualify an elapsed cohort.

The original uploaded artifact is downloaded by its returned artifact ID
and checked against every declared input and image hash. This checks the
actual retained payload, rather than only the producer directory. Hidden
files are enabled within the explicit evidence directory so the copied
public `.github` workflow is retained; the repository workspace is not an
upload target. Original incomplete archives remain separately identified.

The 864,000 total recorded goal-second limit includes all preceding
implementation, queues and review. Keep the original first outcomes and
the approved regression, Howard-margin and startup rules. Do not pool
hosts/sources, retry for acceptance or treat green CI as qualification.
The broader #197/#200/#207/#217 goals stay open where unmet.

The separate [first qualification protocol](qualification.md) declares all
historical and additional inputs, disjoint groups, original native cohorts,
resource/startup requirements and unchanged gates. Its existence does not
establish performance or adoption.

Corrected-setup preparation uses accepted main `3beebb14` and the explicit
unmeasured-input phase. The original `18a25510` study failed before math
calibration and remains frozen. The real controller argv now passes a
synthetic process fixture; all 188 original program/oracle/parameter payloads
are unchanged. This branch runs correctness only until the original outcomes
and the accepted merge's own CI are reviewed and a new protocol is declared.
