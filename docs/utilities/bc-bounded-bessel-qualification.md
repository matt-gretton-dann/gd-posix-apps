# Bounded Bessel precision qualification

This #217/#197 experiment compares the committed bounded ascending pass with
accepted main `64e7ee6763b1b7a0731d88d72b7d9e9c2397dc19`. The mathematical
error budget and rounding fallback are in `bc-portable-math.md`. Nine guard
places apply only for absolute arguments through eight and entry scales
through 100. Ambiguous rounding repeats the existing generic calculation with
the original input. All other inputs retain generic precision. No benchmark
names, cached answers, native Bessel opcode or privileged library dispatch is
introduced. A speed benefit remains unmeasured before this cohort.

Before publication, record the immutable final source and protocol/workflow
hashes in #217. Do not edit that source, replace incomplete/adverse outcomes or
restart the cohort for acceptance. All earlier rejected runtime experiments
and #673's limited diagnostic remain unchanged. Its short CPU cases do not
qualify this candidate or explain earlier regressions.

## Correctness preflight

Commit the 110 new independent Arb enclosures and exact-rational budget tests
before the implementation. The 25 CLI tests pass on the unchanged library and
candidate, including ordinary source on Apple Howard 7.0.3. Six focused math
CTests pass with extensions ON and OFF. The source-bound ledger certifies
727 results and accounts for 220 interpreted outcomes: 208 observed, 218 with
explicit scale-clamp models and two unchanged infeasible exponential-sign
outcomes. These are interpreted decision counts, not compiler MC/DC.

The error proof covers input truncation, squared-argument perturbation, finite
coefficient/term counts, absolute propagation and output rounding. The
boundary tests retain the prior midpoint results. At most two passes run;
ambiguous inputs can require extra work. Hosted correctness remains required
on the final publication head. No production elapsed benchmark runs locally.

## One declared native cohort

Run `ubuntu-24.04`, `macos-15` and `windows-2025`, each with arithmetic,
full-math and held-out-math jobs: nine qualification jobs. Use the maintained
default build options (extensions ON and each platform's ordinary typed,
image and IPO defaults), recorded by the native builders. Extensions OFF is
covered by correctness preflight and hosted builds; it is not an invented
optional-backend comparison. Neither source introduces a new backend mode.

Arithmetic retains all 66 full and 66 held-out cases, nine measured pairs,
one warm-up, seeds 197200/197201, existing bounded confirmations and the fixed
sparse supplements. The maintained 900-second command bound remains, with
the existing sparse-stage bounds. Math retains all 60 cases per preset,
fifteen measured pairs, three warm-ups, ordinary seed 1972170, a .25-second
repeated-duration target, 600-second child bounds and an 18,000-second
controller budget per preset. Counts are frozen after the maintained separate
duration calibration, before paired samples; later shortfalls are retained.
Existing bounded A/A and candidate/base confirmations use seeds 1972171 and
1972161. These declared confirmations are not optional favorable restarts.
Each job has a 360-minute outer bound and must be attempt one.

Arithmetic A/A independently rebuilds accepted base; mathematical A/A
independently rebuilds candidate. All ordinary and diagnostic builds finish
before elapsed sampling. Each job binds pinned native Howard, source,
compiler/SDK/standard-library identity, flags, actual binary hashes, argv and
mathematical libraries. No source or host pooling, aggregate weighting,
population-of-layouts inference or transfer of earlier acceptance is allowed.

## Matched resources and startup

Enable `--compare-resources`. Matching diagnostic builds collect software
allocation/refcount/clone events after all ordinary elapsed stages, followed by
five alternating cold native RSS pairs. Arithmetic representatives include
startup, loop/call/recursion, array/sparse storage, streaming/file input,
temporary chains and the fixed early-return/dense frame-reuse cases. Math
uses ten one-shot regimes at scale 100 plus scale-20 atan and all five Bessel
regimes. Both ordinary and diagnostic math roles use their own `-l` library.
Instrumented elapsed values are discarded. Counts cover selected containers,
not process malloc; cold RSS is descriptive, not a retained-memory estimate
or confidence interval. Unavailable adapters and incomplete pairs stay explicit.

Retain the actual instrumented images/recipes in `profile-images/` and the
actual ordinary images/recipes/declared inputs in `codegen/`, separately.
The collector's generic binary/hash fallback applies when native inspection
is unavailable. Missing measured binaries cannot be rebuilt. Source libraries,
native command logs, build metadata and inspection outputs remain bound to
their source; the archives are not comprehensive source/build-tree snapshots.

After arithmetic collection, bind the exact base/A/A/candidate executables to
a fresh `startup-calibration.py` study with its unchanged 60 two-stage null
blocks, 151 pairs per stage, candidate stages, fixed seeds, 2,400-second budget
and five-second child bound. Preserve the timer, sensitivity/precision and
null-bias qualifications. This no-library startup study does not establish
equivalence or automatically qualify mathematical one-shot latency. Old
calibrations and #673's CPU/wrapper study cannot substitute for it.

## Review and adoption

Apply the current `bc-performance-acceptance.md` policy without changes:
Linux/Windows retain the 5% per-change cap, macOS its existing cap, and the
beyond-cap exception requires a candidate/primary-Howard upper 95% bound at
most .90, passing controls/duration/sample requirements and a worthwhile gain
elsewhere. Startup and one-shot rules remain separate. A green collection job
does not make an incomplete or inconclusive row qualified.

Independently replay raw counts/order/check identities, program/oracle bytes,
role/image/input bindings, intervals and decisions for the full cohort. Review
correctness, startup, allocations, memory and executable size before adoption;
retain every adverse or unavailable outcome. The existing two-platform
threaded workflow may also run because CMake registers the resource fixture;
retain its separate evidence without pooling it into this nine-job cohort.
Main/scheduled/manual trend workflows remain enabled. This topic replaces
redundant narrow PR timing with the declared full cohort only.

Merge only a completely reviewed green candidate, immediately synchronize the
primary checkout to origin/main, and review that merge revision's own CI before
any next integration. #217 and #197 remain open until their actual numerical
and competitive obligations are fulfilled.
