# Native ordinary-library comparisons

The workflow `bc-math-performance.yml` runs on native Linux, Windows and macOS.
It supports issues #216 and #217 under #197. A successful job means its builds
and numerical checks completed; it does not by itself establish a speed target.
A candidate without the ordinary library records `unavailable.json`, with no
performance acceptance. This permits the workflow to land before the library.

`benchmark-math-ci.py` rebuilds the exact candidate, an independent candidate
A/A build, the exact requested GD base, and pinned Howard with matching
uninstrumented release settings and fixed split point 126. A separate candidate
counter build is explicitly instrumented and forbidden to the timing harness.
All builds and correctness/portability checks finish before elapsed measurements.
Allocation probes, CPU/RSS wrappers and executable inspection follow all elapsed
presets and their confirmations. A probe cannot run between full and held-out
elapsed presets in a combined standalone experiment.

The PR base is its actual immutable base SHA. Push, scheduled and default manual
runs use mathematical checkpoint `78a732a6c1f995b45b140d4b86d8d3af57354804`,
which combines the certified ordinary library with the accepted generic divider.
It is an experimental baseline, not a claim that its library meets every speed
goal. Manual dispatch can supply another full immutable SHA. Promote this anchor
only with recorded source, correctness and paired evidence; never substitute a
binary from another runner. Both source-library snapshots are artifacts.

When the base has the library, five preparation roles are candidate GD, its
independent build, base GD, Howard standard and Howard loading the identical
candidate source as ordinary data. Four roles enter the primary paired matrix;
the independent candidate build supplies A/A. A base predating the library emits
`baseline-unavailable.json`; it does not silently load candidate source or produce
a candidate/base pass. Competitive/portability observations remain available.
A candidate without the library records `unavailable.json` without building or
claiming mathematical performance acceptance.

Every subprocess result is checked against an independent committed enclosure.
The twenty regime/argument specifications at four scales have eighty
enclosures, including quick scale 2. Full and held-out corpora each contain sixty cases: ten regimes at
scales 20, 100 and 1000, each with one-shot and repeated modes. Held-out cases
change magnitudes and Bessel orders; their reference file is independently
certified. A hash fixture preserves all sixty original full cases.

PR jobs prespecify scale 100 for every regime and both modes, plus a
repeated scale-1000 logarithm to exercise its high-precision reduction.
Scheduled and manual jobs run both complete corpora. Selection never depends on
observed speed. Each repeated case doubles its invocation count independently
for every preparation role until three checked trials have median duration at
least 0.1 seconds. The common maximum count is frozen before paired trials.
The job has a 120-minute cap and the experiment a 110-minute budget, with
bounded child/build deadlines and retained failure artifacts. Caps and timeouts fail with partial evidence; they never silently shorten a
case. One-shot latency remains unbatched.

Both controls and comparisons replay the same verified program and oracle
bytes. Fifteen paired rounds follow three warmups. A/A intervals outside the
prespecified [0.95, 1.05] band trigger at most one confirmation using a second
fixed order seed and the frozen counts. Bias requires confirmation in the same
direction. Overlapping or noisy intervals remain inconclusive. Subsequent
measured-duration shortfalls remain explicit rather than causing recalibration.

The report retains paired IID and neighboring-round circular-block intervals
and uses their union. Small samples and nonstationary host noise remain
limitations. Each substantive repeated class reports GD/Howard-standard and
GD/Howard-portable ratios separately. The competitive observation needs median
GD/standard <= 0.90 and interval upper < 1; confirmed A/A bias prevents that
observation. Inconclusive controls remain visible qualifications. There is no
aggregate pass hiding individual misses.

Candidate/base ratios use the same paired round IDs and block-aware interval
protocol. A repeated-case interval wholly above 1.05 triggers at most one bounded
confirmation with the same frozen inputs. Both intervals must stay above the
boundary to report a confirmed regression. One-shot latency is not given an
uncalibrated regression pass. Missing duration, confirmed A/A bias and unavailable
baselines remain explicit; inconclusive controls qualify otherwise usable results.
Competitive targets and regression decisions are separate columns, with every
losing class retained. Results remain advisory and do not create a mandatory
performance gate or an aggregate pass.

Whole-process timings include loading, parsing and constant generation. File
identity checks run before the timer and can warm source-file caches. Separate
candidate-only software counters cover one cold scale-100 invocation per regime;
instrumented elapsed values are discarded. Counter reports, exact programs and
enclosures are retained, with zero outstanding tracked storage checked at EOF.
These allocator-request/event counts are not total process malloc, RSS or
hardware instruction counts, and cannot establish a baseline allocation delta.

After all elapsed stages and confirmations finish, native CPU/RSS wrappers collect one cold
invocation per regime and release role near scale 100, for both presets in broad
runs. Linux reports GNU time user/system seconds and KiB RSS; Darwin reports
native user/system seconds and byte RSS, plus optional counters where time emits
them. Printed CPU times can round short calls to zero. Windows queries the
retained child-process handle for peak working-set bytes, peak commit-charge
bytes and user/kernel CPU time. Adapter failures remain explicitly unavailable;
portable candidate allocation counters are separate. Missing native adapters
run no child and are not counted as checked measurements. Resource-query
availability remains separate from an invoked child's output checks.
Diagnostic wrapper elapsed fields never replace paired timing.

The workflow uploads checked raw samples, frozen inputs, calibration trials,
recipes, source snapshots, diagnostic reports, logs, host metadata and per-class
decisions even after failure. The `codegen/manifest.json` artifact also binds the
actual uninstrumented elapsed-role recipes and executable copies, including both
Howard library recipes, plus copies of every declared external input. Duplicate
role names must have identical recipes. Profiled executables cannot enter this
set. Missing or changed declared assets make archival a hard failure; successful
elapsed stages cannot hide that failure or establish acceptance.

Before the first calibration or elapsed child, `elapsed-images/manifest.json`
and its copies retain the same ordinary role assets without native inspection.
Completed build recipes and logs are also copied before that child starts.
These early inputs survive a controller kill that prevents final archival;
they do not replace missing original evidence from older runs. Final recipe
refresh uses atomic replacement so interruption preserves an earlier complete
copy. The separate post-sampling `codegen/` archive and integrity checks remain.

All role assets are copied and hash-checked before optional native inspection.
The original and retained bytes are checked again afterward. Missing inspectors
or exhausted inspection budgets retain the exact files with an explicit
unavailable diagnostic; they do not cause a rebuild or a performance pass.
Math inspection commands have a 15-second maximum clipped to the experiment's
remaining monotonic deadline. Asset preservation still runs after an elapsed
failure or exhausted budget. Exceptions already propagating through cleanup,
including Python interrupts, are not suppressed by final archival attempts.

Older standard math artifacts did not retain measured binaries or all external
input copies. Those missing assets remain unavailable. A later rebuild cannot
repair their binary provenance or qualify an earlier experiment. The new copies
improve future evidence; they do not change historical labels, acceptance rules,
sample counts, calibration, fixed seeds, corpus selection or timing budgets.

Build trees are not uploaded. Parent and descendant
processes are terminated at experiment deadlines on POSIX and Windows; other
Python platforms retain a direct-child fallback. No instrumented configuration
enters the release matrix, and no diagnostic child runs concurrently with timing.

## Broad runs requested through source control

Pushing a candidate branch under `codex/bc-math-broad/` explicitly requests the
same bounded full and held-out matrix as a scheduled or manual run. The runner
builds the exact pushed SHA, with the same read-only token permissions and
checked release protocol. The branch prefix selects the larger workload set;
it does not alter production arithmetic or numerical acceptance.

This provides a reproducible opt-in route when Actions workflow dispatch is
unavailable to a client. Ordinary PR jobs retain their representative subset;
broad push and PR artifacts remain separate experiments with their own source
and host identities. Preserve all per-class misses and failures from either.


## Child and experiment deadlines

The native orchestrator declares `--child-timeout-seconds` (default 120).
It passes that same positive finite bound to calibration, paired/A-A stages,
confirmations and separate diagnostic invocations, and records it in the
orchestration artifact. This is distinct from the retained 6600-second workflow
experiment bound and 120-minute job limit; it changes no sample count,
substantive-duration target, correctness check or acceptance threshold.

Sampling stages without a shorter explicit command deadline use the remaining
`--total-seconds` budget. They have no additional implicit one-hour cap. Explicit
preparation limits are clipped to that remaining budget, and individual child
deadlines still apply. Exhaustion is a hard failure with partial logs retained;
it does not shorten the matrix or establish performance acceptance. Resource
diagnostics retain their separately documented per-child limits.

A common calibrated batch can exceed the standalone harness's 30-second
default when the candidate is substantially faster than a comparator. The
first trig/Bessel broad attempt records a Howard 1000-place cosine timeout at
512 calls; it is retained as failed evidence and never pooled into the bounded
120-second-child retry. The independent full/held-out inputs and per-role
output checks remain mandatory. A global deadline failure still grants no
acceptance; this setting does not promise that every host can finish a matrix.


Broad workflow runs now schedule `full` and `holdout` independently on each
native OS. `--broad-preset full` or `--broad-preset holdout` selects that cohort
and requires `--broad`; omitting the selector retains the standalone combined
mode. Orchestration records the selected presets, and held-out artifacts/checks
carry a distinct suffix. PR-sized full check names remain unchanged.

Both cohorts use the same immutable source and experimental anchor, with
independent builds, calibration, same-job paired roles and controls. No tuning
occurs between cohorts, and their raw observations are never pooled into one
sample distribution. Every job retains its 6600-second experiment and 120-minute
job limits. This avoids exhausting the Windows preparation margin while
preserving all cases, samples, output checks and duration qualifications.
