# Native ordinary-library comparisons

The workflow `bc-math-performance.yml` runs on native Linux, Windows and macOS.
It supports issues #216 and #217 under #197. A successful job means its builds
and numerical checks completed; it does not by itself establish a speed target.
A candidate without the ordinary library records `unavailable.json`, with no
performance acceptance. This permits the workflow to land before the library.

`benchmark-math-ci.py` builds the immutable candidate twice with the same
uninstrumented release recipe, and builds the pinned Howard source once. All
builds finish before timing. Four preparation roles use GD's embedded ordinary
library, an independent GD build, Howard's standard library, and the identical
ordinary source loaded by Howard. Source, binary, compiler and ordinary-file
identities are retained. The same-source GD builds provide an A/A control;
Howard's two library roles distinguish host execution from library algorithms.

Every subprocess result is checked against an independent committed enclosure.
The twenty regime/argument specifications at four scales have eighty
enclosures, including quick scale 2. Full and held-out corpora each contain sixty cases: ten regimes at
scales 20, 100 and 1000, each with one-shot and repeated modes. Held-out cases
change magnitudes and Bessel orders; their reference file is independently
certified. A hash fixture preserves all sixty original full cases.

PR jobs prespecify scale 100 for every regime and both modes (twenty cases).
Scheduled and manual jobs run both complete corpora. Selection never depends on
observed speed. Each repeated case doubles its invocation count independently
for every preparation role until three checked trials have median duration at
least 0.1 seconds. The common maximum count is frozen before paired trials.
Caps and timeouts fail with partial evidence; they never silently shorten a
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

Whole-process timings include loading, parsing and constant generation. File
identity checks run before the timer and can warm source-file caches. Peak
memory, instruction counts and allocation diagnostics remain separate work;
these jobs do not infer those properties from wall-clock samples. The workflow
uploads checked raw samples, frozen inputs, calibration trials, recipes, logs,
host metadata and per-class decisions even after failure. Build trees are not
uploaded. Parent and descendant processes are terminated at experiment deadlines
on POSIX and Windows; other Python platforms retain a direct-child fallback.

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
