# Integrated result-lifetime and Bessel candidate

Refs #696, #694, #207, #217, #200 and #197. This is a new unmerged experiment
against accepted main `7989a2e8505d35f8ebe4a1ff04d3033499b0b48f`. Prefix result
cleanup remains default OFF during correctness preflight. No integrated elapsed
gain, adoption or issue completion is established here.

## Changes and generic execution

Reuse the four signed result-lifetime commits through `f6e47129`: retain empty
generic/typed result objects, reset the conservative accessed prefix and obtain
compact destinations only for producing operations. The explicit OFF option
retains ordinary vector construction/cleanup. See
[result ownership and diagnostics](bc-prefix-clear-results.md).

Reuse the exact numerical source/test/proof change from `99d80c85`: ascending
Bessel evaluation removes dependence on stored input length and uses sixteen
residual digits, with magnitude and digit-count guards. Near a half-unit boundary
it retries once at the previous precision with the saved exact argument.
Hankel behavior is unchanged. See the [portable accuracy argument](bc-portable-math.md).
There is no Number representation/API change, privileged math callback or
function-name recognition. The same ordinary bc source runs on other conforming
implementations. Standard C++20 and the CMake 3.15 floor are unchanged.

These changes address different work. Result initialization/cleanup can be paid
even when a branch returns early; the numerical change reduces arithmetic in
the ascending series. Their interaction must be measured. Earlier Bessel
early-return losses do not establish their cause, and earlier isolated ratios
must not be multiplied to predict an integrated result.

## Independent earlier evidence

The original `ae643450` lifetime preflight is retained with its three native
generic-prefix work-contract failures. The test preceded the correction and was
not weakened. Corrected `f6e47129` has all 48 own supporting checks green and all
38 supporting archives reviewed, but no elapsed qualification.

The frozen `f17b9134` Bessel study has all nine qualification jobs/archives and
31 supporting checks/20 archives complete and reviewed. It remains unqualified:
266 substantive advisory comparisons and Windows startup are inconclusive.
It has workload-specific gains and increased selected-storage cases, not an
overall performance result. No source was retried for favorable observations.

The separate compact Word source `0312d330` remains frozen under its own study.
None of these measurements qualifies this integrated source. Preserve all old
worktrees, images, raw outcomes, source bundles and limitations separately.

## Correctness before timing

Build and exercise the combined source in ordinary, full-check/verified,
generic/typed/wide, explicit OFF and native-function-coexistence modes. Retain
allocation-failure, escaping-owner, sparse-array, recursive, signal and CLI
coverage, and the source-bound independent math/decision checks. The native
18-mode prefix workflow includes all mathematical checks and retains the new
library, oracle/decision inputs, actual compiler selections, VM objects,
correctness images and logs. Its matrix thresholds and tests are unchanged.

Only this topic's redundant narrow PR timing jobs are skipped during preflight;
main, scheduled, manual and unrelated selections retain their previous behavior.
After the exact-head native preflight is green and its artifacts reviewed,
declare a fresh complete three-platform full/held arithmetic/math/startup and
resource study. Bind actual prefix source/compiler policy in both ordinary and
diagnostic images, including the baseline's absent implementation. Flags alone
do not prove dynamic ordinary/profile path identity.

## Acceptance and limits

Keep the approved per-change caps, Howard-margin exception, separate competitive
target, controls, precision, startup, one-shot and resource requirements. Do not
pool or transfer outcomes, omit classes, retry unfavorable studies or relax
thresholds. Prefix tracking can cost more than the initialization it avoids;
retained objects, larger Bessel code, selected storage, process RSS and code size
require review. A green correctness collector is not elapsed acceptance.

No local production elapsed benchmark is run. The ten-day TOTAL #197 goal clock
includes implementation, queues, collection and review; stop qualification at
864,000 seconds, preserving accepted/rejected/unmet results and leaving unmet
issues open. Keep at least 20 GiB free and preserve active source and evidence.
