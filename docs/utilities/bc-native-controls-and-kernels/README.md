# Native controls and kernel checkpoints

Refs [#197](https://github.com/matt-gretton-dann/gd-posix-apps/issues/197),
[#204](https://github.com/matt-gretton-dann/gd-posix-apps/issues/204),
[#207](https://github.com/matt-gretton-dann/gd-posix-apps/issues/207),
[#212](https://github.com/matt-gretton-dann/gd-posix-apps/issues/212),
[#216](https://github.com/matt-gretton-dann/gd-posix-apps/issues/216),
[#397](https://github.com/matt-gretton-dann/gd-posix-apps/issues/397) and
[#402](https://github.com/matt-gretton-dann/gd-posix-apps/issues/402).

This archive retains successful and rejected checkpoints together. There are
466,490 checked native children across 45 checkpoints and 105 byte-for-byte
original native ZIPs. They include 297 timing executables and 594 native
inspection outputs; the six older #387 ZIPs predate executable collection and
retain recipes/binary hashes only. Missing historical machine code remains
explicitly unavailable. Completed local producer, typed-pool and Toom experiments
add 56,394 checked timing children, eleven exact executables and 22 post-sampling
native inspection outputs. These counts are evidence, not pooled acceptance.

Three additional fixed Windows deployment ZIPs retain eighteen byte-identical
image copies, 54 checked module probes and 3,024 checked timing children. These
are separate diagnostic launches and are not included in the native counts
above. The diagnostic did not reproduce persistent deployment bias; it does
not establish a cause for earlier regressions or waive them.

## Decisions and boundaries

| Checkpoint | Status |
| --- | --- |
| `policy-403` | Accepted symmetric A/A correction; #402 closed |
| `producer-initial-401` | Superseded one-sided controls; corrected qualification is separate |
| `producer-final-401` | Accepted bounded producer-slot step; #207 remains open |
| `root-initial-387` | Rejected initial Windows loop regression |
| `root-test-followup-387` | Successful different-CPU batch; no waiver of initial loss |
| `root-pinned-400` | Rejected root candidate; diagnostic workflow pins must not merge |
| `root-clamp-404` | Accepted into the root topic only; no main-baseline waiver |
| `root-integrated-subset-405` | Rejected: two Windows input regressions |
| `root-integrated-broad-405` | No confirmed loss in this batch; does not erase subset losses |
| `toom-default-406` | No confirmed A/B loss; six Windows A/A biases limit attribution |
| `toom-opt-in-406` | Threshold 512 rejected: eight confirmed losses |

[checkpoints.json](checkpoints.json) records exact source revisions, run IDs,
artifact hashes and per-checkpoint treatment. [audit-summary.json](audit-summary.json)
keeps every confirmed loss and control qualification per host. The ordinary main
math implementation was unavailable in these native matrices; green unavailable
math jobs are not math-performance passes. This archive does not close #204,
#207, #212, #216 or #197, nor adopt a Toom threshold. The final staged-root implementation was accepted in #405.

The shared A/A policy requires a complete interval inside [0.95,1.05] to pass.
At most one independent-seed confirmation can establish persistent bias in
either direction. Missing confirmations, overlap, invalid intervals and direction
reversal remain inconclusive. Earlier one-sided arithmetic decisions stay in
the original ZIPs; replaying the frozen corrected policy reports changed
qualifications without rewriting history or removing adverse A/B signals.
Recorded intervals remain nominal: this verifier does not regenerate bootstrap
estimates or eliminate shared-runner contention.

## Windows hardware matters

The retained earlier #387 loop loss, #400 loop losses and integrated #405 subset
input losses ran on AMD EPYC 7763. The successful #387 test-only follow-up used
Intel Xeon Platinum 8370C; the successful integrated #405 broad batch used AMD
EPYC 9V45. This is an observed association across different source checkpoints,
not a proved hardware cause. Passing another CPU model cannot establish coverage
for the known failing model. Full host/compiler/image metadata remains in each
artifact; the data is not pooled across CPU models.

The #405 subset and broad Windows code payloads match within each role, while
the candidate adds 5,120 raw PE code bytes over its base. Equal payloads do not
establish equal behavior: data, encoded addresses, runtime placement and host
contention still matter. Later outline, translation-unit, public-length and fixed-CPU checkpoints are
now retained too. None establishes an accepted root fix; their confirmed
conversion and other losses remain in the original artifacts.

## Local producer-slot comparison

The local Apple Release experiment compares runtime `a5f8334` with `9e2c5d4`
and an independent baseline build. Each full/held-out cohort has 56 historical
arithmetic classes plus four storage cases, and 60 math classes. All GD roles
load the exact same ordinary portable library source without `-l`. This measures
VM effects, not library competitiveness. Programs, independent oracles, source
and binary hashes, 15 samples/three warmups and the pinned Howard arithmetic
reference remain in the original recipes and frozen preparation.

All 240 paired classes have no confirmed regression, with 202 passing and 38
inconclusive controls. Short classes retain historical fixed counts rather than
claiming calibrated duration. Source editing, artifact downloads and read-only
audits occurred during sampling; builds, tests, profiling and RSS collection
finished first. The complete raw matrices and original collectors are retained.

The separate 48-child RSS run has mixed results. Full-cohort large-uncalled
median RSS rises from 34,160,640 to 34,979,840 bytes; called-case RSS falls from
5,390,336 to 5,144,576. Alternating frames are unchanged and recursive RSS falls
slightly. Held-out uncalled RSS also rises. Logical result-slot savings therefore
do not establish a process-memory win. All observations remain in
[producer-resources.zip](producer-resources.zip).

## Further checkpoints and local experiments

The added root checkpoints include all three predetermined Windows CPU replicas
for each of `01b`, `426` and `c866`, including losing EPYC 7763 outcomes. The
public-length restoration has the same Windows code identity as the preceding
translation-unit checkpoint; it is negative evidence for that hypothesis.
The accepted #408 source-path fix is retained separately from root adoption.

The Toom square-identity and sparse fixes at `6a5` retain four native losses at
threshold 512. Training compares 128/512/2048 and records the 2048 selection
before held-out evaluation; exact selection bytes/hash and all outcomes remain.
The local evaluation has no confirmed loss, but large dense operations still
allocate more memory and repeated-digit cases remain slower than Howard. The
native 2048 batch has no confirmed candidate loss, while its separate default
batch retains four Windows losses with matching A/A biases. No threshold or
kernel is adopted by this archive.

The typed-pool checkpoint `9028375` retains sixteen Linux and two Windows losses.
Its local 20,880-child experiment has no confirmed loss; that does not waive the
native results. The 216 RSS observations are mixed, and early RSS collection
overlapped a baseline profiling build; CPU/elapsed readings from those resource
runs are not performance evidence. The 96 separate logical observations release
all accounted storage. Later direct numeric access requires its own native
checkpoint and is not accepted by retaining these earlier results.

The additional Toom diagnostics retain 288 logical observations and 540 RSS
observations separately from timing. Local timing began only after owned builds,
tests, profiles and resource collectors ended; source editing and artifact audits
were permitted. The host was not isolated. Frozen historical classes and fresh
large shapes keep their labels, counts and independent oracles.

The accepted #411 diagnostic is retained with its ordinary three-host comparison.
Its fixed Windows deployments run on Intel Xeon Platinum 8573C and two AMD EPYC
9V74 runners. All module queries succeeded; each copy had one observed base over
three probes, with different bases across copies. Timing qualification is mixed
and no persistent bias was reproduced. These observations do not characterize
EPYC 7763 or reveal the module addresses of the timed children.

## Combining uniform runs and subtractive Karatsuba

The Apple Clang 21.0.0 arm64 Release comparison used ThinLTO and a fixed
split point of 126. The builds were the frontend baseline `72485e34`,
uniform runs `3899f02a`, subtractive Karatsuba `18260d62`, and a clean
merge of those two candidates without additional source edits. The
combined build passed 28 local arithmetic-oracle and VM CLI tests.

The benchmark used the arithmetic full and holdout presets restricted to
names beginning with `multiply_`, `square_`, `factorial_`, or `fibonacci_`.
It ran the four GD builds and pinned Howard 7.0.3, with nine measured
launches and one warmup per role and workload. There were 14 workloads
per preset and 1,400 checked outputs in total.

Across the two presets, the combined median ratios ranged from 0.982
to 1.010 against uniform runs alone. For the repeated 10,000-digit
product, uniform runs took about 4.2% of baseline time and adding
Karatsuba gave a ratio of 1.007 [0.981, 1.038] against uniform runs.
The held-out repeated product gave a combined ratio of 1.004.
The dense cases gave no clear additional gain. This one-host comparison
had no separate A/A control and leaves the combined kernel experimental.

The [full](../bc-multiply-combined-evidence/full.jsonl.gz) and
[holdout](../bc-multiply-combined-evidence/holdout.jsonl.gz) archives
retain the checked samples. The
[ratios](../bc-multiply-combined-evidence/ratios.csv) report each build
against the uniform-run reference.

## Reproduction and limitations

From the repository root, run:

```
python3 docs/utilities/bc-native-controls-and-kernels/verify.py
python3 docs/utilities/bc-native-controls-and-kernels/test-verifiers.py
python3 docs/utilities/verify-bc-evidence-checkout.py
```

The first check reads raw samples/programs/oracles, source/build recipes,
compiled-threshold identities, uninstrumented flags, symmetric qualifiers and
exact executable/native-output hashes. It replays local counts, control results
and declared-input hashes too. It writes the deterministic audit summary and
retains losses as data; its success is an integrity/replay result. Actual
performance acceptance remains the per-checkpoint decision above.

The tests reject omitted checked samples and altered timing executables, and
simulate Windows text newline translation to require byte-stable audit replay.
They run on Linux, macOS and Windows alongside archive replay.

The final check verifies all repository evidence manifests and Windows checkout
byte stability. Use `--index` before committing staged evidence. `.gitattributes`
keeps original artifact bytes, including native Windows CRLF records, unchanged.
The scripts directory preserves original collection/audit source, including
historical absolute paths; the standalone verifier uses this archive's relative
paths. Corrected policy and executable-collector source snapshots are retained.

Validation logs include the original failing A/A tests, producer allocation
budget and Toom square/sparse guards. Two concurrent sanitizer first argument
checks hit their existing two-second deadline; subsequent isolated/idle full
checks and ten bounded diagnostic repetitions passed without deadline changes.
The original failures remain, their cause is unproven, and LeakSanitizer was
unavailable. No recorded successful rerun erases those limitations. Later source
refinements and new native results require their own evidence before adoption.

## Complete-root and numeric-storage follow-up

The additional twelve original native ZIPs preserve these completed checkpoints:

| Checkpoint | Source and result |
| --- | --- |
| `typed-cbbc-409` | `cbbc754`: 16,910 checked children; Windows held-out calls regress about 6.9%, with a passing A/A control. Rejected despite green jobs. |
| `root-535-405` and its three CPU replicas | `535420b`: 33,620 checked children; no confirmed candidate losses, including the predetermined EPYC 7763 replica. Superseded by integration with accepted main. |
| `load-conditions-414` | `854b59e`: 6,300 checked children, no confirmed candidate losses. Accepted parser fix #413 loads the condition before branching; it preserves verifier checks. |

These observations neither replace the earlier failures nor establish the
remaining competitive targets. In the `535` ordinary matrix, scale-5000/10000
square roots improve substantially against the accepted baseline. Huge integer
square roots still lose to Howard. Small-scale roots retain near-baseline
performance; noisy intervals remain inconclusive. Native executables and all
inspection outputs remain inside their original ZIPs.

[root-storage](root-storage) retains 56 checked logical root observations,
126 checked RSS observations, twelve checked numeric-storage logical
observations, collector sources and two test-first metadata failure logs.
`verify-root-storage.py` checks complete observation grids, workload/oracle and
report hashes, allocation balance, compiler snapshots, four retained profiling
executables and links the RSS recipes to the exact native macOS `535` artifact.
The archive replay does not execute those binaries or recompute their original
output checks. `test-verifiers.py` checks valid replay and rejects omitted
observations, changed RSS reports and changed profiling executables.

The original root collector's manifest remains **failed**: all 56 logical
observations completed, but `/usr/bin/time` could not query `kern.clockrate`
in the sandbox. The separate RSS collector fills only that missing phase.
It ran the retained CI macOS arm64 binaries on the local M1 after owned
builds/tests/profiles finished; these are local RSS observations, not hosted
runner RSS or elapsed-time performance measurements. The host was not isolated.
For scale 5000, logical single-root digit peak falls from 20,248 to 11,344 bytes,
while batch median RSS rises from 2,146,304 to 2,277,376 bytes. For scale 10000,
the corresponding values are 40,260 to 35,764 logical bytes and 2,179,072 to
2,326,528 RSS bytes. Native macOS binary size rises from 535,728 to 552,656 bytes.
The memory and code-size costs remain visible beside the speed improvements.

Numeric-storage measurements compare `cbbc754` with the rejected optional
header prototype `4c66d13` and sparse owned-header candidate `8cf4c3b`.
The optional prototype's original executable was overwritten and is unavailable;
its raw reports and recorded identity remain. The intermediate indexed-owner
prototype was uncommitted and its executable is unavailable; only its 688-byte
metadata test failure is retained. No timing or adoption claim rests on either
missing executable. The optional-deque metadata test separately fails at 4088
extra bytes. The final sparse candidate passes both bounds, but its generic
recursion frame peak is 15,928 bytes versus 14,392 for `cbbc754`; it does not
universally reduce frame allocation. The completed sparse and root integration matrices are retained below and included in the totals.


The final `a9a2d591` root matrix adds 33,600 checked children and eighteen exact
executables across ordinary Linux/macOS/Windows and three predetermined Windows
replicas. None confirms a candidate regression. Replicas 1 and 3 ran on EPYC
7763; replica 2 used Xeon 6973P-C, and ordinary Windows used Xeon Platinum
8573C. All twenty current-head check records passed before PR #405 merged as
`66cbf5d`. Scale-5000 paired ratios range from 0.232 to 0.272 and scale-10000
from 0.208 to 0.222 across those six hosts. Small-scale roots remain near
baseline. Neither this successful source nor the earlier `535` source erases
the rejected source checkpoints or their recorded losses. The remaining huge
integer/Howard competitive gap stays under #200/#197.

The final `8cf4c3b` sparse numeric-backing matrix adds 16,860 checked children
and nine exact executables. Linux/macOS have no confirmed candidate loss.
Windows held-out `loop_875000` regresses: first median 1.08283 with 95% interval
[1.06805,1.11639], bounded confirmation 1.07905 [1.06678,1.13833]. Its A/A control
is inconclusive. That limits attribution without waiving the loss: PR #409
remains unaccepted, independently of its earlier `cbbc754` held-call loss.


## Direct remainder and compiler-policy acceptance

Nine additional original ZIPs retain the initial/final remainder sources and
explicit IPO controls. `remainder-initial-8cbb-417` has 17,430 checked children
and a rejected Linux small-square loss: median 1.06474 with 95% interval
[1.06122,1.06940], confirmation 1.06815 [1.06392,1.07600], A/A passing.
`remainder-final-46a7-417` has 17,400 checked children with no confirmed candidate
regression and was accepted in #417 as `dbf0e68d`. Linux small squaring remains
1.0151 [1.00397,1.02600], below the material threshold; it is not called faster.
All seventeen current-head checks passed before merge. Linux controls are
100 pass/16 inconclusive, macOS 116 inconclusive, Windows 91 pass/25 inconclusive.
The noisy macOS batch cannot establish precise universal effects. The earlier
source remains rejected; it is not replaced by a favorable rerun of that head.

`compiler-policy-d08-418` retains 6,530 checked children for #418, merged as
`628ddbfb` after seventeen checks. No candidate loss is confirmed. Windows has
nine confirmed A/A biases despite identical base/candidate/control code payloads
(259,584 bytes, SHA-256 362f8970d3a5a458992d88c075988d1740fdfdca85f7859ac355cc819c119484).
Linux controls are 31 pass/11 inconclusive; macOS 42 inconclusive; Windows
20 pass/13 inconclusive/nine biased. This proves neither a cause for the bias
nor a compiler speedup. Source-generated candidate AUTO policy bytes/digests
are retained for every host; older baseline/control recipes remain explicitly
`legacy-auto`. Full compiler/PGO experiments under #213 are incomplete.

[remainder-compiler](remainder-compiler) preserves twelve logical observations
for each remainder source, four exact profiling binaries, source/compiler
snapshots, the exact moved arithmetic body, symbol observations and a separate
54-child RSS run of exact final macOS CI binaries on the local M1. Three trials
per role/case were fixed in advance; all owned builds/tests/profiles finished
before RSS. These are local, unisolated resource observations, not hosted-runner
RSS or elapsed timing. Full integer-modulo total digit allocation falls
8,832,700 to 4,788,700 bytes and basic product calls 3000 to zero, while peak
tracked digits rise 2792 to 3240. The blocked-call counter is included in basic
calls; those counters are not disjoint. Both scaled directions and all held-out
cases show the same traffic/peak tradeoff. Full integer median RSS is
2,179,072 to 2,146,304 bytes; sample medians are lower for all six cases, without
claiming a universal reduction. Final macOS file size rises 552,656 to 552,704
bytes; Windows code payload rises 259,584 to 262,656 bytes.

`verify-remainder-compiler.py` checks complete observation grids, balanced
allocations, source/report/program/oracle and executable hashes, actual RSS
report values, and native IPO policy bytes. Original output-check records are
retained rather than re-executing binaries on the replay host. Negative tests
reject duplicate observations replacing a missing case, altered instrumented
executables and altered policy bytes. Native timings are never pooled into an
aggregate acceptance claim. Prepared divisors, caller scratch and the other
#197 requirements remain separate work.

The combined accepted source at `dbf0e68d` was rebuilt with the accepted IPO
controls and native remainder implementation together: 257 ASan/UBSan cases
(1,505,459 assertions), fourteen selected CLI/build/controller tests, and
616 ordinary-source math references pass. This checks their integration without
relabeling older source-specific timing or resource observations.
