# Native controls and kernel checkpoints

Refs [#197](https://github.com/matt-gretton-dann/gd-posix-apps/issues/197),
[#204](https://github.com/matt-gretton-dann/gd-posix-apps/issues/204),
[#207](https://github.com/matt-gretton-dann/gd-posix-apps/issues/207),
[#212](https://github.com/matt-gretton-dann/gd-posix-apps/issues/212),
[#216](https://github.com/matt-gretton-dann/gd-posix-apps/issues/216),
[#397](https://github.com/matt-gretton-dann/gd-posix-apps/issues/397) and
[#402](https://github.com/matt-gretton-dann/gd-posix-apps/issues/402).

This archive retains successful and rejected checkpoints together. There are
317,840 checked native children across 31 checkpoints and 75 byte-for-byte
original native ZIPs. They include 207 timing executables and 414 native
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
#207, #212, #216 or #197, nor adopt a Toom threshold or staged-root candidate.

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
