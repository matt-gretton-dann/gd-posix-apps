# Native controls and kernel checkpoints

Refs [#197](https://github.com/matt-gretton-dann/gd-posix-apps/issues/197),
[#204](https://github.com/matt-gretton-dann/gd-posix-apps/issues/204),
[#207](https://github.com/matt-gretton-dann/gd-posix-apps/issues/207),
[#212](https://github.com/matt-gretton-dann/gd-posix-apps/issues/212),
[#216](https://github.com/matt-gretton-dann/gd-posix-apps/issues/216),
[#397](https://github.com/matt-gretton-dann/gd-posix-apps/issues/397) and
[#402](https://github.com/matt-gretton-dann/gd-posix-apps/issues/402).

This archive retains successful and rejected checkpoints together. There are
112,040 checked native children across eleven matrices, plus 19,440 local producer
children. These are evidence counts, not a pooled performance pass. All 33 native
ZIPs remain byte-for-byte original, including 81 timing executables and 162
native inspection outputs from the newer 27 artifacts. The six older #387 ZIPs
predate the executable collector and retain recipes/binary hashes only. Missing
historical machine code is explicitly unavailable. Three actual local producer
executables and six
post-sampling inspection outputs are retained separately.

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
contention still matter. The later outlined-root experiment is not represented
as an accepted fix here; these are its retained pre-change baselines.

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

## Reproduction and limitations

From the repository root, run:

```
python3 docs/utilities/bc-native-controls-and-kernels/verify.py
python3 docs/utilities/verify-bc-evidence-checkout.py
```

The first check reads raw samples/programs/oracles, source/build recipes,
compiled-threshold identities, uninstrumented flags, symmetric qualifiers and
exact executable/native-output hashes. It replays local counts, control results
and declared-input hashes too. It writes the deterministic audit summary and
retains losses as data; its success is an integrity/replay result. Actual
performance acceptance remains the per-checkpoint decision above.

The second check verifies all repository evidence manifests and Windows checkout
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
