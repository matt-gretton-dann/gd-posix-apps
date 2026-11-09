# Frozen scalar-local binding qualification — #707 / #197

The unmerged candidate proposes default ON over accepted main
`1281233d822f090668a3327760018bc0fd5b6c39`, with explicit OFF preserving the
accepted save/restore path. No native opcode, math-source, coefficient-storage,
square, word-tier or prefix-result experiment is imported. Product split stays
126; native function/scalar defaults stay OFF, arrays retain accepted hybrid
storage, and Number/accepted coefficient representation is unchanged.

Immutable preflight `515e47eac370815c9307ec2c603b00348d6cf665` passed all 46
original checks. All 36 archives were preserved and independently reviewed,
including ten enabled ordinary/profile/fallback native configurations across
Linux, Windows and macOS plus Linux Clang ASan/UBSan. Actual source/cache/CDFs,
VM selection and image bytes are verified. Explicit platform/CLI skips and
missing measured images in older support workflows remain recorded. Its
supporting elapsed results use default OFF and do not qualify this backend.
The runtime VM/helper, Number, accepted storage and ordinary math-source bytes
remain identical to that preflight; hashes are adjacent. Changing the proposed
default to ON measures the configuration adoption would actually merge.

This architecture leaves caller Numbers in place and rebinds selected scalar
names to stable depth storage. It can reduce value traffic but adds indirection,
local capacity and pointer/stack metadata. Benefits and regressions are unknown.
Counters, RSS and executable size must remain visible; no layout or ownership
fixture substitutes for throughput evidence. Prior #705's adverse enabled
results and all earlier unqualified cohorts remain separate and unchanged.

## One original immutable cohort

Declare the final committed head, protocol/workflow and backend hashes before
publishing. PR head is the candidate, not a synthetic merge. Workflow
`.github/workflows/bc-scalar-bindings-qualification.yml` launches nine jobs:
Linux/x86-64, Windows/x86-64 and macOS/arm64, each with arithmetic, full math
and held-out math. Only original attempt 1 is eligible. Preserve failed,
partial, slower and inconclusive outcomes; do not cancel, retry for favorable
data, replace hosts, pool sources/hosts, tune inputs or change this protocol
after results. A subsequent material experiment requires its own prospective
source/declaration and cannot repair this cohort retroactively. Retain every
same-head support check/archive separately, with missing assets explicit.

Use source-built native Release GD and Howard 7.0.3 through the unchanged
comparator builder and IPO policy. Distinguish primary Howard standard from
the identical ordinary portable-source role. Preserve source/compiler/runtime/
SDK identities, actual executable/input bytes, commands, flags and code size.
No tuned crossover, PGO, paid runner or privileged math assistance is used.

## Workloads and controls

Both arithmetic presets retain all 66 accepted cases and all 132 historical
program/parameter identities. Append only the three frame classes declared in
issue #707 comment 6742027444 before generation: wide locals, dynamic inherited
writes and recursive heap snapshots. Details, draw order, fixed repetitions,
independent RNG and checksum formulas are in [fresh-arithmetic.md](fresh-arithmetic.md).
Each preset has 69 cases. Historical cases remain exposed controls, not new
untouched holdouts. Full operand seed is 197200, held-out 197201.

Keep 31 measured arithmetic pairs after one checked warm-up for A/A and
candidate/base stages, with unchanged schedule and confirmation seeds/rules.
Sparse long retains 31 pairs and its duration requirement; sparse latency
retains 101 pairs. Startup and fixed cold/latency cases do not become winning
substantive classes. Validate every output, exit status, stderr and deadline.

Each math job retains all 60 full or held-out one-shot/repeated cases, primary
Howard and the identical portable-source comparison. Keep 15 measured pairs,
0.25-second calibrated repeated duration, 600-second child deadline and
18000-second controller budget. Calibration is not acceptance. Keep source/hash
bound inputs and actual library bytes; no recognition or special callbacks.

Before elapsed work, `--require-candidate-scalar-bindings` binds ON to the real
VM compilation and three retained source inputs for ordinary/profile candidate
roles. The pinned baseline must prove legacy absence with VM/CMake bytes and
compilation evidence. Verify CDFs or native Visual Studio Release projects;
retain cache, sources and verification bytes. An option alone is insufficient.
This is configured/source proof, not per-input entry or ordinary/profile dynamic
path equivalence. Native correctness is separate evidence, not a timing result.

## Resources and startup

Each arithmetic preset has 24 resource cases: the 21 original representatives
and all three fresh frame classes. Twelve common program/oracle/parameter
payloads must match elapsed generation exactly, including held-out seed197201;
twelve extra lifetime/cache probes stay labeled diagnostic inputs. Each math
suite retains its 16 declared resource representatives. Separate profile roles
must match their ordinary source, compiled binding mode and source hashes.

Retain counters separately from ordinary elapsed timing and five declared cold
native RSS pairs per case. Keep raw inputs/oracles, output identities, actual
profile images and allocation/live/peak counters, RSS and executable/code sizes.
Counters cover selected containers, not all malloc, retained memory or pages.
Cold RSS is descriptive. Apple's accepted scalar argument forwarding differs
under instrumentation; do not assert equivalent dynamic paths.

After arithmetic timing, run unchanged candidate-specific startup assessment
using its exact retained base/A/A/candidate images. Bind hashes first; keep all
60 blocks, both 151-pair stages, input bytes and calibration, with 2400-second
overall and five-second child bounds. Apply the existing empirical assessment;
do not invent an all-block precision requirement or waive inconclusive startup.
Older studies, identical control images or Howard margins cannot substitute.

## Acceptance, budget and disposition

`benchmark-policy.py` stays unchanged, LF SHA-256
`f92c5ce31e9262547b3766cdbd4f1b637d46ae053ec27f4e8dba3520d466368a`.
Keep the 5% Linux/Windows per-change cap and existing macOS rule. An excess-cap
case can pass only through the approved stricter Howard-margin exception,
with passing controls/measurement checks and a meaningful other-class gain.
That exception requires upper candidate/primary-Howard bound <=0.90. The
competitive target separately remains median <=0.90 and paired 95% upper <1
per substantive class. Report every target miss; no aggregate hides one.
Do not waive inconclusive controls, duration, startup or resource review.

Independently replay original source/build/feature bindings, actual image
and input hashes, seeded order, checked raw rows, intervals, labels, resource
inputs/counters/RSS and startup studies. Report all losing/neutral/inconclusive
classes, Howard gaps and memory/code-size tradeoffs. Green CI is necessary,
not performance approval. Final source review and all same-head native
correctness/CodeQL checks remain separate requirements.

If incrementally qualified, merge the exact green head, immediately update
primary local checkout to origin/main, and preserve that merge's own CI.
Close only genuinely completed issues; broader #197/#200/#207/#217 targets
stay open where unmet. Otherwise preserve the exact unmerged source and every
outcome. No local production elapsed benchmark or CPU sampling is allowed.
All prior work, implementation, queues and review count toward the user's
hard 864,000 TOTAL recorded goal seconds. Stop qualification at that limit
without extension, weaker gates or false completion. Keep at least 20 GiB
local free space throughout evidence collection.
