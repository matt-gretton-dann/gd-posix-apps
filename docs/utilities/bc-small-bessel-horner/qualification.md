> CORRECTNESS-ONLY CORRECTED-SETUP PREPARATION. No new timing cohort is active.
> The historical protocol below records failed source18a25510. All its
> original failures and inconclusive arithmetic evidence remain preserved.
> Preparation is based on main3beebb14; its own CI and all original21
> outcomes must be reviewed before a separate final source/protocol declaration.
> The qualification job is disabled and ordinary timing workflows exclude
> this branch. The mathematical runtime and all188 input payloads are unchanged.

# Finite backward Bessel first qualification

Refs #217, #197 and prospective issue comment 6748002125. This is a new
algorithm study, not a retry of the guard-only Bessel or scalar-binding
experiments. Their original results remain separate and unqualified.

The mathematical implementation remains `4a5d0122`, based on accepted main
`1281233d822f090668a3327760018bc0fd5b6c39`. Corrected preflight `50756c21`
passed all 41 non-skipped checks. Thirty original ZIPs and six decoded core
build logs are preserved; twelve native archives physically retain every
declared source/generated input and tested image. The older `3264d78d`
archives' omitted workflow copy remains explicitly incomplete. It is not
reconstructed or substituted for the corrected originals.

Only ordinary portable bc math source changes at runtime. VM, Number,
coefficient storage, accepted defaults and multiplication split 126 remain
unchanged. Native function/scalar defaults remain OFF. Both generic regime
and rounding-uncertainty fallbacks remain enabled. Source/code size, library
loading, one-shot latency, selected allocations, retained storage and RSS
can offset any reduction in arithmetic work. No benefit is established.

## Frozen inputs and job boundaries

Publish only after pinning the final committed head, this protocol, workflow
and source hashes. The candidate is the PR head, never its synthetic merge.
Only original attempt 1 is eligible. Retain every failed, partial, slower and
inconclusive outcome. Do not retry for acceptance, replace hosts, pool
effects across hosts/sources, tune after observations or omit adverse cases.

The workflow has 21 jobs: Linux/x86-64, Windows/x86-64 and macOS/arm64, each
with broad arithmetic and six math groups (full/held × low/middle/high).
Arithmetic retains all 66 original cases in both presets, with all 132
historical program/parameter identities. Keep 31 measured pairs and the
existing startup, cold, sparse-long and sparse-latency controls. Historical
cases are exposed controls, not newly untouched holdouts.

Each math preset retains all 60 historical payloads unchanged. Append five
Bessel classes at scales 20, 100 and 101, in one-shot and repeated modes:
order zero/small argument; order one/small argument; order one/256-place
argument; argument above four/generic control; order two/generic control.
Use Python Random seed 217709 for full, 217710 for held, in this draw order:

1. Order zero: `randrange(1,4)` integer part, then `randrange(10**6)` as six
   zero-padded fractional digits.
2. Order one: integer part one, then six digits from `randrange(10**6)`.
3. Long order one: integer part two, then 256 individual `randrange(10)` digits.
4. Generic argument: integer part four, then six zero-padded digits from
   `randrange(1,10**6)`, ensuring an argument strictly greater than four.
5. Generic order: order two, integer part one, then six digits from
   `randrange(10**6)`.

Add the two existing fine midpoint inputs at scale zero in both modes as
four explicitly exposed fallback controls. They are not fresh holdouts.
There are 94 cases per preset. Partition once by scale: low <=20 (34),
middle 21..100 (30), high >=101 (30). Their sets are disjoint and union 94.
The source-bound generator, inputs, forward-series oracle and replay bytes
are tested independently of the backward runtime polynomial. Exact forward
rational tails are enclosed geometrically, then rounded outward to compact
decimal rational bounds. Every result and caller state must pass its oracle.

Keep 31 measured math pairs after three checked warmups, .25-second repeated
duration calibration, unchanged schedule/bootstrap/confirmation seeds and
rules, 600-second child bounds and a 20,000-second controller budget. Jobs
have a 360-minute total deadline, including build, diagnostics and upload.
Partitioning bounds job size; it does not authorize dropping a failed group.
All roles replay the same calibrated program/oracle bytes and invocation
count. Calibration is not acceptance. No local production elapsed benchmark
or native CPU sampling is allowed.

## Builds, resources and latency

Use native Release builds, unchanged automatic IPO policy and pinned Howard
7.0.3. Keep primary Howard standard and ordinary portable-source roles
distinct. Preserve actual compiler/runtime/SDK identities, flags, libraries,
executables, generated data, commands, input bytes and code-size observations.
Do not enable unqualified optional backends, tuned crossovers, PGO or paid
runners. Verify the proposed ordinary configuration; an unused cache flag or
older correctness result is insufficient.

Collect matched base/candidate instrumented counters and five cold native
RSS pairs only after ordinary elapsed work. Every math group retains its
16 historical resource representatives plus the group's additional one-shot
inputs from the exact declared corpus: 7 low, 5 middle, 5 high. The repeated
historical diagnostics in different groups are explicitly duplicated
observations, not independent elapsed evidence to pool. Retain actual
ordinary/profile images, source/configuration identities, programs/oracles,
selected allocation/live/peak counters and RSS. Counters omit parts of VM,
stack and process storage; cold RSS is descriptive. Instrumentation may
change Apple's accepted scalar argument-forwarding path.

The arithmetic resource suite retains 21 representatives. All nine programs
shared with ordinary timing must match the actual preset programs/oracles;
the remaining lifetime/cache probes are separately labeled diagnostics.
After arithmetic measurements, bind the unchanged startup study to those
exact base/A/A/candidate images and hashes: 60 blocks, two 151-pair stages,
2400-second overall and five-second child bounds. Do not waive inconclusive
startup or substitute an older study, identical control images or a Howard
margin. Preserve one-shot mathematical latency observations separately.

## Acceptance and timebox

`benchmark-policy.py` remains unchanged, LF SHA-256
`f92c5ce31e9262547b3766cdbd4f1b637d46ae053ec27f4e8dba3520d466368a`.
Keep the 5% Linux/Windows per-change cap and existing macOS rule. The stricter
approved Howard-margin exception requires upper candidate/primary-Howard
bound <=0.90, passing controls/measurement checks and a meaningful other-class
gain. The competitive target is separately median <=0.90 with nominal paired
95% upper <1 for every substantive class. Report every miss. Do not waive
inconclusive controls, duration, startup, adverse latency or resource review.

Independently replay original build/source/feature bindings, actual image and
input hashes, schedules, checked rows, intervals, labels, resource inputs,
counters/RSS and startup. Review code size and all neutral, losing and
inconclusive classes. All exact-head native correctness/CodeQL checks must
also be green. Green CI alone is not performance qualification or adoption.

The hard cap is 864,000 TOTAL recorded goal seconds, including all preceding
implementation, queues and review. Keep at least 20 GiB free locally. Stop
qualification at the cap without extension, weaker gates or false completion.
If qualified, merge only the exact green head, immediately update the primary
checkout to origin/main and preserve that merge's own CI. Otherwise preserve
the unmerged source and every original outcome. Close only completed issues;
#197/#200/#207/#217 remain open where their goals are unmet.
