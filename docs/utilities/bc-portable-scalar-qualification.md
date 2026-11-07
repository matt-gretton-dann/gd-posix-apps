# Portable scalar qualification

Issue #666 evaluates portable guarded integer execution for #207 and #197.
`ENABLE_BC_PORTABLE_SCALAR` defaults OFF. The bounded unsigned scalar subset,
private execution, staged publication and generic fallback are described in
[the implementation notes](bc-portable-integer-regions.md). No benchmark,
function name or library source is recognized. Unsupported workloads, including
arbitrary-precision factorial recursion, retain ordinary execution.

## Independent source and correctness evidence

This experiment does not reuse acceptance from the rejected liveness source
#665. That source and all adverse or inconclusive evidence remain unchanged.
Its collection infrastructure is reused without its liveness runtime code.

Integrated checkpoint `c9c0f67aa514ae520407e43b499bd41491e50749` passed all 35
native checks, with all 27 provider archives retained and independently replayed.
Actual production compiler/cache/source/test bindings cover Linux, macOS and
Windows, all four typed/profile combinations, and language extensions ON/OFF.
Every Linux and Windows profiled mode exercised two actual staged-conversion
allocation failures with complete replay and preserved unrelated state. Every
Mac profiled mode explicitly recorded no ordinary allocation for that fixture.
These are finite correctness and fault observations, not performance evidence.

Test commit `6cb3a6e1` adds independent malformed-operand rejection fixtures.
Local Apple Clang 21 Release coverage with typed ON and profiling OFF exercises
262/262 executor lines and 26/26 MC/DC conditions. Its 154/156 branch outcomes
exclude only two switch defaults not constructible through successful private
lowering. The full 360-case unit run and eight focused CTests are independently
bound to exercised binaries, source hashes, compiler settings and raw profiles.
VM asynchronous publication-interrupt guards remain unexercised in that local
coverage run; native allocation fault logs are separate evidence, not a
cross-platform coverage percentage.

Collector checkpoint `768435f8d9857b1c0c547c7bb6ef04f78f95ed13` adds source-bound
production ON/OFF verification and matched resources. All 35 checks passed; all
27 archives and 24 actual native mode bindings are retained and replayed. The
following test-first
portability fix `f6b62b825b8c0cf48a631b302bd96f3ac37faa6a` maps encoded scalar
indices through the spelled alphabet, matching Letter rather than assuming
contiguous character codes. Available ASCII hosts cannot reproduce the affected
encoding, but the test checks all 26 guards and all-zero activation there.
All 35 native checks for that fixed source passed before protocol publication;
all 27 provider archives and 24 actual mode bindings are retained and replayed. The final publication must retain
runtime-file byte identity with that preflight or refresh the affected evidence.
Fresh local coverage at the fixed source retains 262/262 executor lines and
26/26 MC/DC conditions; raw profiles and exercised binaries remain separate from
the earlier checkpoints. VM asynchronous publication-interrupt guards remain
unexercised in this local coverage run. The final controller
moves the legacy candidate-only allocation diagnostic after elapsed collection;
all other counts, selectors and budgets below remain fixed.

Before the push that starts collection, record the immutable final source SHA
and protocol/workflow hashes in #666. Do not edit that source or restart its
cohort to replace unfavorable, noisy, incomplete or failed observations.

## Fixed cohort

Run one cohort on ubuntu-24.04, macos-15 and windows-2025. Each platform has two
independent comparisons:

- Candidate explicitly OFF versus accepted main
  `f704340ae3e8b4e57f8abfe18d2c98809938b78d`. This baseline predates the portable
  option: use AUTO and record its mode as unknown, without manufacturing OFF
  evidence from an ignored cache key.
- Independently built OFF and ON roles at the exact same candidate source,
  with explicit verified requests and each platform's ordinary typed-result
  default. The independent Apple emitter is disabled and its absence verified.

Each comparison has arithmetic, full-math and held-out-math jobs: eighteen
jobs total. Arithmetic retains the broad full/held-out selections, nine pairs,
one warmup, fixed seeds, existing bounded confirmation rules and sparse
supplements. Math retains fifteen pairs, three warmups, a 0.25-second minimum
repeated duration, a 600-second child bound and an 18,000-second controller
budget per preset. Each job is bounded to 360 minutes. No observation changes
source, workload selection, sample counts, budgets or confirmation rules.

Arithmetic A/A independently builds the base source and mode. Math A/A
independently builds the candidate source and mode. All ordinary and diagnostic
builds finish before elapsed sampling. Explicit modes require production
compiler/project definitions and the matching library source inventory;
standalone unit objects and cache keys cannot establish activation. Keep source,
pinned Howard, platform/toolchain, flags, libraries and binary identities bound
to each role. Do not pool ratios across hosts or revisions.

## Resources, startup and archives

After every ordinary elapsed stage has finished, collect matched software
counters and five alternating cold-process RSS pairs for fixed representatives.
The legacy candidate-only allocation diagnostic and native math CPU/RSS
wrappers also follow elapsed collection. Instrumented durations are discarded.
Compare selected-container allocation categories without turning absent
historical counters into zero. New executor event counts are separate from
interpreter dispatch; fewer dispatches alone do not establish less total work.
RSS repetitions are descriptive, not confidence bounds or retained-memory
estimates. Unavailable adapters stay explicit.

Arithmetic representatives cover startup, loop, calls, recursion, arrays,
sparse arrays, streaming/file transport and temporary chains. Also retain
finite ascending/descending frame diagnostics with eight distinct plans,
array parameters, early-return/dense paths, independent sums and escaped array
value 7. These are resource diagnostics rather than elapsed corpus substitutes.
Math representatives use fixed one-shot regimes near scale 100 plus atan at
scale 20, with `-l` on both ordinary and profile roles.

Archive all measured ordinary binaries, original argv/build metadata and checked
external input copies after timing. Arithmetic retains base, candidate,
independent A/A and Howard. Math retains base, candidate, independent A/A and
both primary Howard standard-library and ordinary portable-library recipes.
Preserve compiler commands/native projects and source-library hashes/copies.
Section/disassembly output is diagnostic; absent tools require explicit
binary/hash fallback. Do not rebuild or reconstruct a missing measured binary.

Each arithmetic job then binds its three exact ordinary executables to a fresh
source/host-specific startup study under #659. Retain the fixed 60 two-stage
null blocks, candidate stages, clock qualification, raw observations and
sensitivity/precision checks in [the startup protocol](bc-startup-calibration.md).
Keep its 2,400-second budget and five-second child bound. Earlier pilots or
another source's startup study do not qualify this candidate or host. A result
of no statistically significant startup regression observed is not equivalence.

## Review and adoption

Apply [the #653 policy](bc-performance-acceptance.md) independently to every
substantive row. Preserve raw decisions separately from hashed advisory
acceptance. Linux and Windows retain their 5% limits; macOS retains its zero
regression limit. An otherwise excessive regression requires passing A/A,
a valid paired nominal 95% candidate/primary-Howard interval with upper bound
at most 0.90, the declared pairs/duration and a worthwhile gain elsewhere.
Startup and one-shot latency cannot use that exception. The primary math
Howard comparator remains the standard library; do not select the favorable
portable-library ratio after observing results.

A gain in one class cannot erase a binding loss or inconclusive control in
another. Review full/held-out elapsed, A/A, startup, allocations, retained
storage, cold RSS and executable size before adoption. Green collection jobs
alone do not establish acceptance. Retain default OFF and the generic fallback
unless qualification supports adoption. Do not close #666, #207 or #197 on
infrastructure, software counters, partial evidence or an unqualified speedup.
