# Native function activation qualification

This prospective #683/#207/#200/#197 cohort compares the opt-in implementation
accepted in #684/#685 against its default activation. It is not approval to merge
an unqualified result. Source, this protocol and the workflow are frozen before
publication; the issue declaration records their exact commit and byte hashes.

The baseline is `acf97f7f9e93b7ecf2f4a9e16c2066807f9594b2`, with native functions
and native scalar loops OFF. The candidate changes the function option's default
to ON and retains scalar loops OFF. The kernel, shared planner, Number interface,
math library and numeric algorithms are unchanged from that accepted source.
Additional changes bind build/resource provenance and the declared workflow.
Supported x64 Windows/POSIX and ordinary Apple ARM64 targets compile a backend;
other targets retain generic execution. A target that falls back cannot supply
native acceptance evidence for this three-platform study.

## Fixed comparisons and stopping rules

One PR dispatches the original nine jobs: Ubuntu 24.04 x64, Windows 2025 x64 and
macOS 15 ARM64, each running arithmetic, full math and held-out math. The source
checkout is the exact PR head. The candidate must have its compiled VM definition,
one matching backend and shared planner verified before sampling. Arithmetic
base/A/A use the disabled baseline and its candidate is enabled. Math GD/A/A
use the enabled candidate while its baseline remains disabled. Every role records its own source/build policy; a cache flag
alone is insufficient.

Use the complete existing full and held-out corpora and independent oracles,
including sparse arithmetic qualification. Arithmetic retains nine paired
samples, one warmup and the existing fixed corpus/order/bootstrap seeds. Math
retains fifteen paired samples, existing A/A and confirmation rules, bounded
per-role calibration to 0.25 seconds, a common iteration count, a 600-second child
limit and an 18,000-second controller limit. Each job has a 360-minute bound.
No input, class, seed, iteration cap or nominal interval changes in response to
the candidate. One-shot observations stay descriptive and excluded from
substantive throughput acceptance. The primary comparator is Howard 7.0.3 at
the committed builder revision; Howard with the GD library is secondary.

Retain all failures, short-duration cases, controls, confirmations and adverse
results. No favorable restart, cancellation, source mutation or cross-study
pooling. A rerun attempt fails before sampling. Failed or incomplete collection
does not establish acceptance, and missing measured images are never rebuilt.

The user's ten-day TOTAL goal-time cap includes implementation, queues, runs and
review. Check the authoritative goal clock before publication; the remaining
time must accommodate the job bounds, queue risk and final review. Do not extend
the cap autonomously or close unmet issues when it is reached.

## Startup and resources

Each arithmetic job subsequently runs the existing startup-null study on those
exact base/A/A/candidate executables. Retain all 60 blocks, initial/confirmation
stages, 151 pairs per stage, source-bound policy and input/output identities.
Its separate 2,400-second collection bound and five-second child deadline apply.
No throughput cap or Howard-margin exception waives startup qualification.

After ordinary elapsed children finish, collect matched instrumented software
counts and five cold-process RSS pairs per declared resource case, with seed
661197. Retain actual ordinary and diagnostic images, inputs, reports, recipes,
caches and compiler/project evidence. Both modes must agree on source, compiler,
common flags, selected backend and shared planner inputs. Preserve the distinct
profiling execution-policy records: Apple argument forwarding can differ, so
selected counts do not establish ordinary execution-path identity.

All existing arithmetic/frame/array/math resource cases remain. Add fixed
full/held function-cache cases with all 26 letter-indexed caches alive together
and 64/53 rounds, requiring 1,664/1,378 native completions. Call resources must
complete actual native calls. Fixed cubic-overflow inputs 999999999/999999997
must attempt once, decline without committing, and produce the exact generic
result. Disabled roles must not report native events. These inputs and
expectations are fixed before observation. Compiler buffers, ownership headers
and executable mappings are outside the selected allocation counters; cold RSS
observes process memory separately and remains descriptive.

## Acceptance and reporting

Keep `bc-performance-acceptance.md` and its committed policy helper unchanged.
Linux/Windows retain the per-change five-percent regression limit and macOS its
existing zero-percent substantive limit. Any excess-cap exception still needs
the primary Howard 95-percent interval upper bound at most 0.90, worthwhile
gains elsewhere and valid controls, durations and samples. The competitive goal
is separately median GD/Howard at most 0.90 with the nominal upper bound below
one for each substantive class. Do not substitute that goal test for the
stricter regression exception. All collectors remain advisory.

Review exact-head correctness, portable fallback, signal/fault recovery, all
raw timing and policy rows, startup controls, one-shot latency, allocation/RSS,
code size and integration before adoption. Report every unsupported, missing or
inconclusive requirement. Green workflows alone do not qualify activation or
complete #197. Source #685's own main evidence must be complete before this
candidate is published. Only this topic's redundant narrow PR timing jobs are
replaced; main, scheduled and manual trends retain their existing behavior.
