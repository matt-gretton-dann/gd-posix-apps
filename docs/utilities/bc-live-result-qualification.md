# Live-result qualification

Issue #661 evaluates bounded CFG-aware reuse of dead result slots for #207 and
#197. The mechanism is optional and production defaults remain OFF. Correctness,
allocation recovery and verified compiler activation precede performance
collection; fewer slots alone do not establish an end-to-end improvement.

The runtime/resource checkpoint 5366b3a1 passed all 23 hosted checks and all
fifteen archives are retained with provider hashes and ZIP validation. All
twelve emitted mode records independently replay against their retained
compiler or MSBuild evidence. Controller/resource checkpoint c20f376d adds
matched post-sampling diagnostics and complete executable/input retention.
Native preflight found an older fixture that still expected only the three GD
executables in the arithmetic archive. Follow-up f3bbc91e requires Howard too;
all fifteen local controller tests pass. The corrected source now passes all
23 hosted checks. Its fifteen provider-hash/ZIP-verified native archives are
preserved, and all twelve emitted mode records independently replay against
their exact retained compiler/project evidence.
The final publication changes only documentation and the workflow; the immutable
source SHA is recorded in #661 before the push that starts collection.

## Fixed cohort

Run one declared cohort on ubuntu-24.04, macos-15 and windows-2025. Each platform
has two independent comparisons:

- Candidate default OFF versus accepted main
  92fb2c62c9293c5bc10a953776ab148733e1587a. The candidate is explicitly verified
  OFF. The old baseline uses AUTO and reports its unavailable liveness API as
  unknown; do not manufacture evidence that its source supported the option.
- Independently built OFF and ON roles at the exact same candidate source.
  The base is explicitly verified OFF and candidate ON. Keep each platform's
  existing typed-result default; this measures the mechanism in its real
  ordinary configuration, not a forced generic-only mode.

Each comparison has separate arithmetic, full-math and held-out-math jobs,
for eighteen jobs total. Arithmetic includes existing full/held-out broad
selection, nine paired measurements, one warmup, fixed seeds, existing bounded
confirmation rules and the declared sparse supplements. Math retains fifteen
paired measurements, three warmups, a 0.25-second minimum repeated duration,
600-second child bound and 18,000-second controller budget per preset. The job
limit is 360 minutes. No observation changes the workload selection, source,
sample counts or confirmation rules. Preserve every incomplete or failed run;
never restart an unfavorable, noisy, short or timed-out cohort for acceptance.

Arithmetic A/A independently builds the base source and mode. Math A/A
independently builds the candidate source and mode. All ordinary and profile
builds finish before elapsed collection. Explicit liveness modes require
recorded production compiler/project evidence, not cache keys or test objects.
The ordinary source, pinned Howard, platform toolchain, flags, libraries and
binary identities remain bound to each role; no cross-host ratio pooling.

## Resources, startup and archives

After all ordinary elapsed stages, collect matched software counters and five
alternating cold-process RSS pairs for each fixed representative. Instrumented
durations are discarded. Compare result acquisition, temporary-analysis and
retained-map allocations without converting missing historical counters to
zero. These are selected-container counts, not total process malloc. RSS pairs
are descriptive native observations, not confidence bounds or steady-state
retention estimates; unavailable adapters remain explicit. Existing native
math CPU/RSS diagnostics remain separately recorded after elapsed stages.

Arithmetic representatives cover startup, loop, calls, recursion, arrays,
sparse arrays, streaming/file transport and temporary chains. Also retain the
finite ascending/descending function-frame diagnostics: eight distinct plans,
array parameters, early-return and dense paths, with independent result sums
and escaped array value 7. These are resource diagnostics rather than elapsed
corpus substitutes. Math representatives use fixed one-shot regime cases near
scale 100 plus atan at scale 20, with -l on both ordinary and profile roles.

After timing, archive all ordinary binaries, original argv/build metadata and
checked external input copies. Arithmetic includes base, candidate, independent
A/A and Howard. Math includes base, candidate, independent A/A and both primary
Howard standard-library and ordinary portable-library recipes. Preserve exact
compiler commands/native projects and source-library hashes/copies. Native
section/disassembly output is diagnostic; unavailable tools have an explicit
binary/hash fallback. Do not rebuild a missing measured binary after the fact.

Each arithmetic job then binds its exact three ordinary executables to a fresh
source/host-specific startup study under #659. Each study retains the fixed
60 two-stage null blocks, candidate stages, clock qualification, raw observations
and empirical sensitivity/precision checks described in
[the startup protocol](bc-startup-calibration.md). Its 2,400-second budget and
five-second child bounds remain fixed. Neither the earlier pilot nor another
candidate's startup study qualifies this source or host. A calibrated result of
no statistically significant startup regression observed is not equivalence.

## Review and adoption

Apply [the #653 policy](bc-performance-acceptance.md) to every substantive row,
preserving raw decisions separately from advisory acceptance. The fixed primary
Howard comparator must qualify a competitive-margin exception. Startup and
one-shot latency cannot use that exception. A benefit in one class does not
erase a loss or inconclusive result elsewhere. Review full/held-out elapsed,
controls, startup, counters, retained storage, cold RSS and executable size
before any adoption. Green collection jobs alone do not establish acceptance.

Fresh generic/typed profile-OFF runtime coverage at c20f376d remains separately
source-bound in [the planner evidence](bc-live-result-slots.md), including the
resource-accounting source. Profile-ON accounting has separate native asserted
contracts rather than an LLVM coverage claim. Remaining feasible coverage and fault obligations
remain open. Keep the generic fallback and default OFF unless qualification
supports adoption. Do not close #661, #207 or #197 on infrastructure or partial
performance evidence. The rejected frozen #658 source and all adverse evidence
remain unchanged; its missing math A/A executable cannot be recreated as
original-run evidence by this cohort.
