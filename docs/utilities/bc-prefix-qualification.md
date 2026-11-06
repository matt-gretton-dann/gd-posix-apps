# Result prefix qualification

Issue #657 qualifies the larger result-lifecycle experiment in #651 for #207,
#217 and #197. Its correctness preflight at 69712134 passed all 18 native jobs;
ten contract archives retain the source and actual modes. This is correctness
and portability evidence, not evidence of a speedup. The rejected #650 cohort
completed before this new qualification; retain its losses under its own source.

The new cohort is fixed before collection. On Linux, macOS and Windows, compare:

- The candidate's default OFF build against accepted main
  92fb2c62c9293c5bc10a953776ab148733e1587a, to detect default-path effects.
- Independently built OFF and ON configurations at the exact candidate source,
  to measure the prefix mechanism. Only the candidate opts into prefix storage.

Each comparison has separate arithmetic, full-math and held-out-math jobs: 18
jobs total. Arithmetic includes the existing full and held-out broad selection,
nine paired measurements, confirmation rules and fixed sparse supplements. Math
retains 15 paired measurements, three warmups, the existing confirmation rules
and the primary Howard standard-library role, with a 0.25-second repeated-call
target, 600-second child bound and 18,000-second controller budget per preset.
The native job limit is 360 minutes. No result changes workload selection or
triggers a favorable restart. Preserve incomplete artifacts on failure.

All builds finish before elapsed collection. The mode verifier must prove the
actual VM compile definition, including native MSVC projects. Arithmetic A/A
uses a separate baseline-source build; math A/A uses a separate candidate-source
build with the candidate's mode. Profile builds are separate and must match each
ordinary role's source, compiler and declared execution configuration.

Collect comparative software allocations, initialized result slots, final live
bytes and native cold RSS pairs for the declared representatives. Ascending and
descending frame-plan reuse includes early-return and dense paths, with private
array state and independent exact output sums. Retain missing old counters as
unknown. Five cold RSS pairs are descriptive, without a confidence or steady
state claim. Retain the exact timed binaries and native section/disassembly
reports after ordinary elapsed children exit. Static layout does not attribute
CPU cost, and instrumentation timings do not measure Release speed.

Each arithmetic job then uses its same three ordinary executables for a fresh
candidate-specific startup study under the separately tested #659 protocol.
The earlier accepted-runtime null pilot cannot qualify this source or host.
The startup helper and ordinary collectors are bound to the PR's immutable
source. Preserve all null and candidate rows, including inconclusive outcomes;
the manifest is advisory and never declares merge eligibility. The collector
must be present before this workflow is published.

The standalone #660 pilot verified 171,288 checked observations. Linux and
Windows met its empirical criteria; macOS was inconclusive, with only 6/60
precise and 10/60 scaled-probe blocks. Those results belong to that pilot's
hosts and source. Do not transfer the passing results, relax the criteria or
repeat this candidate's study to obtain a preferred outcome.

Apply the #653 competitive-margin policy to substantive rows while retaining
raw regression labels. Startup and math one-shot latency remain separate and
cannot use the Howard exception. Review every losing or inconclusive class,
gain elsewhere, startup, resource counts, executable size and final integration
before adoption. A green workflow proves collection/checks completed, not that
the candidate meets the overall target. Refresh source-bound line, branch and
MC/DC coverage for the changed lifecycle paths and review their feasibility
ledger; passing assertions alone do not establish coverage. Keep the default
OFF generic fallback and do not close #651, #657, #207 or #197 on infrastructure
or preflight alone.

## Focused lifecycle coverage

Two separate Apple LLVM 21 Debug collections use extensions ON and OFF, prefix
ON, typed results OFF and allocation profiling OFF. The C++ source remains
identical to 69712134 while integrating the accepted startup collector. Each
runs 62 asserted VM/component cases (388,522 assertions) and 28 VM CLI tests.
They produce 104 ON and 102 OFF raw profiles, in independent directories which
are never merged together. Build-time and exploratory profiles are excluded.
Matching LLVM tools report no export warnings.

| Extensions | Prefix helper scope | Executable lines | Branch outcomes | MC/DC conditions |
|---|---|---:|---:|---:|
| ON | Broad application and component templates | 40/40 | 13/16 | 0/0 |
| OFF | Broad application and component templates | 40/40 | 13/16 | 0/0 |
| ON | Application mappings, excluding component-only types | 27/34 | 9/14 | 0/0 |
| OFF | Application mappings, excluding component-only types | 27/34 | 9/14 | 0/0 |

The helper has only single-condition decisions, so zero tracked MC/DC conditions
is tool scope, not evidence of covered MC/DC. Retain the raw denominators. The
three unobserved broad branch outcomes are the debug assert-failure arms at
`result-prefix-buffer.hh` lines 39, 62 and 81: respectively nonempty preparation,
an access outside reserved capacity, and const access outside the constructed
prefix. They violate the helper's documented preconditions; these are not normal
semantic paths, and no assert-death coverage is claimed. Do not remove the checks
or call invalid indices to change the denominator.

Component-only throwing values and bounded allocators exercise preparation and
growth rollback and saturation. Those observations are not native allocation
fault injection. In the application instance, `ResultSlot` has statically
nonthrowing construction and valid active accesses stay inside reserved
capacity; its growth rollback cannot be entered by a valid access. Preparation
allocation failure remains a distinct native fault-injection gap. Const access
and some observer methods have no application mapping in this collection.
Do not treat the broad row as complete application-instance coverage.

The wider VM has the same focused raw counts in each independent configuration:
1,240/1,372 lines, 555/684 branch outcomes and 22/56 tracked MC/DC conditions.
These collections are deliberately limited to lifecycle/VM assertions; they do
not replace full application coverage, claim the remaining VM obligations
covered, or establish typed-mode or another platform's coverage. Raw profiles,
full LLVM JSON, annotations, compile commands and source hashes are retained
as local evidence. Performance
qualification and the broader #197 coverage review remain separate gates.
