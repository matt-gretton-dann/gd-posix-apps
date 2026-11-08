# Private scalar activation qualification

This #207/#200/#197 experiment compares accepted main
`e7aef88bda2357422df785b9f6496d534a105bb9` with the private scalar activation
prototype. The accepted base's twelve own-main checks and three arithmetic
archives have completed review. Candidate ordinary and profiling builds
explicitly enable `--private-scalars ON`; base builds retain AUTO. The C++
option stays OFF by default on every platform.

Verified closed scalar functions can leave ordinary dynamic bindings untouched,
using lazy Number cells in stable contiguous LIFO chunks. Free bindings, arrays,
output, non-self calls, full checks and the operand-stack engine retain their
generic path. The same ordinary math library source is used, without function
name recognition or native transcendental replacements. Argument staging and
saved-specials behavior remain. See `bc-activation-frames.md` for the proof and
storage boundaries.

This is a risk-bearing architecture experiment, not an established speedup.
The active-frame check can slow unrelated scalar programs; chunk padding and
retention can increase memory. Narrow calls can lose despite wider functions
winning. Report every class and every original outcome. Frozen #678 remains a
separate source and cohort; finish its review/disposition before integrating
this topic. Never transfer its gains, controls or resource observations here.

Record the final immutable source, workflow, protocol and acceptance-helper
hashes in #207 before publication. Never edit that source, cancel its adverse
outcomes or restart a cohort for acceptance. No local production elapsed
benchmark is permitted.

## Correctness and activation preflight

Predecessor `3dca57a4` passed nine enabled native configurations: Linux, macOS
and Windows, each ordinary, typed and profiling. Their retained source-bound
logs record 87 passing lifetime/signal/math CTests, plus VM/storage and CLI
oracles. Local enabled, typed/profile and default-disabled builds also pass.
Tests cover dynamic fallbacks, exact zero autos, nested argument snapshots,
recursion, redefinition, typed execution, stable cell addresses, reuse and
recursive allocation failures. Profiling verifies eligible calls separately
from fallback calls. These results do not qualify another source or performance.

The prospective build verifier passes 29 tests, including native project
retention, ignored options, contradictory/zero defines and ambiguous VM records.
It verifies ON against the actual VM compilation, not merely a cache key.
Since the C++ switch uses a presence guard, OFF must be absent, not defined as
zero. Older source under AUTO remains explicitly unknown. The verifier also
accepts all nine retained predecessor compilation databases as enabled.

Twenty resource fixtures, sixteen arithmetic-controller tests and twenty-four
math-controller tests pass. Four fixed private-frame programs also pass exact
ordinary/profile output checks and expected private-call counts. All of this
is correctness/counter evidence, without elapsed acceptance. The final frozen
head must pass its own complete hosted checks. Each elapsed job additionally
runs CLI and VM CLI oracles for all three ordinary GD roles before sampling.

## One declared native cohort

Run `ubuntu-24.04`, `macos-15` and `windows-2025`, each with arithmetic,
full-math and held-out-math jobs: nine jobs. Keep maintained default extension,
typed/image/IPO and arithmetic settings except explicit candidate private
storage. Retain actual options and compiler records. Arithmetic A/A independently
rebuilds base under AUTO; math A/A independently rebuilds candidate under ON.
Candidate resource profiles must also prove explicit ON. Do not mix these roles.

Arithmetic retains 66 full and 66 held-out cases, nine measured pairs, one
warm-up, seeds 197200/197201, bounded confirmations and fixed sparse supplements.
Keep its 900-second command bound and existing sparse-stage bounds. Math retains
60 cases per preset, fifteen measured pairs, three warm-ups, seed 1972170, a
.25-second repeated duration target, 600-second child bounds and an 18,000-second
controller budget per preset. Freeze iteration counts after separate calibration
and before paired samples; preserve subsequent duration shortfalls. Fixed A/A
and candidate confirmations use seeds 1972171 and 1972161. Each job has a
360-minute outer bound and must be attempt one.

Finish all ordinary and instrumented builds before sampling. Bind source,
pinned Howard 7.0.3 (`c8dabac65d39fd527a9264a5e505a99b1d8df3d8`), compiler/SDK/
standard-library identity, flags, images, argv and library bytes. Check outputs
and state for every role and warm-up. Do not pool hosts, sources or modes, choose
a favorable Howard role, or hide a losing class behind an aggregate.

## Resources, startup and preservation

Use `--compare-resources`. Following all ordinary elapsed stages, matched
instrumented builds collect selected-container allocation/refcount/clone events;
five alternating cold ordinary-image RSS pairs follow. Arithmetic representatives
cover startup, loops/calls/recursion, arrays and sparse storage, streaming/file
input, temporary chains and the existing array-frame reuse cases. Additional
closed scalar cases visit widths 1, 13 and 26 in ascending and descending order,
with full depth/rounds 24/16 and held-out depth/rounds 21/14. These cases expose
private activation demand, padding and historical width reuse. They are resource
diagnostics, not extra elapsed workloads selected after results.

There are thirteen arithmetic resource cases per preset and sixteen math cases
per preset: 174 cases across the nine jobs. Math keeps the scale-100 regimes,
scale-20 arctangent boundary and all five Bessel regimes at scales 20 and 100;
each role uses its own `-l` source. Discard instrumented elapsed values.

Keep both profiling execution policies. Common source/compiler/options do not
establish path identity: Apple arm64 profiling uses scalar packing while ordinary
forwarding can be eligible. Private-storage policy must match its own ordinary
role even when another execution policy differs. Counters describe the
instrumented path and selected containers, not all malloc calls, ordinary-path
allocations or RSS. Missing older event counters remain missing, not zero.
Cold RSS uses ordinary executables separately and does not establish retained
memory or confidence bounds. Keep unavailable adapters, incomplete pairs and
adverse values.

Preserve actual instrumented images/recipes under `profile-images/`, separately
from ordinary `codegen/` images, recipes and declared inputs. Retain hashes when
native inspection is unavailable. Native project evidence for the private switch
is retained under `private-scalars-policy/`; it is not a comprehensive build/cache
snapshot. Never reconstruct a missing measured binary.

After arithmetic, bind its exact base/A/A/candidate executables into the unchanged
startup study: sixty two-stage null blocks, 151 pairs per stage, candidate stages,
fixed seeds, a 2,400-second budget and five-second child bound. Preserve timer,
precision/sensitivity and null-bias qualifications. Earlier studies cannot
substitute. Startup does not qualify mathematical one-shot latency.

## Review and integration

Keep `bc-performance-acceptance.md` unchanged. Linux/Windows allow 5% per change;
macOS retains its existing cap. A beyond-cap exception requires primary Howard's
upper 95% bound at most .90, valid controls/samples/durations and a worthwhile
gain elsewhere. Startup and one-shot rules stay separate. The competitive target
(median at most .90 and nominal upper bound below one with valid controls and
durations) differs from the stricter regression exception. Retain original
collector labels alongside advisory policy labels.

Independently replay raw counts, order/check identities, program/oracle bytes,
source/recipe/image/input bindings, intervals and decisions. Review correctness,
startup, resources, code size and integration. Green collection cannot qualify
incomplete or inconclusive evidence. Preserve the older threaded workflow
separately, including its missing measured images/provenance limits. Replace
only this topic's redundant narrow PR timing; main/manual/scheduled trends stay.

Merge only a completely reviewed green candidate. Immediately synchronize the
primary checkout to origin/main, then review that merge's own CI before another
integration. #200/#207/#217/#197 remain open until their entire scopes and
competitive obligations have authoritative completion evidence.
