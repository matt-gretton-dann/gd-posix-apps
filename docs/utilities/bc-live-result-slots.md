# Control-flow-aware result lifetimes

Issue #661 continues #207/#197. The generic/typed planner is an opt-in VM
experiment, enabled with `-DENABLE_BC_LIVE_RESULTS=ON`. Its production default
remains OFF; that build links the planner into unit tests only. No elapsed
improvement or adoption is claimed. Linux and Windows retain their typed
defaults; generic-only results cannot establish an improvement there.

The existing producer plan and arithmetic consumption proof remain independent
fallbacks. They compact producers and can reuse a consumed first operand inside
one straight-line block. The new experiment instead computes full-region
backward liveness after complete schema, range, availability and control-flow
verification. Branch targets and fallthroughs retain original source PCs.
Definition registration skips its body; a separately registered body has its
own root and analysis. The planner does not replace entry verification.

For each instruction, `OUT` is the union of successor `IN` sets, and `IN` is
`USE union (OUT minus DEF)`. Iteration starts with empty sets and stops at a
fixpoint. A definition interferes with every live-out value and every explicit
operand read by that instruction. This deliberately keeps its destination
distinct from borrowed operands, including when another proof permits moving
the first operand. It does not infer death from a single static use or from
source order: backedges and forward references participate in the fixpoint.
Deterministic coloring reuses only noninterfering storage. The combined planner
colors numeric and other registers independently under the same work budget.
Equal indices in separate lanes do not alias. Every typed producer in the
execution region, including unreachable instructions, retains a valid lane
index because lowering validates all producers. Skipped function bodies keep
`no_register`; they are planned when registered independently.

The retained generic representation is the existing result map and count, with
zero as the empty sentinel. Unreachable generic instructions and skipped bodies
require no producer storage in this region. Typed plans use the existing two
lane counts and map, with no sentinel slot. No runtime liveness table, extra
activation header or changed instruction ordering is introduced. There is no literal-value
folding or assumption about runtime bases, scales, globals or function names.
Verified transient streams prepare once per execution; persistent function
bodies prepare on first use and retain only their selected layout. Full-check
execution does not request liveness analysis. Generic wide execution remains
available when compact or typed lowering declines. Redefinition installs a new
function owner and preparation state.

Result alternatives own numbers and shared array snapshots, or retain code-owned
string views and value wrappers. Explicit borrowed operands interfere with their
destination. The Apple Arm64 compact push/call fusion can additionally borrow
an argument whose slot becomes the call destination: the entire right-hand call
finishes before assignment overwrites the slot, and recursion uses separate
depth buffers. A scalar-prologue skip requires the existing independent proof
that no later operand/branch refers to that prologue. Root-zero liveness is
conservative for this restricted alternate entry. Keep these assumptions under
review if operations, fusions or entry rules change.

Analysis has hard limits of 4,096 instructions and 4,194,304 logical work units;
callers can lower either limit. Work counts node/word propagation, interference
and color-neighbor visits, and color probes. The temporary state uses at most
two instruction-by-word 64-bit matrices plus linear graph/color storage. These
are bounded construction costs, not retained-map bytes or process-RSS evidence.
Tiny once-only streams, unsupported or over-budget analysis and unrepresentable
payload arithmetic retain the complete supplied plan. A proposed layout must
reduce total slot and map payload, including its empty sentinel. An allocation
failure inside analysis also retains the untouched existing plan where possible;
caller-side construction of that original plan is outside this recovery.
The combined planner charges the existing caller-supplied headers, instruction
records, maps and lane payloads with checked arithmetic. Preserve a typed
family that was already strictly cheaper than the original generic layout;
compacting generic storage further is not a CPU-cost proof for demotion.
A replacement typed map must reduce that original typed payload. When no
profitable typed family was established, a newly selected typed candidate must
still beat the best generic retained cost strictly, with generic preferred on
ties. These are per-layout payload bounds, not buffer-capacity or RSS claims.
Both original candidates survive any declined analysis or failed optional
allocation, and no partial pair is published. An already minimal compact
producer map containing only its empty sentinel declines before graph allocation;
there is no active value to color, including skipped definition bodies.

The test-first scaffold compiled successfully and failed seven of eight initial
contracts while preserving its original-plan fallback. The initial implementation
passed nineteen contracts and 101,317 assertions. The typed test-first extension
failed four of five contracts before implementation. A bounded path oracle
independently compares logical value versions against physical slots, without solving
liveness or using the planner's result classifier. It explores both conditional
edges and repeated paths, with finite test-only step limits. Cases include dense
dead literals, repeated operands, joins, live outer values across a backedge,
forward references, independent function bodies, multiple bitset words, generated
diamonds, exhausted work followed by retry, hard bounds and unprofitable layouts.
Typed cases additionally exercise mixed lanes, unreachable producer placeholders,
existing lowerer compatibility, exact cost ties, one-byte savings and overflow.

The VM activation test compiled before integration and failed its two preparation
checks. It now requires both preparation and improvement events, exact distinct
numeric outputs, and typed instruction execution for a simultaneously-live
numeric fixture when typed execution is configured. A separate tiny-caller
contract proves persistent preparation occurs once. Typed execution assertions
use an explicit simultaneously-live numeric fixture. Other semantic fixtures
must not infer either execution family merely from an enabled build switch;
the original profitable selection is preserved before optional compaction.

`live_result_preparations` counts requested VM analyses, including declined tiny
streams. `live_result_plan_improvements` counts accepted improved construction
candidates, including standalone test calls. Neither is an instruction-execution
count or proof that each alternative was selected. Read them alongside actual
compiled mode definitions, exact outputs and `typed_instructions_executed`.

Diagnostic builds additionally track every planner scratch vector in the
`result_analysis` allocation category: region metadata, reachability, pending
work, both bit matrices, colors and neighbor marks. Retained maps stay in
`execution_plans`. Scratch live bytes must return to their prior value after
either API returns; its peak and cumulative allocations remain visible even
when analysis declines. This closes the earlier selected-container accounting
gap; it is still not a count of every process allocation. With profiling OFF,
the scratch alias resolves to the original `std::allocator` vector type.

`generic_result_slots_acquired`, `number_result_slots_acquired` and
`other_result_slots_acquired` sum the live element counts successfully acquired
by activations. Generic counts include the sentinel when a compact map uses
one. Typed counts use separate numeric/other domains. They count neither
reserved capacity nor constructor calls; both typed resizes must succeed before
either typed count is published. These observations identify the actual selected
buffer representation without an activation field or per-access tracking.
Mixed-lane contracts keep 64 numbers and 64 array snapshots live together and
check both nonzero and zero counter cases against full execution.

At the initial VM-integration checkpoint `8fc2d9d0`, local Release profile builds
with liveness ON and typed execution both ON and
OFF each pass 376 unit cases / 2,075,958 assertions and nine focused CTest
targets, including CLI semantics, signals, offset verification and ordinary/IPO
ownership guards. Native replacement-allocation sweeps exercise every observed
optional allocation in each planner API, require exact original maps/counts on
recovery and check outstanding allocation balance. Whole-VM sweeps continue
past absorbed optional failures until a genuinely uninjected call completes;
mandatory allocation failures retain their existing exception behavior.
On this host, the two native sweeps cover nine generic and thirteen combined
typed allocation points in both ordinary and IPO fixtures. The final liveness
OFF / typed ON profile build also passes 376 unit cases / 2,075,958 assertions
and the same nine focused targets, retaining the strict legacy failure checks.
The ordinary generic build passes 364 cases / 2,075,836 assertions and the same
nine focused targets. Its additional borrowed-argument fixture proves the
proposed map can reuse the argument as the call destination, then checks exact
heap-backed results on two calls in full and verified modes. This exercises
the Apple Arm64 fusion in ordinary builds; profile builds use argument packs.

The native correctness workflow requests all four typed/profile combinations
with liveness ON on Linux, macOS and Windows, preserving source, cache, compiled
commands or native projects, unit output and CTest outcomes. These twelve jobs
contain no performance measurements. All twelve passed at `8fc2d9d0` in
[run 42246344254](https://github.com/matt-gretton-dann/gd-posix-apps/actions/runs/42246344254),
and their source-pinned, provider-hash/ZIP-verified archives are retained.
Replaying the new provenance gate against those exact archives verifies both
compiler-command and native MSBuild evidence, including production source
inventory. These checks establish correctness configurations, not performance
acceptance.

The immutable builder accepts `--live-results AUTO|ON|OFF`. Explicit modes
require matching production VM compiler definitions and planner library objects,
or Release project settings and source inventory. A test-only planner object,
ignored cache key, conflicting/undefined macro, per-file override, excluded VM
source or unexpanded response file cannot establish activation. Native project
records are retained under `live-results-policy/`; metadata records the policy
and evidence hashes. Old revisions without this API remain `legacy-auto` with
an unknown `enabled` value. An opt-in candidate role must pass `require_live_candidate`
with explicit, recorded ON; AUTO is not an explicit opt-in even when its default
is known. Both benchmark controllers accept independent base and candidate
`--*-live-results` modes. Arithmetic A/A inherits the base mode; math A/A
inherits the candidate mode. Explicit ON and OFF require matching recorded
production compilation; legacy AUTO remains unknown. Diagnostic builds derive
from their own ordinary role recipes, including divisor and library-image
settings, so counters cannot silently describe a different configuration. The gate's fixture suite covers compiler token boundaries and Windows
paths as well as missing, contradictory and native Release evidence.

Focused Apple LLVM 21 coverage at runtime source `8fc2d9d0` retains separate
generic and typed builds: Debug, extensions ON, liveness ON, profile OFF and IPO
OFF. Each collection runs 74 VM/component cases (489,576 assertions), 27 CLI
cases and the native allocation-failure fixture, retaining 101 raw profiles,
exact executables, source/command hashes and all tool output. No export warnings
were emitted. The initial generic scope label incorrectly said 28 CLI cases;
corrected metadata and its original are both retained, with raw evidence intact.

| Planner scope | Lines | Branches | MC/DC |
| --- | ---: | ---: | ---: |
| Generic broad / application mappings | 390/413 | 329/382 | 39/90 |
| Generic CLI-only | 269/413 | 211/382 | 14/90 |
| Typed broad / application mappings | 391/413 | 334/382 | 43/90 |
| Typed CLI-only | 345/413 | 275/382 | 20/90 |

The broad view includes direct planner contracts and injected allocation
failures, with the exercised unit executable as the primary mapping object.
The application-mapping view uses that same profile set and only application
executable mappings; direct component calls still contribute. Their planner
counters coincide because these are the same non-template functions. The
CLI-only view uses only asserted CLI child profiles and application mappings.
Do not pool configurations or add raw MC/DC records; the displayed denominators
come from LLVM's summaries.

That checkpoint's gaps included independently exhausting budgets during
interference and coloring, a numeric lane needed only by an unreachable
producer, compound guard MC/DC and verified-input rejection paths. Follow-up
contracts now sweep every work allowance from 0 through 1,024 for both APIs,
requiring the exact original or complete layout and monotonic completion. They
also cover the unreachable-only numeric lane and preservation of an already
valid candidate pair under unsupported costs. The expanded focused suite passes
23 cases / 106,602 assertions; all 367 local Debug unit cases pass 2,081,106
assertions. Fresh, separately retained coverage at `f1550c4a` runs 77 VM/component
cases / 494,846 assertions, the same 27 CLI cases and native faults in each
configuration. Broad/application-mapping planner coverage is now 398/413 lines,
343/382 branches and 53/90 MC/DC for generic execution; typed execution reaches
398/413, 346/382 and 55/90 respectively. CLI-only counters remain at the earlier
values because these follow-ups add direct planner contracts, not application
paths. Each configuration retains its own 101 raw profiles and exact binaries;
all three exports are warning-free. At `02b906e8`, all 23 hosted checks passed,
including all twelve native mode configurations and emitted provenance gates;
their fifteen archives and exact gate records are independently verified. The
subsequent resource-accounting changes have separate validation and do not
inherit that hosted result. Locally, the new typed Release profile suite passes
381 cases / 2,081,258 assertions. The generic profile suite passes 380 cases /
2,081,246 assertions before adding the mixed-lane fixture, which separately
passes with both other resource contracts (42 assertions across three cases).
The ordinary allocator build passes 367 cases / 2,081,106 assertions and nine
focused ownership/CLI/signal targets; the profile configurations pass eleven
focused targets. At resource checkpoint `5366b3a1`, all 23 hosted checks pass and fifteen
source-pinned archives are preserved; all twelve emitted mode records
independently match their retained production compiler/project evidence.
Controller/resource follow-up validation is tracked separately in #662.
Performance qualification remains outstanding.

Remaining gaps include compound guard MC/DC, empty/transient-small combined
requests and defensive verified-input rejection paths. Several arithmetic
guards are not reachable at this host width under the hard instruction cap.
These are recorded gaps, not exclusions that establish complete coverage. The
focused results do not qualify every instruction/application path or other
platforms' coverage, and they establish no performance improvement. Broader
coverage and the complete native performance/startup/resource cohort remain
required. The independent frozen #658 prefix cohort is complete and its
candidate is rejected under #653; retain its adverse and inconclusive evidence
without source adjustments or measurement restarts.


Both benchmark controllers offer `--compare-resources`. They build matched
base and candidate diagnostic executables before any elapsed collection, check
source/compiler/flags/build-policy identities against each ordinary role, then
collect software counts and five alternating native cold-RSS pairs per fixed
representative after every elapsed stage. Arithmetic covers the full and
held-out startup, loop, calls, recursion, arrays, streaming and temporary groups,
plus finite ascending/descending frames with array parameters. Math covers the
fixed one-shot regime representatives near scale 100 and atan at scale 20.
Both math resource roles receive `-l`; instrumented durations are discarded.
These diagnostics do not replace substantive elapsed workloads or establish
steady-state retention. Unknown old-source categories remain missing counters,
not zero. Native RSS unavailability remains explicit and cannot become a pass.

Post-sampling code-generation archives retain every ordinary math preparation
role, including the separately built A/A executable and the primary/portable
Howard recipes. Each row retains its original argv and metadata; external
library inputs are hashed and copied, with missing or changed files a hard
error. Embedded GD libraries retain their source hashes and copied sources in
the source-library evidence. Inspection tools never execute the archived bc
binary, and unavailable native inspection has a generic binary/hash fallback.
The frozen rejected prefix cohort is unchanged; its missing math A/A executable
cannot be supplied retroactively by a new build.


Fresh generic and typed collections at runtime/controller source `c20f376d`
now cover the resource-accounting revision, separately from the historical
`f1550c4a` profiles. Both use Apple LLVM 21, Debug, extensions ON, liveness ON,
profile OFF and explicit IPO OFF. Each retains 101 raw profiles (one unit,
99 asserted CLI children, one native allocation-fault process), exact binaries,
source hashes, compiler commands and warning-free broad/application/CLI exports.
Both run the 77-case VM/component selection (494,846 assertions), 27 CLI tests
and native allocation-failure fixture. The raw planner summaries remain:

| Mode and scope | Lines | Branches | MC/DC |
| --- | ---: | ---: | ---: |
| Generic broad / application mappings | 398/413 | 343/382 | 53/90 |
| Generic CLI-only | 269/413 | 211/382 | 14/90 |
| Typed broad / application mappings | 398/413 | 346/382 | 55/90 |
| Typed CLI-only | 345/413 | 275/382 | 20/90 |

The allocator alias and profiling-event macros add no executable planner lines
in these profile-OFF configurations; identical totals do not establish new
condition coverage. Profile-ON accounting behavior has separate asserted native
mode/resource contracts, not an LLVM coverage claim. The documented guard,
verified-input and combined small-request gaps remain. No configurations or raw
MC/DC record counts are pooled, and no performance result is inferred.


## Isolated admission and execution-family follow-up

Frozen source `a9bc6194` remains unchanged in its original qualification. All six
arithmetic collectors failed post-sampling archival because their versioned
Howard role name was rejected (#663/#664), leaving Howard executables and fresh
startup studies absent. Their raw observations remain preserved and unqualified.
Separately replayed Windows ON results contain three confirmed #653 limit losses
(two Fibonacci shapes and frontend definitions). Independently replayed Linux
ON math adds five confirmed losses: four full-corpus scale-20 exponential/Bessel
cases and one held-out scale-1000 logarithm. An archive-only repair therefore
does not justify adopting that VM candidate. Every original outcome remains
preserved; incomplete or inconclusive results are not relabeled as passes.

Post-hoc, counter-only Apple/arm64 diagnostics at the same frozen source forced
typed execution support ON in both OFF/ON builds, with Release/profile ON and
coverage OFF. All six full/held-out Fibonacci/definition programs matched their
independent outputs and each other's output hashes. Most Fibonacci instructions
changed from typed to generic; 2,000 uncalled definitions incurred 22,000 analysis
allocations with no improved plan (held-out 1,750: 19,250 allocations). These are
software counts, not Windows CPU-cost attribution or local elapsed benchmarks.

A separate dependent worktree now tests conservative admission and preservation
of an already profitable typed family, without changing the frozen source or
routing by benchmark names. Test-first commit `d97268d1` builds and fails both
new contracts (five failed assertions of 95). The follow-up passes all 31 planner
cases / 115,212 assertions and the local Debug/typed/profile/liveness-ON unit
suite: 385 cases / 2,089,788 assertions. Its graph minimum and initial generic
tie cases retain independent path/lowering checks. The profiling-disabled unit
suite passes 370 cases / 2,089,544 assertions. Seven focused CLI, lifetime,
profiling-mode, signal and verifier-offset CTests pass in each profiling mode.
At runtime checkpoint `8c4f8e64`, all twelve native liveness contracts across
Linux/macOS/Windows typed/profile ON/OFF and all three native array checks pass.
All fifteen provider-digest/ZIP-verified archives are retained; the twelve actual
CMake caches, production compiler commands/MSBuild definitions and test logs
independently agree with their requested modes. Ordinary CI and CodeQL have
separate exact-source obligations.

Fresh Apple LLVM 21 coverage at that checkpoint uses Debug/live ON/profile OFF,
IPO OFF, extensions ON, in independent generic and typed configurations. Each
passes 80 VM/component cases / 503,300 assertions, 27 CLI cases and native
allocation-failure checks, retaining 101 raw profiles and exact binaries. All
six exports have empty diagnostic output. Broad planner coverage is 400/416
lines in each configuration: generic 346/386 branches and 51/90 MC/DC records;
typed 349/386 and 55/90. CLI-only coverage is generic 257/416 lines, 202/386
branches, 12/90 MC/DC; typed 329/416, 254/386, 18/90. Broad and application-mapping
views share the broad profiles; direct planner calls still contribute. CLI-only
profiles exclude those calls. The separate configurations/scopes are not pooled,
and remaining guards and uncovered records remain visible.

Same-source Release/profile ON Apple/arm64 diagnostics force typed ON for both
liveness OFF/ON builds. Six full/held-out arithmetic and 22 fixed math resource
representatives match their program/output hashes and total/typed instruction
counts. Both uncalled-definition shapes incur zero analysis allocations with
liveness ON. These are checked software counters, not elapsed measurements,
native Linux/Windows CPU attribution, or a substitute for qualification. Smaller
retained result buffers still require transient analysis: for the fixed sine(1)
scale-100 diagnostic, category peaks change from 8,936 to 776 result bytes and
from zero to 41,384 analysis bytes. Category peaks need not be simultaneous and
do not establish total process RSS or a net peak-memory reduction.

A complete independent performance/startup/resource qualification remains
required; none is inherited from the original cohort or these counters.
Production defaults remain OFF and both portable fallback paths remain
available.
