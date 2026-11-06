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
records, maps and lane payloads with checked arithmetic. It selects a typed
candidate only when strictly cheaper than the best generic candidate; ties
prefer generic storage. Both original candidates survive any declined analysis
or failed optional allocation, and no partial pair is published.

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
contract proves persistent preparation occurs once. Reuse can make generic
storage cheaper even in a typed-enabled build; dead padding no longer forces
typed execution in other semantic fixtures.

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
focused targets. Native revalidation and performance qualification remain
outstanding for this resource-accounting revision.

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
