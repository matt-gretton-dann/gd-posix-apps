# Control-flow-aware result lifetimes

Issue #661 continues #207/#197. The current stage is a standalone generic/typed
planner compiled into the unit tests only. The VM does not select it, no new
build mode is enabled, and no elapsed improvement or adoption is claimed.
VM integration remains outstanding. Linux and Windows currently default to
typed execution, so generic-only results cannot establish an improvement to
those configurations.

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
lane counts and map, with no sentinel slot. No runtime liveness table, extra activation
header or changed instruction ordering is introduced. There is no literal-value
folding or assumption about runtime bases, scales, globals or function names.
Before VM integration, implicit argument borrowing, fused operations, entry and
continuation rules, owning snapshots and every supported result alternative
still need explicit review and semantic differentials.

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
contracts while preserving its original-plan fallback. The implementation now
passes nineteen contracts and 101,317 assertions. The typed test-first extension
failed four of five contracts before implementation. A bounded path oracle independently
compares logical value versions against physical slots, without solving
liveness or using the planner's result classifier. It explores both conditional
edges and repeated paths, with finite test-only step limits. Cases include dense
dead literals, repeated operands, joins, live outer values across a backedge,
forward references, independent function bodies, multiple bitset words, generated
diamonds, exhausted work followed by retry, hard bounds and unprofitable layouts.
Typed cases additionally exercise mixed lanes, unreachable producer placeholders,
existing lowerer compatibility, exact cost ties, one-byte savings and overflow.

The complete local Release unit executable passes 363 cases and 2,075,821
assertions. These results establish standalone planner contracts and preserve
the existing unit behavior. They do not execute the new planner through the VM,
prove native allocation-failure injection, establish source/MC/DC coverage, or
qualify another platform for this revision. Keep those limitations visible through
VM differentials, mode verification and the eventual full native performance,
startup and resource cohort. Do not alter or restart the independent frozen
#658 prefix qualification to incorporate this work.
