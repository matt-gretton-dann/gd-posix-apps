# Result slots for value producers

The #207 experiment retains the existing instruction stream, result value types,
relative data operands and source locations. After complete entry verification,
prepare a compact result map for sufficiently large profitable instruction packs.
Use the same static result classifier as the verifier rather than another opcode
contract table. Each value producer receives one distinct slot. Non-producers
share an empty sentinel; a definition body skipped by its outer registration
pack receives no outer value slots. Its eventual function activation has its own
verified plan.

This does not reuse slots by liveness. Forward references, joins, backedges,
repeated operands and loop-carried values keep their original instruction-relative
identities. The map changes where a producer's existing result lives, not when
it becomes available or when it is overwritten. The complete control-flow and
data-availability verifier still runs before preparing either layout.

Small streams, unrepresentable compact indices and maps without a slot/map
payload saving retain the original instruction-indexed buffer. Check narrowing
and map-byte arithmetic before allocation. The payload comparison includes the
map and one empty sentinel, using the actual compiler's result-slot size. It does
not claim an RSS saving: owner headers, allocation capacity, retained code and
cache effects require separate diagnostics. Shared map ownership follows the
existing prepared stream/function-code lifetime; no view outlives its owner.

The full-checking VM keeps the original buffer as an independent execution
oracle. The verified VM retains semantic errors, numeric and array COW behavior,
argument snapshots, caller state, interrupt polling and result destruction.
Per-depth buffers still clear all live values on release while retaining their
capacity. No pointers into a relocatable caller buffer are introduced.

Tests include a failing logical-storage budget for a large function that is
registered but never called, independently checked producer maps and payload
boundaries, forward references/backedges, skipped bodies and the existing
checked/verified differential, malformed-bytecode and lifetime suites. Logical
allocation totals are separate from process RSS and elapsed measurements.

The implementation adds an index lookup for mapped accesses. Its cache/dispatch
cost can offset the smaller result buffer; fewer slots alone are not acceptance.
Measure cold registration, warm calls/loops, recursion, alternating frame sizes,
arrays, streaming input and ordinary portable math source on the full/held-out
native matrix. Keep losing and inconclusive cases. Typed executable operands,
CFG-aware liveness, direct call windows and fusion remain separate #207 work.

## On-demand result prefixes (#651)

An independent prototype reserves the complete planned capacity before taking
an activation lease, but constructs the contiguous prefix only as indices are
accessed. The vector's existing size records that prefix, without a bitmap,
touched list, generation tag or additional owner header. Access adds a branch;
dense workloads can lose time even when early-return workloads avoid unused
slot construction. No elapsed improvement is assumed.

The helper preserves capacity and active references, clears every constructed
owner on release and supports eager initialization for full-check diagnostics.
Preparation failure must leave an empty usable buffer. Growth failure must
restore the preceding prefix. Tests exposed libc++ retaining partially appended
cells after a throwing default constructor; explicit rollback now supplies
those guarantees independently of that behavior. The failing contracts were
committed before their repair, and their original logs remain preserved.

Eight deterministic component tests cover construction work, stable references,
allocation failure/retry, partial initialization, dense initialization, escaped
owners, amortized growth and allocator limits. VM integration and focused
interruption/fault checks also pass locally. Native cross-platform correctness
and resource qualification remain required. It does not repeat or supersede #585/#588's retained
object and static cleanup-prefix experiments or their adverse/inconclusive
native results. No new timing cohort starts while #650 is running, and no
adoption or parent issue closure is claimed.

The VM integration is controlled by `ENABLE_BC_PREFIX_RESULTS`, initially OFF.
When enabled, generic verified activations prepare reserved capacity and grow
their initialized prefix on access; full-check execution still constructs the
entire identity buffer eagerly. Typed lanes keep their existing lifecycle.
Both paths release constructed values through the existing depth lease and
exception/interrupt cleanup. The actual result-buffer header size is statically
checked against the original vector, and result slots have nonthrowing default
construction. Valid active accesses therefore need no allocation or relocation.

Profile builds count actual default result-slot initialization with
`result_slot_initializations`; ordinary builds retain their original slot
constructor. An executed short early-return bytecode stream proves one cell is
initialized with the option enabled versus twelve in OFF/full-check modes,
including configurations with typed execution enabled. This uses the existing
independent expected-mode definition on the test consumer, rather than a private
library macro. The enabled CTest integration group also exercises all VM-tagged
component tests, including mixed alternatives, recursive snapshots and control
flow. Allocation-failure and signal fixtures remain separate checks.

There is one vector owner and no auxiliary allocation structure. That does not
promise identical lifetime allocation totals: `reserve` can choose a different
growth capacity from the old empty-vector `resize` on a standard library.
Preparation grows capacity geometrically, saturating safely at the allocator
limit, rather than requesting an exact reserve on every larger activation. A
deterministic local libc++ probe for requested sizes 1 through 20 showed six
capacity changes for both the old eager vector and this helper, instead of
twenty for exact reserve. Other standard libraries may choose different
capacities. Changes in allocation frequency and retained capacity must be
reported alongside per-access branch cost in native qualification. Construction counts are logical
work evidence, not elapsed time, CPU attribution or RSS.
