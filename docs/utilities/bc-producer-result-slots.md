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

## Typed numeric-pool experiment

The next #207 step gives statically numeric producers their own contiguous
`Number` pool. Locations, strings and array snapshots retain the existing
variant pool, including one empty nonproducer sentinel. A tagged 32-bit map
names each `(pool,index)` pair uniquely. There is no liveness reuse: loops,
forward references and joins keep the same verified producer identity.

Complete entry verification runs before planning, including operand schemas,
result kinds, dominance and parameter-stack obligations. The number kind of a
producer is fixed. Default-constructed numeric storage therefore needs no
runtime engagement/type tag; reaching its consumer is already proved to follow
its producer. A scoped result view borrows the current activation's owning
storage. It does not cache pointers across activation-buffer growth or moves.
Literal caches and code remain owned by the existing immutable code owners.

The planner uses actual caller-supplied slot sizes and charges the map, numeric
pool, variant sentinel and extra buffer/header budget. It must beat the best
existing producer-map or identity payload. Tiny streams, incompatible sizes,
overflow, tagged-domain limits and unprofitable layouts retain that existing
generic representation. The typed domain requires fewer than 2^31 source
instructions; generic maps keep their complete 32-bit index domain, and the
identity fallback keeps the full original instruction-index width. No public
language or source-program limit is narrowed.

The fully checked VM retains instruction-indexed variants and the complete
per-result diagnostic switch. It remains the differential oracle. Runtime
semantic errors remain in both modes. Number and variant buffers belong to
one stable deque element per simultaneous activation depth, clear owning values
on release, and retain capacity for later calls. If allocation fails after
variant resizing, both buffers are cleared before propagating the exception,
so the same VM can retry without consuming or damaging its activation depth.

Validation includes exact checked/verified execution, separate array/scalar
load kinds, recursion with local restoration and array snapshots, excluded
uncalled bodies, map/header profitability, and generic fallback. The isolated
lifetime test fails each ordinary allocation position once while executing a
mixed stream with 67 heap-valued numeric literals, then retries the same VM.
Destruction must return the ordinary outstanding-allocation count to its prior
value. This exercises partial preparation, both result pools and literal-cache
failures without masking them through a successful fresh VM.

Local ASan/UBSan checks pass 234 component cases (1,211,033 assertions),
including five targeted cases with 139 assertions, the allocation-failure and
lifetime test, bounded malformed-stream checks, driver/VM checks, signal
delivery, 616 identical ordinary-source math references and Release profiling
checks. LeakSanitizer is unavailable. Native and elapsed adoption evidence is
still required. Smaller selected payload is not a claim about process RSS,
allocator rounding, retained capacities, startup or execution speed. The extra
pool/header and lookup branches can still cost memory or time on fallback
streams, and must be measured.

This step does not compact the wide source instruction operands, infer CFG
liveness, fuse operations, or complete #207. Those items retain their original
scope and require separate proof and performance evaluation.

The topic uses the explicit `codex/bc-native-broad/` branch convention, retaining
full and held-out native arithmetic samples for this executable change. This
only selects the existing corpus; it does not relax control or loss policies.
