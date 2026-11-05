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
Generic per-depth buffers reset live values to the empty variant on release
and retain their length as well as capacity. Reacquiring an equally sized
activation avoids reconstructing every empty slot. Typed lanes keep their
existing clear-and-resize lifecycle. No pointers into a relocatable caller
buffer are introduced.

A verified generic function return can carry a cleanup prefix. Lowering bounds
that prefix by the highest result index encountered through the return's source
PC, including identity-indexed layouts. A return crossed by a backward edge
retains full cleanup because a later producer may have executed first. Other
returns can only have populated the proved prefix. Exceptions, interrupts and
other exits retain full cleanup; debug assertions check the untouched tail.
Tests cover conditional and unconditional backedges, mapped and identity result
indices, and a return that reads a later producer through a backward edge.

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

The retained-buffer and cleanup-prefix candidate remains experimental. Earlier
native qualification found a Linux sparse-multiplication regression, despite
unchanged normalized sparse-kernel instructions. Its integration with the
separately emitted GCC sparse kernel is being qualified on full and holdout
native workloads. Neither passing correctness checks nor local speedups alone
establish performance acceptance.
