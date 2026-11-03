# Verified VM live registers (#207, #197)

The verified interpreter can assign reusable result registers instead of
retaining one slot for every producer. `ENABLE_BC_LIVE_REGISTERS=ON` selects
this layout; `OFF` retains the existing producer-only compaction. Full checked
execution always uses independent, instruction-indexed storage. The candidate
branch defaults ON so native CI actually measures the proposed change. This
is not an acceptance claim: merging remains conditional on correctness and
native full/held-out performance evidence.

## Lifetime proof

Preparation follows complete bytecode verification. Branch targets, conditional
fallthroughs, definitions and terminal instructions delimit straight-line
blocks. A producer read outside its block, or read at/before its static position,
receives a dedicated register for the activation. This conservatively preserves
loop-carried values, join inputs and forward references reached by backedges.
Only producers whose every read follows them within the same block enter the
reusable pool. Their registers become available strictly after the final read,
not during the consuming instruction. Thus destination writes cannot invalidate
operand references or a variant currently being visited.

Preparation skips bodies registered by an outer definition; each verified
function gets its own persistent plan. Dedicated registers never enter the free
pool. Each activation still owns a separate buffer, and releasing it destroys
all remaining owning values before the buffer can be reused. Ordinary Number
and array copy-on-write snapshots, literal caches, diagnostics, interrupt polls
and bytecode entry checks retain their existing contracts.

The map uses 32-bit register indices only when representable. Tiny streams,
indices outside that range and layouts that cannot repay the map and empty
sentinel retain the generic identity layout. This allocator does not infer
control-flow liveness across blocks, reuse a source as its own destination,
compact bytecode operands, eliminate variant dispatch or implement native/JIT
execution. Those are separate remaining #207 experiments.

## Evidence required

Focused tests check peak overlap, repeated/final reads, branch joins, loop
invariants, forward references, skipped function bodies and fallback selection.
Checked/verified differential tests supply independent VM states and literal
caches; exact native-integer, source-language and mixed-value oracles detect
semantic changes. Sanitizer and allocation-balance tests cover ownership.

The 200-expression synthetic test needs four slots including the empty sentinel,
compared with 601 under producer-only compaction. These are logical slot counts,
not an elapsed-time, RSS or end-to-end speedup claim. Report preparation time,
allocations, memory, executable size and full/held-out paired runtime before
adoption. Linux and Windows allow up to 5% per-change elapsed-time slowdown under
the revised #197 policy; macOS requirements remain unchanged. Do not pool losing
workloads into an aggregate score or treat inconclusive measurements as proof
that a bound is satisfied.

## Local candidate validation

Integrated with main `609d1e08943a0e6e019157f4e1dbf0b57c9efb9f`, AppleClang
21, Debug with ASan/UBSan and a fixed multiplication split point of 126:

- Register reuse ON, extensions ON: 323 cases, 1,669,898 assertions.
- Register reuse ON, extensions OFF: 323 cases, 1,669,883 assertions.
- Producer-only fallback, extensions ON: 323 cases, 1,669,898 assertions.
- Consumer-mode, VM allocation/lifetime, CLI and VM CLI CTests passed in all
  three configurations.

LeakSanitizer is unavailable on this host; `detect_leaks=0` was used. The
isolated allocation-balance test supplies separate ordinary-allocation evidence.
These local checks establish neither native Windows/Linux acceptance nor a
performance gain. Native paired measurements and their raw evidence audit are
pending.
