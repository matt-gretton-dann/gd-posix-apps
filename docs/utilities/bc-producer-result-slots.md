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
semantic errors remain in both modes. Number and variant buffers have stable
per-depth owners, clear live values on release, and retain capacity for later
calls. Numeric backing is acquired only when a selected plan needs it. Failed
variant, lazy owner or number-vector growth clears the current activation
before propagating the exception, so a retry does not consume its depth.

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

## Direct numeric access refinement

The initial typed-pool candidate, PR #409 at `9028375`, is not accepted:
native run 41491016435 retains 16 confirmed Linux losses and two Windows
losses. Its completed local full/held-out comparison has 20,880 checked
executions and no confirmed local loss, which does not waive those native
results. Logical result storage falls in several cases while process RSS
remains mixed. Payload savings alone do not establish adoption.

The next refinement uses the verifier's existing numeric-kind proof directly
for arithmetic operands and returns. Numeric C++ result types select their
fixed destination pool for literals, arithmetic, scalar loads, calls and
parameter pops. Location loads retain their statically nonnumeric variant.
Operations with genuinely mixed public alternatives still use the scoped
result view. Generic layouts and the full diagnostic interpreter retain
variant engagement/type checks; there is no new execution mode, source limit,
slot reuse or cross-activation pointer. The existing last-use proof continues
to govern coefficient transfers independently of this access change.

Expanded exact opcode fixtures run the same independent native-integer
oracles through both tiny and numeric-heavy streams, comparing each with the
full checked interpreter. Further native full/held-out measurements are
required before claiming that avoiding dynamic result views fixes any loss.

## Lazy numeric backing and generic call frames

The direct-access checkpoint `cbbc754` retains a confirmed Windows held-out
`calls_175000` loss in native run 41508074256: candidate/base medians 1.06774
and 1.06930 on independent confirmation, with a passing A/A control on Intel
Xeon Platinum 8370C. Its Linux and macOS batches have no confirmed loss, but
that does not waive the Windows result. All 16,910 raw checked executions and
actual executable/inspection identities remain retained. This candidate is
unaccepted, as is the earlier eighteen-loss typed-pool checkpoint.

The storage refinement restores single-vector headers in the generic
activation deque. Numeric buffers receive individually owned headers only
when a profitable verified numeric plan first needs backing. A sorted registry
contains only selected depths, so generic ancestors leave no empty owner slots. A generic
or fully checked call acquires no numeric backing. Each active pack borrows
its generic buffer and a nullable numeric-buffer pointer. Generic deque growth
and registry handle moves cannot relocate those owners. That pointer also identifies the
layout, replacing a separate runtime Boolean. Values are cleared through the
borrowed view on release; capacity can be reused by later calls at that depth.

The empty registry still adds a VM-wide vector header. The planner charges
its whole actual compiler-dependent size, a numeric vector header, one registry
entry and the existing bookkeeping allowance to every selected numeric plan. Charging that
VM-wide cost repeatedly is conservative. Tiny or unprofitable packs retain
the generic representation; allocator rounding, capacities, executable size
and process RSS remain separate measurements. Neither fewer generic frame
headers nor a changed payload decision proves a timing or RSS improvement.
Header objects and registry entries are charged to frame profiling; number
elements retain the result-storage category. Header construction is proved
nothrow, and ownership is installed before element-vector growth.

Allocation failure during header creation, registry insertion or element-vector
resizing clears the current acquired values without advancing the activation
depth. Earlier callers' owners remain stable. Existing isolated allocation
failure/retry tests cover every allocation position in a mixed numeric stream.
A new exact checked/verified test alternates large numeric recursive bodies
with tiny generic calls, then reuses the same depths in the opposite layout.
The generic fallback, literal caches, source instructions, transfer proof,
semantic errors, caller restoration and interrupt checks retain their contracts.

Local validation passes 235 ASan/UBSan cases (1,215,643 assertions), including
six typed-result cases with 146 assertions, nine CLI/verifier/lifetime/signal
and controller checks, 616 identical ordinary-source math references, and
twenty separate Release profiling checks. LeakSanitizer is unavailable.
New full/held-out native evidence is required before accepting this refinement;
it does not complete the wider operand/register/liveness work in #207/#197.

The intermediate optional-deque implementation `4c66d13` is not adopted. Its
local logical profiles increased mixed-recursion frame peak from 5,304 to
9,392 bytes, and a new first-backing test measured 4,088 extra frame bytes
against a 256-byte metadata bound. An indexed-owner working prototype removed
that block but allocated unused entries through generic recursion: a second
test measured 688 extra bytes for one numeric leaf below eighty generic calls.
Both tests were committed before the final ownership fix. Sparse individually
owned headers now pass those metadata bounds without weakening their inputs.
All prototype evidence remains retained; the bounds concern requested frame
metadata and do not establish a process-memory or timing win.
