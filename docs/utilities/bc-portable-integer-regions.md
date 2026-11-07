# Portable guarded integer regions

Issue #666 evaluates a portable counterpart to the bounded scalar experiment
in #528, as part of #207 and #197. This checkpoint is a standalone lowering and
execution component linked only into the unit-test executable. The VM does not
call it, and no production option or default changes. Performance remains an
unmeasured hypothesis. It does not depend on, qualify or replace the rejected
or inconclusive liveness evidence in #661/#665.

`plan_portable_scalar` accepts 16–64 instructions from a limited scalar subset:
unsigned decimal integer literals, scalar locations, loads, stores, addition,
unsigned less-than, unconditional branches and forward conditional branches.
At least one backward unconditional edge and one scalar are required. Literals
have at most twenty characters and must fit exactly in `uint64_t`; longer,
fractional, signed and otherwise unsupported literals decline to ordinary
arbitrary-precision execution. There is no benchmark, function-name or library
recognition.

Lowering checks operand presence, schemas, offset bounds and producer kinds
before calling the ordinary control-flow verifier. It computes negative offset
magnitudes without negating the signed minimum. Data references must identify
numeric producers or scalar locations as appropriate; a branch may additionally
target the end of the stream. The graph proof rejects reachable reads whose
producers do not dominate them, including skipped producers and values available
only from an earlier loop iteration. Store instructions produce no value.
Unsupported instructions, conditional backward edges and graph allocation
failure decline the optimization. The owned lowered plan has no references to
the source instruction vector.

The executor uses fixed private result and scalar arrays. Numeric operations
never inspect general operand variants inside the loop. Addition checks the
unsigned overflow boundary before assigning its result. Every backward edge
spends fuel, with an unconditional maximum of four million edges even if a
caller requests more. An interruption callback runs before execution, after
each 1,024 backedges and before returning successful results. Overflow,
exhaustion and interruption return no result, including after earlier private
stores; repeated attempts start with fresh zero scratch state. The component
does not perform I/O, touch public VM state, allocate executable pages or use
platform assembly.

An eventual VM caller must guard decimal input and zero, scale-zero values for
all referenced scalars, and preserve ordinary checked/interpreter execution for
every declined attempt. It must stage all ordinary Number conversions before
publishing state, preserve interrupt handling across publication, and retain
full-check execution. These are integration obligations, not behavior supplied
by this standalone component. The existing Apple Arm64 emitter is unchanged.

Component tests cover independently calculated triangular sums, zero-trip
loops, parsed branch joins and nested loops, source mutation/destruction,
exact maximum integers, literal and addition overflow, repeated failed
attempts, fuel boundaries, the maximum fuel cap, all interruption checkpoints,
size/opcode boundaries, bad offsets and producer types, and failed dominance.
Release tests additionally exercise malformed operand representations which
Instruction's own constructor assertions prohibit in Debug builds.

Before any adoption, the integrated implementation still needs differential VM
and CLI tests, both language-extension configurations, exercised native build
mode evidence, fault/coverage review, and a newly declared immutable
Linux/macOS/Windows qualification cohort. Apply #653 independently to full and
held-out arithmetic/math, startup, allocation, memory and executable size.
Preserve adverse and inconclusive results. No local production elapsed study,
native speedup, merge eligibility or completion of #207/#197 is claimed here.
