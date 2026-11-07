# Portable guarded integer regions

Issue #666 evaluates a portable counterpart to the bounded scalar experiment
in #528, as part of #207 and #197. The `ENABLE_BC_PORTABLE_SCALAR` option enables an experimental VM backend
and defaults to `OFF`. It uses ordinary C++ on every supported architecture;
unsupported streams retain the arbitrary-precision interpreter. Performance
remains an unmeasured hypothesis. It does not depend on, qualify or replace the rejected
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

The VM requires decimal input and zero, scale-zero values for every referenced
scalar. Full-check execution always retains the ordinary interpreter. Shape,
initial-state and analysis-allocation failures decline the optimization. The
executor writes only private scratch arrays. The VM converts every result into
an ordinary Number before publishing any scalar, rechecks interruption, then
moves all staged Numbers into place without throwing. Overflow, fuel exhaustion,
interruption and conversion allocation failure retain the original public state
for ordinary interpreter handling. Existing Apple Arm64 emission has priority
when both experimental backends are requested.

Profiling reports `portable_scalar_attempts`, `portable_scalar_successes` and
`portable_scalar_fallbacks`. Attempts count entry into the executor after shape
and initial-state guards; analysis declines do not increment them. Fallbacks
count started attempts that could not publish. `instructions_executed` continues
to count interpreter dispatch only. These software counters establish path use,
not elapsed speed or equivalent work across different backends.

Component tests cover independently calculated triangular sums, zero-trip
loops, parsed branch joins and nested loops, source mutation/destruction,
exact maximum integers, literal and addition overflow, repeated failed
attempts, fuel boundaries, the maximum fuel cap, all interruption checkpoints,
size/opcode boundaries, bad offsets and producer types, and failed dominance.
Release tests additionally exercise malformed operand representations which
Instruction's own constructor assertions prohibit in Debug builds.

Differential VM tests compare full and verified execution, exact maximum
integers, overflow rollback, unrelated scalar/array preservation, initial-state
guards and unsupported streams. An isolated allocation sweep checks both
handled analysis failures and propagated setup failures, including retry and
object lifetime. A separate profiled driver attempts to fail actual allocations
after executor entry, checking that conversion failure cannot publish partial
state. Some Number/string representations allocate no ordinary storage for this
fixture: the driver records that limitation rather than claiming injected
conversion-failure coverage.

The dedicated preflight workflow checks all four typed/profile combinations on
Linux, macOS and Windows, with language extensions both enabled and disabled,
portable execution enabled and Apple emission
disabled, retaining immutable source, actual compiler commands/projects, native
caches and unit/CLI/ownership/signal logs. Its binding check requires the portable
component in the production library and checks its private enabling definition.
Only this topic enables those jobs; ordinary elapsed workflows exclude it until
a separate immutable qualification is declared. Green correctness contracts do not demonstrate speed or adoption eligibility.

Before any adoption, the integrated implementation still needs completed native
differential VM and CLI checks, both language-extension configurations,
exercised compiler-mode evidence, fault/coverage review, and a newly declared immutable
Linux/macOS/Windows qualification cohort. Apply #653 independently to full and
held-out arithmetic/math, startup, allocation, memory and executable size.
Preserve adverse and inconclusive results. No local production elapsed study,
native speedup, merge eligibility or completion of #207/#197 is claimed here.
