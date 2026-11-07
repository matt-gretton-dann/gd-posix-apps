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

The optional transaction is compiled as the separate `bc-portable-scalar`
object target. Its entry receives the encoded scalar span, input base,
verification mode and a synchronous interrupt callback with caller-owned
context. Planning, integer scratch, Number conversion and exception handling
stay outside the central interpreter. Conversion finishes before the final
interrupt check and nonthrowing publication; a declined transaction preserves
every public scalar. The Apple emitter retains its existing priority.

Local Apple Clang 21 Release coverage of this boundary (typed execution on,
profiling off) covers 271/271 executor lines, 156/158 branch outcomes and 26/26
MC/DC obligations. The two uncovered private enum defaults cannot be built by
the checked planner. The adapter covers 48/51 lines, 34/34 branch outcomes and
9/9 MC/DC obligations; its two allocation-failure handlers remain unexercised
on this host's inline integer representation. Native fault sweeps and hosted
mode checks remain separate evidence. These local totals are not a
cross-platform coverage or performance claim.

This target explicitly disables IPO in every configuration while the core keeps
its ordinary IPO policy. This portable CMake boundary is an experiment in
optimizer isolation, not evidence that it removes the earlier factorial
regressions. A shared interface carries array layout, prepared-divisor,
profiling and language definitions to both targets and their consumers. Coverage
must instrument the backend objects themselves. Deterministic callback tests
exercise every publication checkpoint separately from real OS signal tests.

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
objects linked into the production library, shared ABI definitions and the
actual IPO boundary. Compiler commands and native project references must agree
with the generated target-property contract; all are retained for review.
Only the isolated backend topic enables those jobs; ordinary elapsed workflows exclude it until
a separate immutable qualification is declared. Green correctness contracts do not demonstrate speed or adoption eligibility.

Before any adoption, the integrated implementation still needs completed native
differential VM and CLI checks, both language-extension configurations,
exercised compiler-mode evidence, fault/coverage review, and a newly declared immutable
Linux/macOS/Windows qualification cohort. Apply #653 independently to full and
held-out arithmetic/math, startup, allocation, memory and executable size.
Preserve adverse and inconclusive results. No local production elapsed study,
native speedup, merge eligibility or completion of #207/#197 is claimed here.

The immutable build helper accepts `--portable-scalar AUTO|ON|OFF`. Explicit
modes require the configured value, the private production compiler definition
and the matching library source inventory; a standalone test component cannot
prove VM activation. The helper disables and verifies the separate Apple emitter
for these isolated comparisons. A source predating this option is supported only
through `AUTO`, with its mode recorded as unknown rather than disabled. Unexpanded
response files, contradictory definitions and unverifiable project overrides
fail closed. Current object-boundary revisions also retain the evaluated
Release target contract and verify the two production objects, their inclusion
in the core library, shared mode definitions and the absence of backend IPO.
Older revisions without this boundary retain their historical in-library
validation; their evidence is not upgraded to an opaque boundary claim.

Arithmetic and mathematical collectors accept `--base-portable-scalar` and
`--candidate-portable-scalar`, propagating each role into both ordinary and
separate diagnostic builds. `--compare-resources` builds those diagnostic roles
before elapsed sampling, verifies their source, mode, toolchain and ordinary
binary identity, and runs matched counter/RSS diagnostics after all elapsed
stages. Counter totals describe instrumented software categories, not process
malloc or RSS. Resource repetitions remain descriptive; they cannot substitute
for the independently required elapsed, startup or #653 acceptance evidence.
