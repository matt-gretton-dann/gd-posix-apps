# Portable integer function experiment

Issue #598 extends #207's verified-execution work. The intended experiment is
a portable integer tier for hot pure functions, targeting call and recursion
traffic. No speedup or default-adoption claim is established. The execution
tier is an opt-in experiment (`ENABLE_BC_INTEGER_FUNCTIONS=ON`, default OFF).

## Number boundary

`Number::to_machine_integer()` returns an optional signed 64-bit value. It
rejects any nonzero declared scale, including `1.0` and fractional zero, rather
than silently changing the scale of intermediate bc arithmetic. It also rejects
magnitudes outside the signed range. Rejection does not emit a diagnostic,
detach a shared coefficient or modify the original value. Leading zero limbs
do not affect the guard, and every coefficient accumulation is checked before
the machine multiply/add.

`Number::from_machine_integer()` constructs the exact scale-zero value without
narrowing through the storage limb type. Unsigned magnitude arithmetic handles
the most negative signed value without signed overflow. Ordinary register and
array-index `to_unsigned()` semantics and diagnostics are unchanged.

Tests compare the boundary against independent decimal-text construction in
8-, 16- and 32-bit traits. They cover signed extrema, radix/limb boundaries,
values beyond both signed endpoints, shared heap and inline coefficients,
declared scales, and the complete integer range from -1024 through 1024.
Compiler/runtime validation uses the enabled native workflow; a local diff
check alone is not a build or execution result.

## Execution contract

After eight eligible scalar calls, the tier attempts one plan per verified
function generation. It accepts one scalar parameter, parameter/auto-only
scalar state, single decimal digit literals, scale-zero signed integers,
checked addition/subtraction/multiplication, comparisons, branches, stores
and self-calls. Arrays, special-register access, I/O, other callees and
unsupported instructions reject the plan, even in unreachable code.
Full-check execution uses the original interpreter. Allocation-profile builds
execute the selected tier and report its owned storage and speculative work;
they remain separate from uninstrumented timing builds.

Variable operands remain locations until consumption; explicit loads snapshot
values. Self-calls use explicit continuations instead of native recursion.
Preparation accepts at most 1024 source instructions. Each attempt uses at
most 1 MiB of combined logical value, continuation and argument storage.
Allocator/vector capacity and RSS remain separate measurements. Scratch
capacity is reusable, and live state clears on every exit, including exceptions.
Interrupts are polled before each instruction.

Unsupported inputs or scratch exhaustion restart the original function.
Purity prevents observable effects, and the incoming argument pack
stays intact until the successful result is boxed. Arity/type errors retain
the original path. Redefinition resets the plan and hotness count. Deferred
definitions materialize normally. Both forwarded and packed arguments are
supported.

The experiment's metadata is owned by a lazy per-VM table outside the common
function entries and VM prefix. A program without calls allocates no table.
Allocation occurs only after the VM-scale, arity and scalar-type guards pass;
warmup metadata can still be allocated for a function whose body later fails
the purity proof. The table has 26 bounded entries plus scratch headers, and
profiling charges that fixed object to `execution_plans`. It is outside the
per-attempt 1 MiB scratch limit, as are cached IR and retained vector capacity.
Both ordinary definition replacement and deferred-library replacement reset
the affected entry. Lowered IR owns its data and borrows no source storage.

`test-integer-layout.py` toggles the feature in one real Clang compiler context
and compares actual records. Common function layout and all pre-existing VM
field offsets must match; total VM size can include the tail owner and routing pointers.
Source/command hashes and each mode's records are retained. Unsupported
compiler/generator evidence is explicitly unavailable; portable semantic,
redefinition and allocation-failure tests remain required. Layout equality
does not establish runtime acceptance or explain a host's slowdown by itself.

A failed attempt suppresses speculation throughout its generic fallback call
chain. One rejected machine argument per function generation is remembered;
the same argument enters generic execution directly, while different arguments
remain eligible. This bounded performance hint changes no numeric semantics.
Without it, overflow near the bottom of recursion repeatedly executes the same
speculative prefix at each generic level. The preserved first local diagnostic
observed calls at 0.623x baseline but factorial recursion at 3.572x baseline.
That two-role probe has no independent A/A control and establishes no acceptance.

## Exact large integer continuation variant

The stacked hybrid variant keeps signed machine values for ordinary arithmetic
and promotes an overflowing operation to the existing arbitrary-precision
`Number` implementation. The continuation stays intact. Later results that fit
the signed machine range demote again, including cancellation to zero. Self-call
arguments and return values can carry either representation internally; the
external entry guard still requires an exact scale-zero machine integer.
Addition, subtraction, multiplication, negation and comparisons retain exact
integer semantics. No division, scale-sensitive operation or new effect is
introduced. In particular, factorial recursion can retain its large result
without repeatedly abandoning the machine tier at integer overflow.

Small cells contain a signed payload and a large-value tag. Large values live
in a separate immutable spill bank; cells and arguments borrow indices, so bank
growth and cell overwrites preserve aliases. The bank's Number storage is opaque
to the native engine. Its header is allocated on the first promotion, retained
for reuse and charged to the logical limit, even after its records are cleared.
Logical scratch charges actual
cell/frame/argument/spill object sizes plus a decimal-length estimate,
`decimal digits * sizeof(Number::NumType)` charge per owned spill. Shared
coefficients in separate spills are charged separately. The estimate is not a
measurement of retained heap capacity after arithmetic shrinks a coefficient.
Dead spill records stay
owned and charged until the attempt ends; a long loop can reach the guard and
fall back even when its most recent value is small. The combined
limit remains 1 MiB, with a 4096-decimal-digit limit per live value. A single
multiply can temporarily produce at most twice that digit bound before its
publication guard; bounded arithmetic temporaries, allocator overhead and
retained vector capacity are separate from the logical live-state limit.
Exceeding either guard restarts generic execution with the input pack intact.
The earlier backoff-only candidate and adverse measurements remain separate.

Direct tests additionally cover exact factorial(50), signed-minimum negation,
promotion followed by cancellation/demotion, aliases across cell overwrites and
bank growth, per-value and dead-record budget failure, and scratch reuse.
The first large-variant diagnostic observed calls at 0.935x baseline and
recursion at 0.962x; it remains preserved separately from the compact cells.
Native performance, resource measurements and sanitizer checks
are still required. The signed-zero boundary correction #601 must be accepted
and integrated before adoption; this experiment canonicalizes numeric zero.

## Diagnostic execution

When both profile and integer-function options are ON, diagnostics execute the
actual engine rather than substituting generic interpretation. Its values,
spills, continuations and argument vectors use the existing `frames` allocation
category; persistent integer IR uses `execution_plans`. These scoped counters
include requested vector capacity and owned spill objects. Heap coefficient
storage remains in `digits`; inline coefficients live within their owning
objects. Allocator overhead, compiler/runtime temporaries and RSS stay outside
the selected-container counters.

Explicit `integer_function_*` events count engine attempts, produced results,
abandonments, dispatched instructions, attempted self-calls, promotions and
demotions. Abandonments include guards, interrupts and exceptions. They can
include work later repeated by generic fallback; they are not semantic call or
instruction totals. Existing generic instruction/function counters retain their
interpreter-entry meaning. A result counter records engine completion before
the caller's final interrupt/commit check. Exact output and post-VM teardown
zero live bytes are tested separately. Profiling builds must never supply
elapsed-time acceptance evidence.

The isolated VM lifetime executable also walks allocation failures through a
recursive integer call until it reaches the first fully completed allocation
walk. It covers generic warmup, plan publication, scratch growth, large spills
and boxing, then checks caller globals, correct retry and complete ordinary
allocation teardown. The sweep is enabled only when the integer tier is built;
it runs in both instrumented and ordinary configurations without production
fault-injection hooks.

Tests cover the engine, signed endpoints, lvalue timing, explicit loads,
interruptions, depth exhaustion, scratch reuse and effect rejection. Parsed
hot-call programs compare verified execution with the full-check interpreter,
including overflow fallback, fractional inputs, scale changes and redefinition.
The dedicated native workflow enables the experiment on Linux, macOS and
Windows with extensions ON and OFF. These checks establish no speed claim.

The diagnostic CLI tests also use fixed-seed generated pure functions with
independent Python integer expectations. They cover signed endpoint inputs,
mixed-sign products, large comparisons, loop branches, recursive products and
nested self-call argument marks. Each generation is warmed before its asserted
inputs. Engine counters must prove successful tier execution without fallback;
every invocation checks complete numeric output and zero tracked live bytes
after teardown. Decimal continuation lines are unfolded for these numeric
oracles; separate CLI tests cover their formatting. The generators are bounded
semantic probes, not exhaustive coverage or timing measurements.

Direct signed-edge tests compare each arithmetic operation with generic
`Number` arithmetic over signed extrema, near-extrema, zero and mixed signs.
They also exercise each supported comparison with both machine and large
operands. Profile assertions check that an operation promotes exactly when its
result exceeds the signed range and does not demote unnecessarily; exact values
alone would not catch redundant generic arithmetic. Each attempted execution
must release its live scratch entries. These bounded tests improve feasible
branch coverage without establishing complete condition coverage of the engine
or verifier rejection paths.
Large-value tests also exercise truth tests, negation and double negation after
promotion, including zero and both signed endpoints. Preparation tests accept
and execute a 1024-instruction body and reject the otherwise valid 1025-entry
body, counting the prologue and return in that limit.

Keep the tier experimental until differential semantic tests, cold and warm
full/holdout measurements, and native Linux/macOS/Windows resource evidence
justify adoption. Preserve adverse and inconclusive classes; the overall #197
competitive requirements remain open.

## Native qualification recipe

The compiler-isolation experiment (#614) builds the optional engine as a native
object target with project IPO disabled, including configuration-specific
inherited IPO defaults. It inherits the library's language, definitions,
diagnostic and coverage options. Other library translation units retain their
normal optimisation policy. `ENABLE_BC_INTEGER_ENGINE_ISOLATION=OFF` restores
the original engine compilation path. The integer feature itself remains OFF
by default, and no engine object is built in that configuration.

`bc-integer-isolation.json` records the configured boundary. Build evidence
verifies the separate engine command and rejects explicit engine LTO flags;
native Visual Studio projects retain both owning targets and reject enabled
whole-program optimisation in the engine project. Earlier sources without this
report retain a legacy/unknown isolation status. This evidence establishes a
compiler boundary, not a performance win or proof that IPO caused a slowdown.

The follow-up Number boundary keeps promoted addition, subtraction,
multiplication and comparison in `integer-number.cc`, compiled with the ordinary
library policy. The opaque-ownership follow-up also keeps Number construction,
destruction, conversion, spill ownership, normalization and final boxing there.
The native engine passes only scalar cells and immutable bank indices across
this boundary. The ordinary adapter takes both owned input snapshots before
arithmetic or bank growth. Every zero result normalizes to a small cell, so a
big cell cannot take a zero branch. Final boxing precedes cleanup and success
accounting, including exceptions. This changes neither the accepted operations nor the checked
machine path, generic fallback, scratch guards or mathematical algorithms.
The build helper additionally verifies the bridge's compiled source/definition
alongside the VM and engine.

`bc/integer-symbols` inspects the actual native engine object, requires its
planner/scalar-core entry points and rejects every defined Number/Digits
instantiation, including ownership and conversion. It retains object/tool
hashes, raw symbols and the narrower arithmetic-definition inventory for
comparison with earlier sources. A missing
or incompatible symbol adapter is explicitly unavailable rather than passing
proof; portable numeric and ownership tests still run. Native workflow artifacts
retain this evidence, actual engine objects (including COFF when available) and
the actual record-layout checks. An archived object alone is not a successful
symbol-adapter result. The original isolated
object fails the arithmetic guard; the later arithmetic-only boundary still
fails the expanded ownership guard. Removing these definitions does not by
itself establish a runtime gain or explain previous losses. Native evidence,
complete fault sweeps and new full/holdout qualification remain required.

`bc-integer-performance.yml` compares OFF and ON at the same immutable source
on Linux, macOS and Windows. It starts for the fixed experiment topic and also
supports manual replay with a full source SHA. Ordinary PRs retain the separate
fast semantic workflow. Arithmetic includes the full/holdout corpus and fixed
sparse latency/duration controls. Math runs full and holdout separately with a
0.25-second calibration target, 600-second child deadlines and a bounded total.

Calibration, paired sampling and confirmation commands use the remaining
explicit total command budget rather than a hidden one-hour stage cap.
Shorter explicit build/test deadlines and individual benchmark-child deadlines
still apply. Exhaustion remains a hard failure with partial evidence retained;
longer budgets do not reduce counts, controls or correctness checks.

`build-gd-bc.py --integer-functions ON|OFF` verifies both the configured option
and actual VM/engine compiler definitions and source inventory. Native Visual
Studio Release projects are retained when compile commands are unavailable.
The arithmetic A/A independently builds OFF; math A/A independently builds ON,
matching the existing controllers' control definitions. Raw outcomes, actual
hosts, commands, policies and artifacts remain separate per job; no favorable
rerun or pooling is implied. Green workflow status remains distinct from every
class's runtime/resource acceptance. Same-source mode isolation must also be
followed by final integrated-tree qualification against accepted main.

The state- and compiler-isolation topics start with three-platform arithmetic.
That stage tests the structural hypothesis while preserving the earlier source
and its separate fixed Windows confirmation. It does not replace full/holdout
math, resource or final integration requirements. A separate immutable
qualification topic or manual replay runs all suites before any adoption.

### Focused ownership coverage

A separate Apple LLVM 21 collection at runtime source
`2aba8ec2cc1ed23a3806079d2f6dc1caa6cfc27d` instruments only
`integer-function.cc` and `integer-number.cc`, reusing the unchanged Release
objects elsewhere. The unit executable is supplied first to `llvm-cov`, with
the CLI executable as an additional object. This measures those implementations
and their mapped header code; it is not whole-program coverage, a timing run,
or a substitute for native hosted checks.

The collection checks the integer unit cases, 32 programs against independent
numeric oracles and a separate allocation-profile binary, and 25 VM CLI tests.
The self-move follow-up verifies that a populated bank preserves both borrowed
cells and its logical charge when move-assigned through an alias of itself;
subsequent arithmetic must still consume those cells correctly. A populated
bank must also report nonempty. These tests change no runtime implementation.

After that follow-up, `integer-number.cc` covers all 126 mapped lines, 52 branch
outcomes and four supported MC/DC conditions. The ordinary integer unit suite
passes 11,066 assertions in 14 cases; its profile counterpart passes 14,155 in
15 cases. The wider `integer-function.cc` collection remains at 289/313 lines,
216/256 branch outcomes and 19/43 supported MC/DC conditions. In addition,
LLVM explicitly excludes the nested Boolean multiplication-overflow expression
from MC/DC instrumentation. The supported denominator must not be presented as
proof for that expression or for every planner/engine guard. Raw profiles,
objects, binaries, commands, source hashes and before/after exports are retained
separately from performance artifacts.

### Signed-zero integration prerequisite

Unsigned boundaries must accept exact numeric zero regardless of its stored
sign before the integer tier can be adopted. A cold generic function returning
`-0` and its prepared integer version must behave identically when the result
sets `scale` or indexes an array. The #601 integration tests exercise both cold
and warmed calls, reject genuine negative fractions and integers in both
states, and separately assert successful tier execution and complete tracked
cleanup. The correction checks the magnitude before rejecting a negative sign;
it does not change the unsigned range or fraction rules.

This integration does not replace the fixed #627 source cohort or the earlier
standalone #602 adverse Linux result. The changed source still requires its
own hosted correctness and performance evidence before merging or adoption.

### Speculative caller boundary

The #635 experiment moves preparation, speculative execution and ownership of
the completed `Number` box into separate helpers and the lazy `IntegerState`.
The generic caller retains its scale/check/pack guards and fallback-chain scope.
It consumes the untouched argument pack only after the helper has produced the
complete box. A nonthrowing transfer helper moves that box into the return value
and immediately clears its owner. No borrowed result escapes the tier state.
The result storage is outside logical scratch, as the earlier caller-local box
was; its fixed header is included in the tracked state-metadata allocation.

Supported GCC/Clang and MSVC builds enforce the helper boundary with their
no-inline attributes. Other compilers retain the same standard C++ behavior
without guaranteed outlining. Entry eligibility, bounded scratch, rejection
memoization, source generations, interrupt checks and generic fallback are
unchanged. Tests preserve escaped promoted values across later calls, another
function's redefinition and a scale-guard fallback.

Local Apple Clang Release validation passes 1,998,416 assertions in 364 ordinary
cases and 2,001,593 in 375 profile cases. Thirty-four checked programs agree
across ordinary/profile binaries and the signed-zero parent. All tracked live
storage returns to zero; allocation counts are unchanged, while requested and
peak tracked bytes increase by 48 per VM in these programs. This is a persistent
metadata cost, not a memory saving. Ordinary and IPO allocation-failure fixtures
pass in both modes, alongside supported record-layout and native-symbol checks.
The non-isolated configuration also retains the portable interpreter behavior.

The local profile caller's span to the next symbol decreases from 3,516 to 2,876
bytes, with actual calls to both helpers. Its stack reservation remains 0x190
bytes. These compiler-specific observations supply no elapsed-performance claim
and do not predict another platform's frame. Full native qualification, resource
review and final-main integration remain required. The controlled #627 loss and
all earlier adverse/inconclusive cohorts remain preserved separately.

The interrupt ownership test also sweeps every execution poll of a finite
recursive factorial that promotes beyond the machine-integer range. At each
cut it requires abandonment, empty value/frame/argument contents, the retained
bank-header charge, an unchanged escaped exact result and successful scratch
reuse. The probe proves that some
cuts interrupt live promoted values, recursive frames and argument marks;
profile mode checks abandonment counts and restoration of tracked live bytes
after destruction. This deterministic callback test covers engine cleanup, not
OS signal delivery, caller-level recovery or elapsed performance.
The current factorial fixture visits 643 polls; its focused test passes 7,086
assertions normally and 7,096 with profiling. Both fresh full C++ suites pass.

### Separate call-entry experiment

#636 completed its nine native qualification jobs and closed unmerged. A
Windows held-out order-dominant Bessel case confirmed a slowdown above 5%,
despite executing no integer instructions in the checked local profile. The
old caller/helper measurements above are retained evidence, not acceptance
of that source or proof of the native cause.

#641 moves the speculative guards and fallback scope into a distinct call
entry. A small non-owning router forwards the caller's result destination to
either the generic or speculative entry. At nonzero scale and in full-check
mode, every entry is generic. At zero scale, eligible generations can warm
and execute speculatively. A statically rejected generation gets a generic
entry until ordinary or deferred redefinition resets it. Argument-dependent
rejection remains the separate one-value memo; it must not disable a valid
generation for all other arguments.

Scale writes and saved-special restoration select a complete table in constant
time. A failed attempt selects the generic table for its entire fallback
chain, including scale writes and nested calls. Only the initiating entry
clears that suppression, on normal or exceptional return, and then selects
the table for the current scale. Generic calls own no speculative fallback
guard and construct no speculative result box. The complete box still exists
before pack consumption and transfers without throwing or allocating.

Two shared tables each contain 26 function pointers. The lazy per-VM state
adds another 26-pointer table, and the VM tail adds one table pointer. These
are explicit storage costs, outside the bounded execution scratch. No table
is allocated merely to enter full-check or nonzero-scale generic execution.
The existing allocation accounting includes the lazy table. The VM header,
shared static image storage and RSS require separate reporting.

The router joins the engine's native object target outside project-managed
IPO; the non-isolated comparison retains portable behavior. Both objects
must pass the ownership/math symbol guard, and build provenance verifies the
actual router translation unit and its isolation in compile commands or
native Release projects. Source-level prvalue elision guarantees no
intermediate Number owner. A machine tail transfer is a separately inspected
compiler result, not a portability requirement or a performance claim.

The transition regressions precede this implementation and check both
execution-check modes and saved-special settings, redefinition of accepted
and rejected generations, and exact tier-event counts across scale changes.
Native correctness, resources and full/held-out per-workload qualification
remain necessary. #601 is still an independent adoption prerequisite; no
earlier adverse or inconclusive measurement is superseded by this design.

Routing-only source `92c7ba05` passes local Apple Release validation with
2,005,518 assertions in 366 ordinary
cases and 2,008,705 in 377 profile cases. The ordinary/profile call contracts,
including complete allocation-failure fixtures with and without IPO, pass.
Thirty-five independent checked programs agree across ordinary/profile builds
and the frozen #636 profile binary, including complete semantic event maps;
all tracked live storage returns to zero. Allocation counts are unchanged,
while requested and peak tracked bytes each increase by 208 in these programs.
That is the lazy 26-pointer table on this ABI, not a memory saving. Actual
compiler records put the VM at 6,408 bytes with the feature OFF and 6,424 ON,
preserving all common field offsets. The non-isolated CLI passes 29 tests.

The local Arm64 router is seven instructions ending in an indirect branch,
with no stack adjustment or Number/Digits definitions. Its generic-entry
adapter is six instructions ending in a branch to the generic caller. The
generic caller spans 2,680 bytes versus the prior 2,876-byte caller, but both
reserve 0x190 stack bytes. The speculative entry separately reserves 0x50.
These exact-build observations neither prove native Windows/Linux behavior
nor establish elapsed performance. Raw counters, binaries, object reports,
source/command hashes and disassembly are retained separately.

The combined candidate carries #637's terminal signed-zero conversion
into this experiment, retaining the original nonnegative continuation and
returning immediately for a negative exact-zero coefficient. Its independent
native performance remains inconclusive, and the earlier #602/#631 losses
remain binding evidence about those sources. The combined routing/conversion
source requires its own qualification before any adoption; neither bug-fix
correctness nor routing-only measurements qualify that final source.

### Library reload coverage

#643 adds an isolated public-loader fixture after the frozen #642 source.
It warms a pure function, retains an escaped exact promoted result, reloads
the real library and checks that its ordinary binding replaces the old plan.
It also warms a statically rejected generation and redefines a deferred
library function before materialization. Both full/verified execution and
saved-special settings are exercised. The exact zero oracle and independent
cubic integer result check outputs and the retained value, while profile
events prove actual tier execution and promotion where enabled. Each mode
must restore every tracked live allocation category after VM destruction.

The fixture uses the native image when configured and parses the identical
maintained source otherwise. It adds no production test hooks or runtime
changes. Local ordinary image, profile image, profile source and feature-OFF
profile configurations pass all four VM modes. The ordinary/profile CLI
binaries are byte-identical to the parent in the checked Apple builds; this
is not a claim about Windows PE timestamps or other toolchains.

Native semantic/profile/sanitizer selections include the new fixture. These
checks cover loader replacement and ownership, not allocation-failure
injection, OS signals or elapsed performance. #642 remains held because its
actual Windows router objects export Number/Digits destructors; passing reload
coverage neither removes that isolation failure nor adopts the parent.

### Earlier router ABI preflights (unmerged)

#645 follows the concrete Windows object failure. The router and callbacks
now use identical free-function signatures, including a borrowed entry-context
pointer. Each table cell still contains one function pointer, and the context
does not own VM or Number state. Exact signature matching keeps argument
positions consistent across the forwarding boundary; a machine tail transfer
remains a separately inspected compiler result. Scoped explicit-instantiation
declarations in the router translation unit keep Number/Digits ownership
definitions in the ordinary library context. Native Windows confirmation of
that intended boundary is required before accepting the repair.

The symbol collector tries the configured adapter, available LLVM and generic
nm adapters in order, deduplicates equivalent paths and retains failed attempts
as well as the selected commands and outputs. A missing or incompatible
configured adapter no longer hides an available compatible one. Genuine
adapter absence remains explicitly unavailable. Borrowed Number signatures
are accepted; actual ownership definitions and missing engine/router identities
fail. Eight independent adapter tests cover fallback, rejection, exceptions,
timeouts, exhaustion, identity and the observed COFF destructor definitions.

Three regression tests fail on the previous collector before the repair.
The new collector also correctly rejects the preserved old Windows objects
through LLVM fallback. Fresh local ordinary/profile suites pass 2,005,518 and
2,008,705 assertions, with allocation-failure, loader, layout, symbol and profile
contracts passing. Thirty-five independent programs preserve complete semantic
event maps and allocation totals against #642; the non-isolated CLI passes
29 tests. The local Apple Arm64 router has three instructions ending in an
indirect branch, without stack adjustment. These are local correctness and
compiler observations, without an elapsed-performance or Windows acceptance
claim.

The exact repair branch runs native correctness/object preflight with timing
jobs skipped. #642 and #644 remain frozen. A new runtime performance cohort
requires successful preflight, completion and preservation of #642's declared
cohort, and a separately frozen qualification source. No prior losing or
inconclusive observation is superseded by this repair.

The first native preflight at `0a2b26a4` exposes the ownership failure with a
working Windows LLVM adapter: both defaulted destructors are still exported.
Matching signatures shrink that router from 0x33 to 0x28 bytes, while its
saved RBX and 0x40-byte stack reservation remain. Numeric, reload and lifetime
checks pass; this does not establish the required compiler boundary.

The follow-up targets explicit instantiation declarations at the two defaulted
destructor members instead of their enclosing classes. The ownership guard
continues to reject every Number/Digits definition; it gains no exemption.
Local ordinary/profile suites, eight/seven focused contracts, 35 exact
programs with matching event maps/allocation totals and the non-isolated CLI
pass locally. Native preflight at `c65b776c` fails: GCC rejects the
defaulted-destructor declarations, and MSVC still exports both destructors.
PR #646 is closed without merging. Both preflights' objects, binaries,
failed reports and logs are retained separately.

### Opaque routed return ownership

#647 replaces both failed suppression approaches. The native router includes
only the minimal Letter declaration and standard array support; Number and
the large VM header are absent. Its matching callbacks return void and carry
borrowed opaque argument/result pointers. All numeric construction, moves and
destruction belong to the ordinary compiler context. The Letter declaration
and public binding are unchanged by extracting its minimal header.

Generic table entries are null and enter the existing generic body directly.
Only speculative entries use the opaque transport. An ordinary-context helper
owns its return storage behind the supported helper boundary, keeping that
object out of generic callers. The incoming Number is completely constructed
before the helper's `construct(Number&&)` call begins. Placement construction
inside aligned union storage performs only a nonthrowing move; ownership is
then recorded. A throwing argument expression never enters that call. Taking
the result performs a second nonthrowing move, then destroys the moved-from
owner normally. This retains the complete-box-before-pack-commit rule and
adds no return allocation.

Those transfers are real costs: profile mode records two extra digit moves
per opaque routed call. They are not hidden or compared as unchanged counters.
The initial factory-based transport recorded one extra move per call; its first
35 independent checked programs preserve every other event and all allocation
totals against #642, while reporting 8--40 extra moves per program. Those
original profiles remain separate from the completed-value implementation.
Generic/full-check calls bypass the transport. Native stack/code costs and
elapsed behavior still require actual measurement; a smaller router alone
does not establish an improvement.

Focused tests cover empty and partial initialization, a throwing factory that
has already allocated, recovery, escaped heap aliases and the exact extra move.
The existing full VM, 643 interruption cuts, ordinary/IPO allocation-failure
sweeps, loader reload and generation/state tests remain required. The symbol
guard is unchanged and rejects every native Number/Digits definition. This
topic runs correctness/object preflight with timing skipped; no native repair,
performance adoption or #197 completion is inferred from local results.

The first opaque-transport preflight at `f8e50c43` passes GCC, Apple Clang
and sanitizer jobs. MSVC rejects the always-throwing test factory during
link-time code generation: the ownership publication after that factory is
provably unreachable (`C4702`, treated as an error). The fixture now reads a
volatile condition before throwing, leaving both factory outcomes reachable
without changing production code or weakening the ownership guard. The
original failed source, logs and objects remain retained; fresh native
validation is required for the corrected fixture.

At `1a8c86a2`, the corrected fixture builds on MSVC and its native symbol
guard passes. Nine of ten semantic jobs pass; the Windows profile job reports
68 tracked bytes still live after the named-return throwing factory. This
does not yet distinguish actual retained storage from a counter or compiler
cleanup problem. Keep that failing test and add direct named-return and
opaque prvalue-return controls, plus all four direct/opaque and named/prvalue
combinations in the ordinary-allocation lifetime executable, with and without
IPO. Preserve the failed source and archives; production code and adoption
requirements remain unchanged while these controls establish the cause.

At `38e06ac7`, Windows profile controls pass for a direct named return and
an opaque prvalue return, but the original opaque named-return case still
retains 68 tracked bytes. The ordinary-allocation matrix was incorrectly
guarded by the private library macro and did not compile into its consumer
fixtures. Use their existing `GD_BC_EXPECT_INTEGER_FUNCTIONS` definition;
the previous lifetime passes do not prove that matrix. Retain the failing
case and rerun the correctly enabled ordinary/IPO controls.

The completed-value implementation removes factory invocation from the
placement-new expression. Direct named-return cleanup and opaque prvalue
cleanup passed in the native controls; the original combined expression did
not. Requiring a fully constructed Number argument separates those lifetimes
portably, without a compiler-specific exception or weaker ownership checks.
Keep the original named-return failure/recovery scenario and both controls,
and account for the extra publication move. Full local ordinary/profile
suites and the correctly enabled lifetime controls pass; fresh native
confirmation remains required before qualification.

The completed-value 35-program differential preserves exact outputs, every
non-move event and every allocation category against #642. Its 16--80 extra
moves per program are exactly twice the initial transport's counts. Keep
both raw collections and their source/binary identities; neither collection
measures elapsed performance.

### Completed-value native qualification

#649 prepares the dedicated `codex/bc-649-opaque-qualification` topic. Its
runtime is unchanged from `c5685ae7`; only workflow selection and this protocol
are added. Do not open its performance PR until the completed-value native
preflight establishes cleanup and object isolation. The rejected #642 cohort
is complete, with all 30 archives retained; its Windows holdout loss remains
authoritative.

Freeze and declare the final source before opening one draft PR against the
accepted main anchor. Run nine default-OFF versus main jobs and nine independent
same-source OFF/ON jobs: arithmetic, full math and held-out math on native
Linux/macOS/Windows. Each arithmetic job retains both full and held-out inputs.
Use the existing controls, certified inputs, seeds, samples, duration targets,
confirmation policy and budgets. The plain branch name avoids duplicate
push-only math runs; threaded compatibility reports remain separate.

Retain every source-bound archive, host/toolchain/binary identity, adverse row
and inconclusive outcome, verifying provider hashes and ZIP integrity. Review
per-workload time, resources and code size; green advisory jobs do not establish
acceptance. Linux/Windows keep the 5% allowance and macOS policy is unchanged.
Do not pool gains, waive losses or replay a failed source for favorable timing.
Any later runtime change needs a separately declared frozen evaluation. This
protocol adds no local implementation elapsed benchmark or adoption claim.
