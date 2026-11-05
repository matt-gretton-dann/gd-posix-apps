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
field offsets must match; total VM size can include the tail owner pointer.
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
growth and cell overwrites preserve aliases. Logical scratch charges actual
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
library policy. The native engine owns its snapshots and receives the exact
result through an out-of-line call. It does not instantiate those arithmetic
routines itself. This changes neither the accepted operations nor the checked
machine path, generic fallback, scratch guards or mathematical algorithms.
The build helper additionally verifies the bridge's compiled source/definition
alongside the VM and engine.

`bc/integer-symbols` inspects the actual native engine object, requires its
entry points and rejects defined Number/Digits addition, subtraction and
multiplication routines. It retains object/tool hashes and raw symbols. A missing
or incompatible symbol adapter is explicitly unavailable rather than passing
proof; portable numeric and ownership tests still run. Native workflow artifacts
retain this evidence and the actual record-layout checks. The original isolated
object fails this guard with generic arithmetic definitions present. Removing
them does not by itself establish a runtime gain or explain previous losses.

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
