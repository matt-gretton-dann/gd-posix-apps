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
Compiler/runtime validation uses hosted CI while a separate local benchmark is
running; a local diff check alone is not a build or execution result.

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

Keep the tier experimental until differential semantic tests, cold and warm
full/holdout measurements, and native Linux/macOS/Windows resource evidence
justify adoption. Preserve adverse and inconclusive classes; the overall #197
competitive requirements remain open.

## Native qualification recipe

`bc-integer-performance.yml` compares OFF and ON at the same immutable source
on Linux, macOS and Windows. It starts for the fixed experiment topic and also
supports manual replay with a full source SHA. Ordinary PRs retain the separate
fast semantic workflow. Arithmetic includes the full/holdout corpus and fixed
sparse latency/duration controls. Math runs full and holdout separately with a
0.25-second calibration target, 600-second child deadlines and a bounded total.

`build-gd-bc.py --integer-functions ON|OFF` verifies both the configured option
and actual VM/engine compiler definitions and source inventory. Native Visual
Studio Release projects are retained when compile commands are unavailable.
The arithmetic A/A independently builds OFF; math A/A independently builds ON,
matching the existing controllers' control definitions. Raw outcomes, actual
hosts, commands, policies and artifacts remain separate per job; no favorable
rerun or pooling is implied. Green workflow status remains distinct from every
class's runtime/resource acceptance. Same-source mode isolation must also be
followed by final integrated-tree qualification against accepted main.

The state-isolation topic starts with the three-platform arithmetic stage.
That stage tests the structural hypothesis while preserving the earlier source
and its pending fixed Windows confirmation. It does not replace full/holdout
math, resource or final integration requirements. A separate immutable
qualification topic or manual replay runs all suites before any adoption.
