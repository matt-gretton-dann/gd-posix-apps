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
Full-check and allocation-profile builds use the original interpreter.

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
cell/frame/argument/spill object sizes plus a conservative
`decimal digits * sizeof(Number::NumType)` charge per owned spill. Shared
coefficients in separate spills are charged separately. Dead spill records stay
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
