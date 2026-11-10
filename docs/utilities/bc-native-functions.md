# Experimental cached native functions

Issue #683 explores a separate way to reduce function-call and expression
dispatch costs under #207 and #197. `ENABLE_BC_NATIVE_FUNCTIONS` defaults to
`ON` in the unqualified final integration candidate, independently of
`ENABLE_BC_NATIVE_SCALAR`. Unsupported targets and explicit OFF builds use
the generic interpreter. Correctness alone does not establish a performance
benefit or qualify this default activation for merging.

Backends support x86-64 Windows/POSIX and ordinary Apple ARM64 targets with a
matching compiler. ARM64EC, Apple arm64e, universal Apple builds and other
architectures retain generic execution even when the option is requested.
The CMake capability probe checks compiler target macros as well as the configured
processor.

## Eligibility and ownership

The machine-leaf path admits verified functions with the parser's one-scalar
parameter prologue and straight-line parameter loads, single decimal digits, addition, multiplication
and a return are eligible. There are at most 132 source instructions and 64
native operations. Stores, global reads, branches, calls, arrays, I/O, special
registers and other operations before the return reject the definition. Single
decimal digits have the same value at every bc input base; wider literals must
use the generic runtime decoder. An explicit return makes the parser's trailing
implicit return unreachable.

Both backends share bounded SSA validation and last-use register allocation.
Each immutable function definition owns its lazy executable page and permanent
rejection state. Repeated calls reuse the page or cached rejection, including
invalid native operation plans and excessive register pressure. Redefining replaces
both. Compilation reports resource unavailability separately, so allocation and
executable-memory publication failures remain retryable. Every failure leaves
arguments and bindings intact. Full-check execution always retains the generic
interpreter.

Inputs must be nonnegative, integral, inline Numbers below the Number limb
radix. The kernel accepts only nonnegative signed-64 intermediates. Any overflow
declines before publishing a result. The ordinary VM constructs the exact bc
result, using its decimal constructor for values wider than one limb. Only
after successful result construction and an interrupt check does it consume
the parameter pack. On ordinary Apple ARM64 calls, an eligible borrowed argument
retains its existing caller-owned result slot through preparation and execution;
fallback forwards the same borrowing contract to the generic activation.
Negative, fractional, large, heap and incorrectly shaped arguments retain the generic call semantics, including diagnostics. Native
execution never installs dynamic parameter bindings because an eligible body
cannot observe anything except its incoming scalar value.

## Generated code and fallbacks

The integer-only kernel interface contains no Number objects or C++ ownership.
The x86-64 emitter uses five volatile temporaries, preserves input registers,
never changes RSP, writes no nonvolatile registers and makes no calls. Excess
register pressure declines rather than spilling. Code includes ENDBR64 and
checked arithmetic. As a true Windows leaf it needs no frame-unwind record.
The Apple ARM64 emitter uses X9-X13 as temporaries and X14 for a high product.
It preserves X0/X1 until publication and never changes SP, FP, LR, X18 or other
nonvolatile registers. ADDS/overflow and UMULH/MUL checks enforce the same
nonnegative signed-64 domain; the entry includes BTI c. It makes no calls.
An output word is written only after every operation succeeds.

Pages are allocated writable, populated, then changed to executable/read-only;
there is no writable/executable phase. Allocation, protection, instruction-cache
publication and ordinary allocation failures decline native execution. The
page owner releases partially prepared and published mappings. Interrupts are
checked before the bounded kernel and before committing its result. Unsupported
operating systems and architectures keep the generic implementation.

The routing frame owns no speculative Number. Compilation, native result boxing
and the original generic activation use separate non-inlined functions on the
supported compilers. This bounds one source of generic-frame growth found in
earlier integer-tier experiments; actual performance still requires a new
source-bound comparison.

## Validation and limits

`test-native-expression.cc` checks independent integer oracles, overflow,
register lifetime/pressure, 256 deterministic DAGs against checked integer
oracles, exact wide outputs, dynamic bindings, function
redefinition, ibase and fallback bodies. Profile builds assert positive native
completion and exclusion under full checks. The isolated ordinary/IPO lifetime
fixtures inject failures throughout emitter allocation and require cleanup and
successful retry. They also compare ordinary allocations for one and eight
warmed calls to a register-pressure fallback: additional calls must not repeat
compiler allocation work. A subsequent redefinition checks cache replacement.
These are allocation and semantic checks, not elapsed-performance measurements.
The dedicated workflow checks Linux/Windows x64 selection
and macOS ARM64 native execution plus explicit OFF fallback configurations,
preserves the compile database and runs VM, CLI, signal, math and lifetime
regressions.

The dedicated matrix includes the four-case POSIX PTY suite in ordinary native,
profiling and explicit OFF modes. It warms a scalar function, interrupts repeated
calls and checks subsequent argument reuse and the restored global binding.
Unsupported PTY hosts report an explicit skip. This observes recovery around
calls without identifying the particular machine-code instruction interrupted.

`native_function_attempts` counts eligible-domain kernel attempts and
`native_function_completions` counts boxed results committed by the VM. These
are software diagnostics, not speed measurements. Emitter vectors, shared-owner
headers, strings and executable mappings are outside the selected-container
allocation counters; code pages also require separate process-memory review.
Compilation cost, cache footprint, cold startup and unrelated workloads remain
part of any later acceptance study. This feature is not a general bc JIT.

## Pure integer self recurrences

The same opt-in function cache can recognize a closed recurrence with one
scalar parameter: a threshold comparison, one base return and one return of
`n+f(n-k)` or `n*f(n-k)`, in either operand order. The comparison may be
strict or inclusive and either arm may contain the base return. The base
value is a single-digit integer constant or the parameter. The decrement k
is a positive single decimal digit. All literals are independent of ibase.

Recognition follows the actual immutable instructions and separately proves
both conditional paths. It permits no stores after the parameter prologue,
global or special-register reads, arrays, I/O, second call or other callee.
A redefinition discards the cached proof; a descriptor also records its
explicit self-call letter. Unsupported definitions retain normal execution.

Admitted arguments have stored scale zero, are nonnegative, use no retained
heap storage and fit the existing one-radix-digit input domain. The terminal
argument and all recurrence operands are therefore integers with scale zero.
Addition and multiplication retain scale zero at any global scale, so exact
reassociation preserves bc arithmetic. A Number accumulator computes the
result beyond machine-word range, without one VM activation per recursive
step. Subtraction, division and fractional recurrences are excluded. The
separate machine-leaf path boxes wide integer results directly in Number's
radix, avoiding decimal-string formatting and parsing.

Before arithmetic, decline more than 65,536 steps or an estimated four
million decimal-digit steps. For products the estimate uses
`steps*(steps*digits(input)+2)`; sums use
`steps*(digits(input)+digits(steps)+2)`. These are general resource bounds
for the admitted family, not a factorial-specific threshold or measured RSS
limit. Larger work retains the interpreter. Each iteration checks interruption.
All arithmetic and final result ownership complete before consuming the
incoming parameter pack. A declining or interrupted attempt leaves that pack
unchanged; allocation failure follows the existing generic fallback.

Tests compare checked and optimized execution over different thresholds,
decrements, signs, operators, base values and caller bases/scales. Explicit
recognizer assertions distinguish admission from a fallback-only test pass.
Directed checks cover a 50! result wider than 64 bits, side effects, dynamic
redefinition, fractional inputs, resource refusal and interrupted output
ownership. Profiling proves one outer VM call for the real factorial source
shape. Function-call counters count executed VM dispatches, so folded internal
calls do not increment them. Counters establish selection, not speed.

The final integration candidate enables the option on supported targets,
with explicit OFF modes and unsupported-architecture generic fallbacks.
Correctness, memory/code-size review and source-specific hosted performance
qualification are required before merging that default activation;
no earlier native or numerical candidate's timing results transfer.
