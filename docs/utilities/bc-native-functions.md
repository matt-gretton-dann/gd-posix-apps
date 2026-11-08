# Experimental cached native functions

Issue #683 explores a separate way to reduce function-call and expression
dispatch costs under #207 and #197. `ENABLE_BC_NATIVE_FUNCTIONS` defaults to
`OFF`; this option is independent of `ENABLE_BC_NATIVE_SCALAR`. No performance
benefit or default activation is established by the implementation or its
correctness workflow.

Backends support x86-64 Windows/POSIX and ordinary Apple ARM64 targets with a
matching compiler. ARM64EC, Apple arm64e, universal Apple builds and other
architectures retain generic execution even when the option is requested.
The CMake capability probe checks compiler target macros as well as the configured
processor.

## Eligibility and ownership

Only verified functions with the parser's one-scalar parameter prologue and
straight-line parameter loads, single decimal digits, addition, multiplication
and a return are eligible. There are at most 132 source instructions and 64
native operations. Stores, global reads, branches, calls, arrays, I/O, special
registers and other operations before the return reject the definition. Single
decimal digits have the same value at every bc input base; wider literals must
use the generic runtime decoder. An explicit return makes the parser's trailing
implicit return unreachable.

Both backends share bounded SSA validation and last-use register allocation.
Each immutable function definition owns its lazy executable page and structural
rejection state. Repeated calls reuse the page; redefining the function replaces
the cache. A failed compilation leaves the arguments and bindings intact and
may be retried. Full-check execution always retains the generic interpreter.

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
successful retry. The dedicated workflow checks Linux/Windows x64 selection
and macOS ARM64 native execution plus explicit OFF fallback configurations,
preserves the compile database and runs VM, CLI, signal, math and lifetime
regressions.

`native_function_attempts` counts eligible-domain kernel attempts and
`native_function_completions` counts boxed results committed by the VM. These
are software diagnostics, not speed measurements. Emitter vectors, shared-owner
headers, strings and executable mappings are outside the selected-container
allocation counters; code pages also require separate process-memory review.
Compilation cost, cache footprint, cold startup and unrelated workloads remain
part of any later acceptance study. This feature is not a general bc JIT and
does not accelerate recursive or branching function bodies.

## Qualification provenance

The immutable-source builder records `native_function_policy` separately from
the loop tier. It verifies the VM presence definition and selected backend in
the compilation database or native Release project. Enabled leaf builds also
retain the backend and its same-directory shared planner with byte hashes.
Requesting the CMake option on an unsupported target remains a recorded generic
fallback. `--require-candidate-native-functions` rejects that fallback, an ignored
option or a missing planner before either comparison controller starts sampling.

Matched resource diagnostics require the ordinary and instrumented roles to
agree on source, compiler, mode and planner inputs. Compilation does not prove
execution: call diagnostics must complete native calls, and fixed overflow
cases must attempt then decline the kernel before generic replay. Separate
full/held cache inputs keep all 26 function caches alive together, requiring
1,664/1,378 native completions. Cold RSS observes process memory separately;
selected-container allocation counters exclude executable mappings and compiler
buffers. Profiling can still change other execution paths, particularly Apple
scalar argument forwarding, so these counts do not establish ordinary-path
identity or replace elapsed, startup and code-size review.
