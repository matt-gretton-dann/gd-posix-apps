# Number-independent recursive word executor

Issue #690 evaluates a distinct compiler boundary for the earlier portable
integer experiments (#598/#607/#635/#641/#647). Those experiments already
implemented checked word arithmetic, exact promotion and explicit recursive
continuations. Their executor nevertheless included `bc.hh` and exposed
`Number` operations. The new core includes only its decoded interface and
allocation diagnostics. All `Number` operations belong to callbacks compiled
in the ordinary library context. This separation is a hypothesis, not evidence
that shared template definitions caused any earlier slowdown.

## Core contract

`Word::Program::prepare` owns at most 1,024 decoded instructions. Operands name
earlier producer slots or one of 26 scalar locals. Explicit loads snapshot
values; local operands retain location semantics until consumption. The
instruction set contains single decimal digit constants, loads/stores, checked
signed addition/subtraction/multiplication, comparisons, branches and self-call
argument marks. All locals initially contain zero, except the scalar parameter.

Preparation rejects malformed opcodes, operand bounds/kinds, fallthrough beyond
the plan, incompatible argument-mark states and non-dominating producers at
control-flow joins. It validates bounds and kinds even in unreachable code.
The ordinary source compiler must additionally prove the function's purity,
instruction correspondence and one-scalar/self-call interface; a decoded plan
alone cannot establish those source properties.

The executor uses private value, continuation and argument vectors. It polls
interrupts before initial allocation and every instruction, and accepts only
budgets up to 1 MiB of logical live scratch and 4,194,304 dispatches. Recursive
depth is bounded by that same scratch budget. Smaller budgets are available
for deterministic guard tests. Scratch live state clears on entry and every
exit, including exceptions; vector capacity may be retained for reuse.

Checked arithmetic uses standard C++ signed-overflow guards, without compiler
intrinsics. An overflowing operation or a large input cell requests exact
arithmetic through opaque callbacks. Large values are immutable bank indices;
growth and cell overwrites must preserve existing aliases. Callbacks normalize
numeric zero to a small cell, preserve their inputs and report logical bank
bytes within the remaining budget. The core checks the reported bound again
before allowing successful publication. Missing callbacks or an unavailable
resource decline execution. The caller owns spill cleanup, final boxing and
the final interrupt check before committing an incoming argument pack.

Logical scratch counts live cell/frame/argument objects plus the callback's
reported bank bytes. It does not measure allocator overhead, retained capacity,
temporary coefficients or process RSS. Existing profiling allocators count
selected requested vector allocations; these are not complete process malloc
statistics. Execution counters currently run in ordinary and diagnostic core
builds alike, so their cost is part of any future ordinary timing image.

## Current implementation and evidence limits

The standalone `bc/word-functions` target exercises independent small native
arithmetic, signed endpoints and operand pairs, exact promoted factorial(50),
promotion/demotion, lazy locations, snapshots, joins, recursive return values,
every dispatch interruption cut, scratch/fuel failure, exceptions and seeded
mutations of decoded plans. Its test-only bank uses ordinary `Number` arithmetic
and known decimal answers. A compile-time dependency guard rejects Number,
Digits or VM headers in the actual executor translation unit on every compiler.

This component is not yet connected to VM calls. Production execution and
defaults are unchanged. Source-to-plan lowering, immutable generation ownership,
redefinition, deferred replacement, fallback-chain suppression, final pack
publication and ordinary/IPO allocation-failure integration remain required.
Do not infer a speedup, complete VM coverage or default adoption from core tests.

Before any adoption, require native Linux/macOS/Windows semantic preflight,
actual include/object/layout evidence, and a prospectively frozen full/held-out
elapsed/startup/resource cohort against accepted main. Preserve all old losing
and inconclusive cohorts and the unchanged per-workload acceptance rules. No
local production elapsed benchmarks are part of this experiment. Queue, run
and final review must fit the user's ten-day **total** #197 goal-time cap.
