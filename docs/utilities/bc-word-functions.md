# Number-independent recursive word executor

The compact-slot and generic-call follow-up requires its own native correctness
preflight and prospective full qualification. The original frozen activation
study at `516f50c2` was rejected for three confirmed Windows scale-20 math losses
and its PR was closed unmerged after all nine outcomes were retained. Those
results do not qualify the new candidate. Local storage reductions alone cannot
resolve the general-path regressions.
The topic skips ordinary narrow PR timing jobs during preflight; this does
not waive full/held, startup or resource requirements before adoption. Keep
the full qualification and total timebox requirements below.

Word-only builds leave the generic call body's inlining choice to the compiler,
as the original VM does. Native-leaf configurations retain their deliberate
out-of-line generic fallback. Word entry preparation and failure cleanup remain
outlined in both configurations. This removes an extra compiler constraint on
ordinary work; it does not guarantee inlining or establish a fix for the Windows
losses. No argument, publication, allocation, lifetime or fallback contract is
changed. Native correctness and new elapsed evidence must test that hypothesis.

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
argument marks. Decoded local names still cover all 26 letters. After checking
the complete decoded plan, preparation rewrites only its owned copy to dense
private slots for the referenced names and parameter. Each activation reserves
those slots rather than all 26 possible names. Referenced slots initially
contain zero, except the scalar parameter; unreferenced bindings are unobservable
because the source compiler has already proved the body pure. Producer indices,
branches, argument marks and source instruction storage stay unchanged.

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
statistics. Both core source configurations expose execution counters;
optimizers may remove unobserved ordinary updates. Qualify the actual ordinary
image separately from the diagnostic counter image.

## Current implementation and evidence limits

The standalone `bc/word-functions` target exercises independent small native
arithmetic, signed endpoints and operand pairs, exact promoted factorial(50),
promotion/demotion, lazy locations, snapshots, joins, recursive return values,
every dispatch interruption cut, scratch/fuel failure, exceptions and seeded
mutations of decoded plans. Its test-only bank uses ordinary `Number` arithmetic
and known decimal answers. A compile-time dependency guard rejects Number,
Digits or VM headers in the actual executor translation unit on every compiler.

`lower_word_function` runs in the ordinary compiler context. It accepts already
schema/type-verified VM instructions, proves the parser's one-scalar prologue,
and rejects globals, arrays, special registers, other callees and unsupported
operations throughout the body. Relative offsets are bounded before addition;
lowered producer indices and branches exclude the consumed prologue. Single
decimal digit literals preserve runtime input-base semantics. The decoded plan
then receives the core's independent control-flow/argument-state checks.
Differential tests compare parsed expressions, local stores, loops and nested
self-calls against independent full-check VM states, and retain plans after
changing/destroying the source. These tests do not exercise a VM tier route.

## Experimental VM integration

The qualification candidate defaults `ENABLE_BC_WORD_FUNCTIONS` to ON; this is
an unmerged experiment pending the full acceptance review described in
`bc-word-function-qualification.md`. Explicit OFF builds retain the generic
implementation. Full-check execution remains generic. The VM accepts exactly representable
scale-zero signed 64-bit arguments, including both endpoints and signed zero.
Inline one-limb arguments use the existing unsigned conversion; other possible
word values use existing read-only decimal output and checked `from_chars`.
Other inputs retain ordinary Number execution. No Number API is added.
The accepted operations preserve scale zero even when the VM's global scale
changes; the proof rejects reads or writes of that special register.

Each function generation has an allocation-free, saturating eight-call warmup
counter at the VM tail. Scale/length guards precede counting; exact signed-range
conversion waits until warmup completes. Only a hot eligible call creates the
lazily owned tier table. Its at-most-26 entries own decoded programs and static
rejection flags; ordinary definition replacement and deferred-library replacement reset both
the counter and entry. No field is added to the common function records or
pre-existing VM prefix. Unsupported bodies cache rejection before subsequent
numeric conversion checks. The existing native leaf tier has priority when
both optional tiers are enabled; recursive and store-containing functions can
still use the word tier.

The ordinary-context boundary owns immutable promoted Number records. It
charges each live record's Number object plus decimal digits times limb size,
rejecting values above 4,096 decimal digits. This conservative coefficient
estimate is not retained heap capacity. The fixed tier table, counter array,
vector headers and result owner are bounded metadata outside the 1 MiB
per-attempt logical payload bound and must be measured separately. Dead spill
records stay charged until the attempt ends. Selected profiling allocators
track requested table/vector/limb storage; they do not include every process
allocation or all arithmetic temporaries.

All throwing execution and boxing finish before the VM consumes an incoming
argument pack. Successful publication moves the boxed owner without allocating;
escaped values can outlive both later calls and the tier owner. Scratch and
spill live state clear on every exit. An abandoned attempt enters an outlined,
exception-safe suppression scope until its generic fallback call chain returns.
One deterministic failing word argument is remembered per generation and also
enters that suppression scope. Interrupted attempts and allocation failures do
not poison that hint and remain retryable. The routing frame owns no speculative
Number or fallback cleanup object.

Diagnostic `word_function_attempts` count core executions; completions count
successful VM pack publication. Abandonments also include conversion and
preparation allocation failures, so they need not be bounded by attempts.
Instruction/self-call work survives exception cleanup in the diagnostic counts.
Promotion/demotion events describe the ordinary-context spill boundary; generic
VM counters retain their interpreter-entry meaning.

VM differential tests cover ordinary/full checks, changed bases/scales,
argument snapshots, globals, redefinition and escaped promoted results. Existing
allocation-failure tests remain intact; additional ordinary/IPO sweeps cover
first plan preparation, promotion, publication, generic fallback and retry.
The five-case POSIX signal fixture warms eight eligible calls before its
readiness markers. It includes an interrupted pure function with a backedge,
checking parameter/auto restoration and later calls through the same generation.
It tests recovery around calls without claiming a particular instruction PC.

`bc-word-function-correctness.yml` builds explicit ordinary, profiling, OFF and
native-leaf coexistence configurations on native Linux/macOS/Windows. It retains
actual VM/test compiler definitions, core object ownership, source bytes, fault/semantic/CLI logs and
explicit unavailable PTY skips. Enabled profiling VM cases require positive
completion counters; full-check and OFF cases require generic execution. This
does not establish dynamic ordinary/profile path identity or a runtime speedup.

The former signed-zero prerequisite #601 is already accepted on this baseline:
`Number::to_unsigned` accepts an exact zero with either stored sign. Do not
reintroduce the old assumption that it remains unfixed or add a new conversion
method to implement this tier. General Number ownership/layout stays unchanged.

The immutable builder's `--word-functions AUTO|ON|OFF` option records the actual
production VM definition, all three library translation units, and six source
inputs. Test-only copies do not establish production activation. Explicit modes
require a declared boolean source option; an older source can establish absence
from its actual VM inventory. Unknown selection never satisfies qualification.
Both arithmetic and math controllers accept `--require-candidate-word-functions`
to reject inactive candidates or unverified generic baselines before sampling.

With `--compare-resources`, matched ordinary and diagnostic builds retain source,
toolchain and mode identities. Diagnostics follow all elapsed stages, preserve
their instrumented images separately, and require actual word completions in
call/recursion probes. Additional fixed controls cover all 26 simultaneous
caches, cold entry, signed endpoints and out-of-range/fractional fallback,
promotion, demotion, and static effect rejection. Counts describe selected
containers; cold RSS pairs are descriptive, and neither proves ordinary/profile
dynamic path identity or process-wide allocation bounds. The control machinery
reuses the retained native qualification experiment from #686; its rejected
measurements remain separate from this new tier.

Before any adoption, require native Linux/macOS/Windows semantic preflight,
actual include/object/layout evidence, and a prospectively frozen full/held-out
elapsed/startup/resource cohort against accepted main. Preserve all old losing
and inconclusive cohorts and the unchanged per-workload acceptance rules. No
local production elapsed benchmarks are part of this experiment. Queue, run
and final review must fit the user's ten-day **total** #197 goal-time cap.
