# Verified typed result storage (#449, #207, #197)

This work starts from accepted main edd686a5 and retains its numeric ownership
and algorithms. The ownership prototypes in #446 are separate experiments.

## Planning and lowering contract

The verifier already establishes operand schemas, producer kinds and dominance.
Typed planning uses that proof; it does not establish availability itself and
must never make invalid bytecode valid by supplying default numeric values.

Each numeric producer receives a distinct Number register. Each string,
location or array-value producer receives a distinct nonnumeric register.
Both index spaces start at zero; there is no empty shared result slot. The
map marks non-producers and skipped definition bodies with no_register. It
does not reuse registers at joins, backedges or last uses. Forward references
retain their verified execution order even when the producer is later in
source layout. Separately registered functions receive independent plans.

The caller supplies actual slot, fixed-object and executable-record byte
costs. Planning charges both lanes, all map entries and metadata/buffer
objects against the existing generic layout. Equal or greater cost, arithmetic
overflow, missing numeric results, tiny transient streams and unrepresentable
indices keep generic execution. Compiler-specific vector/variant sizes must
not be replaced with assumed constants in the VM integration. Unit fixtures
use explicit synthetic sizes to test exact profitability boundaries.

Typed lowering shares operand encoding with the existing compact stream.
Data offsets become their producer's lane index, while branch destinations
remain absolute source PCs. Strings, locations, function masks and literal
caches remain owned by the source sidecar. Load, store, print, length and
argument push acquire lane-specific operations; numeric arithmetic can then
address its lane without a Number-versus-location variant check. Diagnostic
print retains every supported nonnumeric alternative. Non-producing result
fields are ignored, so their zero value does not claim numeric register zero.

Disabled, unprofitable or unsupported typed layouts must preserve the existing
checked/wide/compact representations and their complete operand domains.
Neither static kinds nor cached plans freeze ibase, scale, variable/array
bindings, function definitions or argument snapshot/evaluation order.

## Delivery state

Planning/lowering and their contract tests are implemented. The VM still
executes its existing result representation; runtime integration, typed-path
lifetime/failure checks, fresh mode-specific coverage and performance evidence
remain pending. No speedup or #449 completion is claimed at this stage.

The Debug foundation passes 140 focused assertions in four typed-plan cases,
all 325 unit cases / 1,669,178 assertions, and all 92 bc CTest entries (47 pass,
45 declared skips). This includes the original compact/generic execution paths.
It is not sanitizer or active typed-interpreter evidence.

The new tests cover independent lane numbering, profitability and overflow,
polymorphic operations, forward producers/backedges, skipped bodies and function
plans, tiny-stream fallback and malformed plan bounds. Existing binary numeric
oracles now include harmless unused literals before EOF to make those exact
programs eligible for lowered execution without changing their observed result.

Integration must retain stable activation buffers and release every owning
value on normal, exceptional and interrupted exits. Begin with ordinary
per-depth capacity reuse. Liveness reuse, lazy cleanup, shared register windows,
direct argument destinations and JIT remain separate experiments. Compare
independent VM states and literal caches with checked execution and exact
oracles, including array COW, saved specials and redefinition.

Adoption requires source-bound full/held-out native results, local paired
confirmation, startup, memory/allocation and code-size evidence, with all losing
and inconclusive classes retained. Linux/Windows allow up to 5% slowdown per
change; macOS is unchanged. Smaller metadata or fewer tag checks alone does not
establish a runtime improvement.
