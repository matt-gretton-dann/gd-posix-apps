# Verified typed result storage (#449, #207, #197)

This work starts from accepted main edd686a5 and retains its numeric ownership
and algorithms. The ownership prototypes in #446 are separate experiments.
The earlier #409 typed-pool prototype retained numeric-capable variants,
tagged references and runtime pool/fallback access branches. This design uses
lane-specific executable operations and removes Number from the nonnumeric
variant. Earlier rejected native results remain historical evidence; they do
not establish acceptance of this different source. The isolated allocation
failure/retry test is carried forward and extended to the new lane setup.

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

The baseline prototype at 400b4274 is implemented. Four ASan/UBSan
configurations pass the full 326-case unit suite and six focused CTest entries:
typed results ON/OFF, each with extensions ON/reuse OFF and extensions OFF/reuse
ON. The typed/default language configuration also passes all 92 bc CTest
entries (47 pass, 45 declared skips). Three non-sanitized profiling configurations
pass all 336 unit cases, the six focused entries and profiling CLI checks:
typed ON/OFF with compact execution ON, plus typed ON with compact execution
OFF. Each profile configuration uses extensions and prepared divisors ON.
Compile commands verify the selected VM modes and all 43 normal / 44 profiling
bc.hh consumers' language definitions.

After the full suites, focused tests were rerun in all seven configurations
with additional assertions proving persistent typed-function execution from
a tiny generic caller, including after function replacement. The tests cover
nested array snapshots and saved scale/radix state. Forward references and a
backedge to PC zero also assert actual activation through the profiling counter.
Allocation-failure sweeps retry the same VM after each injected failure in a
pack without output, global stores or argument frames; this is not a rollback
guarantee after arbitrary runtime side effects.

Its local paired matrices show gains for several arithmetic/VM classes but
a repeated 1–2% file-statement slowdown. It is not accepted under the unchanged
macOS requirement. Its native run and sixteen-mode coverage collection remain
separate evidence; a green advisory workflow alone does not establish adoption.

The current continuation replaces simultaneously stored generic/typed layout
and buffer headers with mutually exclusive variants. Each prepared owner holds
one plan and executable format. Each depth slot retains capacity only for its
current format; changing format releases the previous buffers before allocating
their replacement. Typed packs borrow lane pointers and generic packs borrow
their generic vector directly. A failed format or lane setup leaves an empty
usable slot without acquiring a depth lease. The fault sweep now exercises both
directions and retries the same VM. Typed execution is compiled only when both
typed results and compact execution are enabled; other configurations retain
generic execution. Coverage-guided fixtures now make every unary oracle eligible
and exercise a raw quit opcode in isolated checked/verified processes, asserting
its exact status and that later instructions do not run. Four ASan/UBSan and
three profiling configurations passed the unit suite and seven focused CTest
entries each. The default sanitizer configuration passed the broad bc suite,
and all profiling configurations passed the profiling CLI checks. Release
performance validation remains pending.

The Debug foundation passes 140 focused assertions in four typed-plan cases,
all 325 unit cases / 1,669,178 assertions, and all 92 bc CTest entries (47 pass,
45 declared skips). This includes the original compact/generic execution paths.
It is not sanitizer or active typed-interpreter evidence.

ENABLE_BC_TYPED_RESULTS selects the prototype. It also requires compact
execution; disabled or unprofitable preparation retains the generic path.
Checked execution remains independent. Per-depth buffers keep the current
format's capacities while releasing values at every exit. Allocation
failure during lane setup clears partially constructed lanes before the depth
lease is acquired. Nested calls grow different deque entries and cannot resize
an active lane, so cached lane pointers remain valid through those calls.
Retained capacities and preparation peaks still require measurement.

The profiling-only typed_instructions_executed counter distinguishes actual
activation from configuration. It is absent from ordinary execution work.
Internal invariant dumps retain the source instruction trace without reading
unwritten typed registers; checked execution retains its detailed value dump.
Program print instructions continue to support every old result alternative.

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

## Target defaults and generic build

Fresh configurations on Linux and Windows select typed lanes. Other targets
default to the generic VM. Set `ENABLE_BC_TYPED_RESULTS` explicitly to override
the target default. An existing CMake cache retains its selected value.

Disabling typed results or compact execution compiles out the typed
layout variant and activation buffers. Generic builds own a single
generic plan and retain one vector per call depth. The checked and wide
interpreters remain available. This policy candidate needs separate
default-build performance results on all three hosts.
The fresh macOS default and explicit overrides passed all four sanitizer
and three profiling configurations, including the wide fallback.
