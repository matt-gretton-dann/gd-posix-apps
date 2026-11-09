# Reusable result objects with prefix cleanup

Refs #694, #207 and #197. `ENABLE_BC_PREFIX_CLEAR_RESULTS` is an unmerged,
default-OFF experiment. Explicit OFF builds retain the vector-based generic
and typed buffers. No elapsed benefit or adoption is established here.

## Ownership and execution contract

The VM already retains result-buffer capacity by simultaneous activation depth.
The existing buffer clears all constructed slots at release and default
constructs all declared slots on the next acquisition. An early return can
therefore pay for result producers it never executes.

The experiment retains empty objects too. A buffer records its active logical
size separately from retained objects and the greatest mutable index accessed.
Every mutable access extends the cleanup prefix before returning its reference.
Cleanup resets that entire prefix, including conservative reads and empty
holes. All owning numbers and array snapshots are released before reuse.
Untouched objects are empty from construction or a previous cleanup; they do
not preserve language values between calls. Const reads cannot introduce an
owner. No mutable raw pointer, iterator or span bypasses access tracking.

Generic slots reset their variant to monostate. Typed numeric slots reset to
the existing allocation-free default Number; typed nonnumeric slots reset to
monostate. Numeric zero construction during reset remains work, separately
from buffer object initialization. The Number representation and public API
are unchanged. Full-check and verified execution use the same ownership rule.
Typed numeric cleanup destroys the old Number and constructs its default zero
directly in the retained slot. This releases its old coefficient without a
temporary-zero move assignment. All slot borrows end with the activation;
escaped language values keep their ordinary ownership.
Typed execution accesses the owning buffers rather than cached raw pointers.
Generic compact execution obtains a destination only for an operation that
produces a value. Eagerly touching a branch or EOF destination would otherwise
expand cleanup to the instruction index when slot compaction is unprofitable,
even though that instruction stores nothing. The early-prefix test covers this
identity-map case as well as full-check and typed execution.

Each active depth still owns stable independent storage. Escaped results retain
their ordinary value ownership. A failed growth publishes no activation. Copy
assignment stages storage and bounds together; moves leave the source inactive
and reusable. Changing generic/typed format releases the old buffer before
constructing its replacement. Exception cleanup clears any first lane whose
acquisition succeeded before a second lane failed.

Each buffer adds two `size_t` bounds. Existing typed-layout admission charges
the actual buffer `sizeof`, including those fields, rather than assuming a
particular ABI. Retained objects can extend to the largest prior activation;
their owners must be empty after cleanup. Requested allocator bytes and process
RSS remain separate measurements. This portable C++ implementation needs no
architecture-specific kernel; the OFF representation remains available.

## Diagnostic counters and tests

Separate `ENABLE_BC_PROFILE=ON` images report completed
`result_slot_initializations` and `result_slot_resets`. The first counts new
backing slots when a result vector grows, excluding reallocating moves and all
reset construction, including in-place Number replacement. The second counts
slots visited during activation cleanup:
the full active size in OFF builds, or the accessed prefix in ON builds. These
counts do not count every Number constructor, complete malloc traffic or CPU
time. Failed growth contributes no completed initialization event.

Seven standalone component cases exercise ownership release, escaped values,
early/late access, smaller reuse, throwing operations, failed initial/retained
growth, failed copy assignment, moves, copies and 300 simultaneous depths.
One finite case constructs 512 objects once and resets 800 slots across 100
eight-slot activations. It measures work, not elapsed performance.

The VM profiling regression warms a real 512-producer pack, executes only its
early prefix and then repeats it 30 times in both full-check and verified
modes. It requires exact heap-coefficient output, no new backing slots with
ON, at most 90 cleanup visits, and at most two ordinary digit
moves per execution. Clearing a numeric lane must not add temporary-zero moves;
fewer ordinary transfers remain valid. OFF must reconstruct and
clear at least 15,360 slots. Existing mixed-kind, recursive snapshot, array,
allocation-fault, signal and CLI tests remain required in each relevant mode.
Sanitizers and native compiler checks supplement these finite contracts.
The topic's native correctness workflow selects six modes on each of Linux,
Windows and macOS: ordinary ON, explicit OFF, generic/profile, typed/profile,
wide/profile and native-function coexistence. It retains actual compiler-mode
inventories, changed source inputs, VM objects, correctness images and logs.
This preflight dispatches no elapsed qualification cohort. Only this topic's
redundant narrow PR timing jobs are skipped; main, manual, scheduled and
unrelated selections retain their existing behavior.

## Qualification limits

This source is separate from the frozen Bessel and compact Word studies. Their
measurements do not qualify this experiment. The Bessel early-return regression
motivates investigation, but no causal attribution follows from these changes.
Extra prefix tracking affects hot result access and can cost more than the
initialization/cleanup it avoids; retained storage and code size also require
review. No local production elapsed benchmark is run.

Adoption requires a new prospectively frozen full/held three-platform arithmetic,
math, startup and resource study, all final-head correctness checks and unchanged
acceptance thresholds. Preserve adverse and inconclusive results. The ten-day
TOTAL #197 goal clock includes implementation, queues and qualification; stop at
864,000 seconds without closing unmet issues or extending the limit. Keep at
least 20 GiB free and preserve active source and unique evidence.
