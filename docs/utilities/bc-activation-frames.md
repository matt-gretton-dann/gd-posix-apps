# Activation storage

Arguments occupy a contiguous vector of scalar/array values. Each active
argument pack records its beginning and next unread offset. Nested argument
expressions may grow the vector; no pointer or iterator is retained across a
call. Consuming parameters preserves source order. Popping the pack releases
its values and retains capacity for later calls.

Verified functions can bind one scalar parameter directly when the prepared
body starts with the usual extraction, variable, store and marker sequence.
The binding must be among the saved locals. The remaining bytecode must
neither read a result from the prologue nor branch into it. Argument
evaluation and staging retain their order, and binding uses the same
argument type and pack-size checks. The direct-binding path is disabled
during full checks, instruction profiling and Windows builds. The Windows
fused build exceeded the 5% regression limit in the native comparison
cohort.

Saved locals occupy a separate contiguous vector. A call records its starting
offset, moves the selected scalar and array values into it, and installs empty
locals. After execution it restores those values from the recorded offsets.
Recursive calls can grow the same vector without invalidating the offsets.
The previous redundant copy of the entire saved-local list is removed.

Each simultaneous instruction-pack activation borrows a result vector from a
VM-owned deque. Deque growth preserves caller references. The pack destructor
clears its values on normal return or exception, then releases its depth slot;
vector capacity remains available for subsequent calls. Separate depth slots
prevent recursive or mutually recursive calls from overwriting live results.
Function instructions remain persistent and retain their existing definition
replacement and runtime-base literal-cache contracts.

These buffers retain capacity up to the largest observed argument, local and
result demand until VM destruction. Diagnostic allocation categories describe
selected allocator payloads, not process RSS or every allocator's metadata.
A regression compares one call with 200 sequential calls of the same shape,
asserting identical frame/result allocation counts and exact results rather
than a wall-time threshold. Existing VM and CLI oracles cover nested and mutual
recursion, array snapshots, arguments, local restoration, redefinition and saved
special variables.

Ordinary calls retain the existing host-recursive control flow. The measured
layout reduces allocation costs and preserves return and unwind behavior. The [acceptance report](bc-208-frames/accepted-native/README.md) records
the decision to retain host recursion, complete native evidence and limitations.
Deep-recursion limits remain #184 work; retaining capacity does not establish a
new recursion-depth guarantee.

## Exact integer self recurrences

On the first verified call, the VM can prepare a portable reduction for a
narrow family.
The verified instructions must extract one scalar parameter, choose between
a base return and a single decreasing self call, and combine the recursive
result with the parameter by addition or multiplication. The base returns
the parameter or a signed single-digit literal. The threshold and positive
step are also derived from single-digit literals. This keeps the proof
independent of the caller's input base. Instructions that access another
binding, affect special registers, store values or call another function
keep ordinary execution.

Only a complete pack with a nonnegative, scale-zero inline argument can
use the reduction. Every intermediate integer remains scale zero at any
caller scale. The additive case uses the signed sum of an arithmetic
progression. The multiplicative case starts from the terminal value and
multiplies the exact factors in unwind order. The step and digit-work
limits bound resources before evaluation. The VM polls for interruption
and consumes the argument pack only after it has the complete result.

Full-check execution, unproved definitions, unsupported inputs and
interrupted or failed reductions retain the original call path. Replacing
a definition discards its reduction. The VM allocates the reduction table
only when it finds a match. It sits outside the ordinary function entries
and after the other VM state, preserving the offsets of existing fields. Differential tests compare
ordinary recursion with the prepared path. An allocation fault sweep
checks retry and preservation of the outer binding, and instrumented
tests count one VM call for a wide factorial and a stepped sum. These
counts measure dispatch work. Hosted comparisons determine elapsed
performance and resource tradeoffs.
