# Activation storage

Arguments occupy a contiguous vector of scalar/array values. Each active
argument pack records its beginning and next unread offset. Nested argument
expressions may grow the vector; no pointer or iterator is retained across a
call. Consuming parameters preserves source order. Popping the pack releases
its values and retains capacity for later calls.

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

This layout retains the existing host-recursive call control flow. The measured layout
addresses the allocation cost while preserving existing return and unwind
behavior. The [acceptance report](bc-208-frames/accepted-native/README.md) records
the decision to retain host recursion, complete native evidence and limitations.
Deep-recursion limits remain #184 work; retaining capacity does not establish a
new recursion-depth guarantee.
