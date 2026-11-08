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
argument type and pack-size checks. Other prologues, full checks and
instruction profiling execute the original bytecode. Windows also uses the
original call path because the fused build exceeded the 5% regression limit
in the native comparison cohort.

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

## Experimental private scalar activations

`ENABLE_BC_PRIVATE_SCALARS=ON` enables an unqualified #207 experiment; its
default is OFF on every platform. Verified functions may use private cells
when all ordinary scalar references belong to their parameter/auto mask,
there are no arrays or output, and every outgoing call is self-recursion.
Special registers remain VM state and retain the existing saved-specials
contract. The eligibility proof is cached with the prepared definition and
is replaced with that definition. Full checks, the optional operand-stack
engine and ineligible bodies use ordinary dynamic bindings.

Eligible activations leave global bindings untouched. Their scalar cells
start empty and read as exact zero; the first write constructs a Number.
Incoming arguments retain their existing evaluation order, pack ownership
and snapshot behavior. The existing fused scalar prologue writes the private
parameter cell when applicable. This experiment does not bypass argument
staging or borrow pointers into the growable argument vector.

A VM-owned arena supplies contiguous slices from stable 32-cell chunks.
Nested calls can grow the chunk list without moving active cells. Leases
clear their owned values and restore the cursor in LIFO order on both normal
return and exceptions. Chunks retain the largest simultaneous chunk demand
until VM destruction, including end-of-chunk padding; they do not retain a
maximum frame for each historical depth. Allocation counters include arena
chunks and their directory under `frames`, and proof metadata under
`execution_plans`. `private_scalar_calls` counts successful lease acquisitions
in separate profiling builds; it does not establish identical ordinary and
profiled machine paths or a speedup.

The proof deliberately excludes calls to unrelated functions because bc uses
dynamic bindings: a callee could otherwise observe a caller's unpublished
auto. Such bodies, free-variable accesses, arrays and output remain on the
generic path. The option adds an active-frame check to scalar access, so it
can slow unrelated programs even when they never use private storage.
Recursion still uses the host stack. This prototype has no performance
acceptance or new supported recursion-depth claim. Adoption requires complete
native correctness, elapsed, startup and resource evidence for both eligible
and fallback workloads.
