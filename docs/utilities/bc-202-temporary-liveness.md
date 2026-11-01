# Conservative arithmetic temporary consumption

Refs #202, #207 and #197. This candidate transfers a first arithmetic operand
when its result slot cannot be read again. It does not consume variables,
arrays or arbitrary aliases, replace COW storage, or claim a measured speedup.

Complete entry checks still run first: operand schemas/ranges, producer kinds,
function boundaries, dominance and parameter-stack invariants. The plan counts
all data uses, including two operands of the same instruction; branch and
function-end offsets are control metadata rather than data reads.

Every branch target and position after a control transfer begins a block;
function boundaries are separated too. A transfer needs exactly one static
use, a producer strictly earlier than the consumer, and the same straight-line
block. Every entry into that consumer's block therefore executes the producer
first. A backedge into the middle would create a new block, preventing transfer.
A backedge to the producer permits a fresh value on each iteration. A producer
outside the loop is never consumed merely because it has one static use.

The first operand becomes the working Number; the second remains borrowed.
Both-operand aliases have two counted uses and keep the copy path. Other COW
owners, including variables, snapshots and immutable literal caches, remain
intact. A consumed slot is not read again before its producer overwrites it.
Runtime semantic errors and interruption polls remain active. Full checked
execution prepares no consumption plan and retains copies for differential
execution and diagnostics.

Outer execution owns its prepared metadata beside the borrowed instruction
stream. Persistent function code owns both instructions and metadata. Views
are formed on demand, so copying/moving owners cannot retain a pointer into
a former owner's metadata. Function redefinition recomputes the plan; nested
function definitions are already rejected by the entry verifier. Instruction structure and
metadata must remain unchanged and alive throughout their borrowed execution;
ordinary decoded literal caches can still populate at runtime.

Common outer streams shorter than sixteen instructions skip planning. Function
plans persist across calls; streams without arithmetic or proven transfers use
an empty plan. A nonempty plan has one tracked byte per instruction, plus its
ordinary owner header. Construction uses linear temporary storage, which is
not included in the selected allocator payload counts. The `execution_plans`
category and `temporary_consumptions` event expose their scoped costs; process
memory and code size require separate diagnostics.

The failing profile regression precedes implementation. Direct checked/verified
fixtures protect a repeated loop consumer and duplicate operands. Plan tests
cover empty/tiny streams, conditional entries, later uses and both safe/unsafe
backedges. Release passes 211 cases/691931 assertions and nine CLI groups. Extensions-OFF
ASan/UBSan passes 28 VM/plan cases/54298 assertions, fifteen VM CLI and six
offset groups. Thirteen profiling CLI groups pass; nine focused profiling unit
cases (39 assertions) show the protected literal still clones in both modes,
while the unique intermediate avoids one clone only in verified execution.
Matched timing/memory and native acceptance remain required before adoption.
