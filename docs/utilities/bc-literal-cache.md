# Runtime numeric literal decoding

Numeric text belongs to executable instructions. Its value depends on the input
base when that instruction executes, including inside a previously defined
function. Parsing a function under one base does not fix its constants to that
base. Decimal scale and output base do not replace the input-base cache key.

The bounded cache keeps one decoded value and its last input base per
executed multi-character literal. A hit copies the Number snapshot; arithmetic
must preserve the stored value through inline value semantics or heap COW.
Alternating bases replace the entry rather than retaining unbounded history.
Function instruction storage owns the cache, so new activation packs do not
invalidate it. Replacing operand text must invalidate its cache, and redefining
a function releases the old instruction storage normally.

Single integer digits have special language semantics: their values 0 through
15 are independent of ibase. They can construct a small integer directly;
fractions such as `.1` and multi-character literals such as `10` still require
execution-time conversion. Arbitrary expressions are not folded across base or
scale changes.

Each Instruction adds one unique owning pointer, including nonnumeric code.
Only executed multi-character literals allocate a memo; a base miss replaces
its Number in place. The pointer and payload costs are measured separately.
Copying instructions copies source operands and starts a fresh empty memo;
activation packs borrow their function code and do not copy or clear it.
Independent code copies therefore do not share mutable caches. Reusing the same
instruction vector concurrently requires external serialization.

Profiling counts hits, misses and single-digit fast paths. The `literals`
allocation category measures memo payload bytes and lifetime, with allocator
metadata excluded; Number heap payloads remain in `digits`. These are scoped
requested bytes, not process RSS. All caches are released with their instruction
storage when code is replaced or the VM is destroyed.

Baseline semantic tests precede implementation: fifteen CLI cases plus direct
re-execution after operand replacement. Exact performance and memory results are
pending; #205 remains open until the paired evaluation is complete.
