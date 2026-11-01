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
its Number in place. On the measured 64-bit Apple host, Instruction grows from 104 to 112 bytes
and Number remains 40 bytes. Each allocated memo requests 48 payload bytes.
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
re-execution after operand replacement. All 74 full-corpus diagnostic runs pass their exact output checks and balance
selected allocations at EOF. Loop/call literal conversions fall from
2,000,003/600,003 to one each. Base-flip conversions fall from 120,003 to 40,001,
with two memo allocations reused across all changes. Selected loop/call peaks
increase from 5,040/7,288 to 5,424/7,704 bytes; the repeated 2,000-digit literal
peak falls from 9,596 to 8,536. These costs are explicit rather than a blanket
memory-reduction claim.

The [paired evaluation](bc-205-literal-cache/README.md) retains all full and
held-out results, native artifacts and inconclusive division repeats. Isolated
measurements retain their original source identities. Combined square/cache
validation passes 165 Release bc cases, eight CLI groups, 18 sanitizer
VM/instruction cases, 15 sanitizer CLI cases and eight profile checks. The
40-case corpus preserves both earlier generators' programs, parameters and
oracles in all three presets. Native integration and merge validation are
recorded in [PR #319](https://github.com/matt-gretton-dann/gd-posix-apps/pull/319).
