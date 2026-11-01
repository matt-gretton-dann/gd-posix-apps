# Array storage and sparse semantics

Element reads and length calculations borrow the VM-owned shared array for the
lookup. Whole-array loads still copy ownership so arguments and saved locals
retain their value snapshots. Writes retain the existing whole-array COW rule.

The candidate container combines a dense prefix of optional numbers with an
ordered sparse map. An absent optional differs from an explicitly assigned zero:
reads return zero without inserting an entry, while assigning zero extends the
logical length. Length tracks the highest assigned index independently of the
container's ordering. Array-index validation remains the VM's numeric contract.

The prefix grows only for indices below 64 or within four times its populated
entry count, with a hard storage-policy cap of 1,048,576 slots. This is not a
language array-size limit: remaining indices use the sparse map, including very
high isolated indices. Previously sparse values covered by a growing prefix
migrate once without changing the logical entry count. Taking the assigned value
before growth protects a source borrowed from that same array.

Both containers and the presence metadata are copied when an array snapshot is
mutated. Dense capacity and initially empty slots can increase small-array
payload; the constant growth window and density/cap limits bound that tradeoff.
Diagnostic payload, allocation and timing evidence must include dense and sparse
cases rather than treating logical entry counts as process RSS.

Independent ordered-map oracles cover sequential and random writes, overwrites,
assigned zero, sparse-to-dense migration, holes, high indices, lengths, copied
snapshots, moves and assignment from a borrowed element during vector growth.
A separate profile check proves absent high reads allocate nothing and a single
high write allocates bounded fallback storage. Existing VM/CLI cases cover array
arguments, local restoration, invalid indices and recursive snapshots.

This is a #209 candidate pending sanitizer, profiling and paired measurements.
Paged, flat/sorted and hash alternatives remain separate evaluation work.
