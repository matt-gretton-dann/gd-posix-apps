# Portable array layout comparison

Issue #209 compares containers through the ordinary VM, rather than inferring
end-to-end speed from lookup complexity. The default remains `hybrid`: a bounded
dense prefix with an ordered sparse tail. Explicit `BC_ARRAY_STORAGE` values
`paged`, `flat`, `hash` and `ordered` select portable experimental alternatives.
None needs a CPU instruction extension or changes language-level index limits.

- `paged` uses ordered sparse pages of 64 optional values. An isolated write
  allocates one page, with no allocation up to the highest array index.
- `flat` keeps sorted key/value pairs in a vector. Lookup is binary search;
  insertion charges element movement and capacity growth.
- `hash` uses the standard unordered map with its ordinary growth policy.
- `ordered` is the standard ordered-map reference, with highest written index
  recorded independently of traversal.

All retain absent-read versus explicitly-written-zero distinctions, logical
entry count, highest-written length, value-copy semantics and moved-from empty
state. Insertion takes its value before any allocation or reorganization, so a
value borrowed from the same container remains valid. Shared Number coefficients
retain their existing copy-on-write semantics. Array allocation profiling uses
the same allocator category for each container, including hash buckets and full
pages; those bytes exclude allocator overhead and are not process RSS.

Build each candidate in its own directory with the same compiler, Release flags,
extension mode and multiplication threshold. For immutable measured recipes:

```sh
python3 src/bc/scripts/build-gd-bc.py --source /path/to/repository \
  --revision FULL_COMMIT_SHA --split-point 126 --array-storage paged \
  --output paged-release
```

The recipe records the layout and requires its nondefault compile definition in
`compile_commands.json`. An older revision silently ignoring a CMake variable
cannot publish a candidate config. Generators without compilation commands can
still build experiments directly, but cannot publish a verified nondefault
recipe through this builder. Legacy default-hybrid recipes remain supported.

The default unit suite compares every layout with an independent ordered-map
value oracle using 8/16/32-bit Number models, including sparse maxima, page
boundaries, random writes/reads, zero, replacement, copies, moves, heap values and
borrowed insertions. Builds selecting another layout must additionally pass the
ordinary VM/parser/CLI and diagnostic suites. The completed [matched comparison](bc-209-array-comparison/README.md) records
full/held-out elapsed pairs, separate allocation counters and native peak RSS.
It retains the hybrid default: the alternatives have workload-specific wins,
but sorted insertion, sparse-page memory and snapshot costs prevent a general
replacement. The portable alternatives remain explicit experiments.


`prepare-array-benchmark.py --output prepared --preset full` creates eight
exact-oracle workloads for sequential and permuted dense access, random sparse
keys and absent neighbors, sparse clusters, repeated high-index replacement,
absent reads, parameter snapshots and heap coefficients. The `holdout` preset
changes dimensions and sparse/permutation parameters; `quick` is correctness
only. Replay these frozen programs through `bench-bc.py --replay-corpus prepared`
with all candidate configs and the hybrid reference. This separate corpus does
not change any existing benchmark program or seeded operand.

The real-interpreter quick regression runs in both extension modes on native
64-bit hosts. These timing inputs deliberately use the native 10^9 index domain;
smaller pointer-width targets retain the generic storage oracles but skip this
CLI corpus, rather than silently truncating its high keys. No timing acceptance
is inferred from quick correctness. The archived full/held-out runs and separate resource diagnostics retain
their exact historical source identity. Their layout timings are from one
native Apple ARM64 host; cross-platform unit correctness is separate.
Competitive misses and inconclusive controls are recorded per class.
