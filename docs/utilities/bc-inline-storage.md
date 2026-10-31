# Inline coefficient storage experiment (#202)

Small coefficients now retain up to three limbs inside their value. Copies of
those limbs are independent, with no vector/control-block allocation. Larger
coefficients use the portable allocator-aware vector and shared ownership;
mutation detaches shared storage at BasicDigits' existing COW boundary. This
retains cheap sharing for large immutable literals and snapshots. Scale and sign
remain explicit and independent of storage choice.

`DigitStorage` provides the contiguous operations needed by BasicDigits, not a
complete replacement for `std::vector`. Its nullable facade preserves absent
versus owned-empty state without allocating an empty owner. Heap shrinking
retains capacity and iterator location; implicit demotion would violate vector
iterator expectations. Reset/assignment can demote, and detaching a shared
small heap value copies into inline storage. Growth invalidates iterators as a
vector growth does. A move leaves the source absent. Raw iterator users must
retain their owning value and obey those invalidation rules.

The representation uses ordinary `std::variant`, `std::array`, `std::shared_ptr`
and `std::vector`; it assumes no pointer tagging, shared_ptr layout, ISA or
architecture. The heap path remains available for arbitrary supported sizes.
There is no manual arena or cross-frame lifetime coupling. Storage growth is
exact; arithmetic, public scale/sign rules and zero padding are unchanged.

Tests cover inline/heap promotion, independent inline copies, heap COW snapshots,
shrinking without relocation, shared-small demotion, reset, moves and bounds at
all three generic limb widths. Existing arithmetic/alias/snapshot/recursive VM
oracles continue to exercise the public Number interface. An isolated global
ordinary-allocation counter confirms zero allocations for small-number native
construction, copy, add and subtract, without relying solely on a storage tag.
Diagnostic fixtures deliberately exceed inline capacity when asserting COW and
heap allocation events.

The initial Debug tree passes 134 bc cases (580,424 assertions), eight selected
CLI/helper groups and four diagnostic checks. The extensions-disabled ASan/UBSan
run passes 94 number/VM cases (396,215 assertions). Larger Number/result objects
may cost memory and copying even when allocations fall. Matched full/held-out
runtime, selected allocation bytes, RSS and object/code sizes must be measured
before adopting this experiment; allocation counts alone do not settle it.

This is the first storage stage. Last-use temporary consumption, capacity reuse,
unique/deep-copy comparisons and any arena evaluation remain separate #202 work.
