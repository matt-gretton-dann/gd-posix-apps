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

The representation uses an always-live inline array and `std::shared_ptr` owner,
with `std::vector` as the arbitrary-size fallback. A non-null owner selects the
heap path directly. There is no union, pointer tagging, shared_ptr layout or ISA
assumption. Only the inline prefix described by its size is initialized and
copied; growth initializes every newly exposed element. An absent size sentinel
exposes an empty range. Unused inline slots are never copied or inspected.
Reset releases the owner and changes the size, without clearing unused limbs.
This costs more object space than the variant prototype; runtime and memory
must both justify adoption. There is no cross-frame lifetime coupling.

Tests cover inline/heap promotion, independent inline copies, heap COW snapshots,
shrinking without relocation, shared-small demotion, reset, moves and bounds at
all three generic limb widths. Existing arithmetic/alias/snapshot/recursive VM
oracles continue to exercise the public Number interface. An isolated global
ordinary-allocation counter confirms zero allocations for small-number native
construction, copy, add and subtract, without relying solely on a storage tag.
Diagnostic fixtures deliberately exceed inline capacity when asserting COW and
heap allocation events.

The current tree passes 149 Release bc cases (581,015 assertions), 109
extensions-disabled ASan/UBSan number/VM cases (396,806 assertions), eight
CLI/helper groups and five diagnostic checks. Growth after copy, shrink and
reset explicitly checks newly exposed inline elements. Large Number/result
objects can cost memory even when allocation counts fall; matched full/held-out
runtime, selected bytes, RSS and object/code sizes remain adoption criteria.

VM parameter/terminal-return consumption was merged separately. Numeric code
also consumes divisor coefficients and Newton guesses only at their proven
lexical last use. General arithmetic liveness, result/capacity reuse and
ownership-policy comparisons remain #202 work. Per-operation multiplication
scratch is evaluated independently under #203.

An explicit-union alternative was rejected after an uninstrumented Release
output failure under Thin LTO/-O3, despite passing Debug and diagnostic builds.
The cause is unresolved; neither that representation nor a global compiler
flag workaround is adopted. Release correctness checks must precede timings.
Native paired runs also found square-root and sparse-product regressions in
the original three-alternative prototype, so PR #314 remains an experiment.

The storage/workspace combination passes 156 Release bc cases (582,397
assertions), 116 extensions-disabled ASan/UBSan number/VM cases (398,188
assertions), five diagnostic checks and all eight CLI/helper groups. Its
performance comparison must use the workspace dependency as the exact baseline;
earlier variant/legacy-multiplication data remain separately labeled.

The local comparison baseline is workspace source
`5e23d646393051f3e22a957495fab69473a01f64` (PR #316), rebuilt with the same
compiler, flags and fixed split point. Its accepted main merge is `b8c2e79`;
production C++/CMake is identical. PR #314 now targets main, retaining the
original source labels rather than relabeling already collected experiments.
