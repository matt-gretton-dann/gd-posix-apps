# Bounded parser instruction reuse

Refs #210 and #197. Reuse the current instruction vector only when Parser is its
sole shared owner. A caller retaining an earlier result still receives an
independent snapshot. Clearing destroys every instruction and decoded literal
cache before the next input item; only vector capacity survives. VM function
registration copies its persistent body, and no outer instruction view survives
synchronous execute().

Capacity is bounded by the largest item parsed so far, not by the number of
input items. A large item followed by small ones can retain capacity until the
parser dies; report that cost in memory measurements. This stage does not batch
items, read ahead, delay output, change quit/error timing or extend signal
handler lifetimes. Scanner, buffering and interactive-policy work remain #210
stages.

Tests retain several returned results across more parsing, and independently
check exact output with bounded instruction allocations over repeated items.
PR #346 accepts this stage after matched all-class timing, separate peak/live
process memory and all fourteen native/CI/CodeQL checks. [The exact-source
report](bc-210-parser-reuse/README.md) retains costs and noisy intervals alongside
the bounded gains. Other #210 stages remain open.
