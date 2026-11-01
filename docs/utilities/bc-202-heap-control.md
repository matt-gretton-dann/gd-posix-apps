# Heap COW control

Refs #202 and #197. Experimental branch, not an adoption decision. Disable
inline coefficient capacity while retaining current vector/COW arithmetic and
verified execution. Empty/absent values remain allocation-free, with one unused
local limb providing a valid empty pointer range. Every nonempty coefficient
uses the heap; shrinking retains its existing location. No arithmetic or VM
algorithm is changed.

This is a contemporary zero-inline control, not the historical baseline binary.
Storage-layout and small-allocation assertions target the production policy;
independent numeric/VM oracles and process RSS/timings compare this policy.
