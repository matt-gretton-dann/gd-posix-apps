# Bounded deferred-carry multiplication experiment

The schoolbook candidate accumulates products into small stack tiles before
normalizing them into decimal limbs. It preserves the accepted operand order.
Rows are bounded by `min(16, WideMax / base / base)` and columns by 16. Actual
tile tails determine every loop bound; unused stack elements are never read.
Inputs are borrowed, output is disjoint and initially zero, and the caller owns
all allocation and decimal sign/scale handling.

For radix B and at most r rows, each coefficient accumulates at most
`r*(B-1)^2`. Incoming carry is at most `r*(B-1)` and an existing result limb is
at most B-1. Their sum is below `r*B^2`, which fits the widened type by the
compile-time row bound. The same carry bound holds after normalization. Carry
from overlapping tiles propagates through the caller's full result extent.

Traits that cannot safely accumulate two rows, and operands shorter than four
limbs, retain scalar multiplication. The ordinary one-limb multiply-accumulate
and shared-coefficient square dispatch remain ahead of this candidate. The
recursive workspace leaves use the same tile kernel without allocating.
`blocked_multiply_calls` is a subset of `basic_multiply_calls`, so the existing
logical product counts are unchanged.

Direct tests compare against independent decimal-character convolution at all
three supported limb widths. They cover maximum carry, sparse/zero inputs,
both rectangular shapes and every relevant tile boundary/tail, with untouched
output guards. A radix-200 uint8/uint16 trait checks the scalar fallback where
the widened type holds ordinary products but not two-row accumulation.

This is a #203 experiment pending correctness and paired performance evidence.
The stack bound and independent products do not establish vectorization or a
speedup. No ISA-specific code or hardware feature requirement is introduced.
