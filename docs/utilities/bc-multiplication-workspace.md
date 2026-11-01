# Recursive multiplication workspace experiment (#203)

Coefficient multiplication retains the portable limb radix and widened arithmetic
used by BasicDigits. Public Number scale/sign rules and COW snapshot boundaries
remain separate. One-limb multiply-accumulate and products at/below the configured
split point retain the existing schoolbook path.

Above that point, a contiguous result and one scratch vector replace separately
owned recursive low/high products, sums and middle products. Each recursive
call borrows spans whose owner outlives the entire multiplication. No recursive
call allocates, resizes a vector, stores a view or returns an owning temporary.
This is a per-operation workspace, not an arena holding escaping numeric values
or a process/thread cache retaining the largest input indefinitely.

At each Karatsuba node, the low and high products occupy disjoint result ranges.
Their calls sequentially reuse the whole scratch buffer. Two half sums and their
product use its prefix; middle recursion uses its suffix. Subtract the low/high
products from the middle and accumulate it directly at a limb offset. Generic
decimal rescaling is not part of recombination. The result starts zero-filled;
only reused middle-product scratch is cleared again.

For a maximum input extent n, each child has at most k=ceil(n/2)+1 limbs. The
current node therefore reserves at most 4k scratch limbs for its sums and their
product, plus the bound for k. For n>=4, k<n. A three-limb leaf prevents
nonshrinking carry sums with artificially tiny split-point overrides. Overflow
in the workspace/result size calculation raises length_error before allocation.
The bound also covers leading-zero trimming changing an internal operand shape.
Asymmetric children with an empty high half use the generic schoolbook fallback;
blocked asymmetric dispatch is a separate experiment, not a claimed improvement.

Independent decimal-character convolution tests check all three supported limb
widths, carries, odd lengths, sparse halves, 2xN/10xN shapes, threshold boundaries,
COW snapshots and aliased squares. Direct kernel tests use exactly the planned
scratch extent, guard values on both sides of output and scratch, and nonzero
scratch contents. Instrumented and ASan/UBSan validation precede acceptance.

This remains an experiment until matched full/held-out runtime and allocation
measurements establish its cost. A dedicated square kernel, dispatch sweeps,
asymmetric blocking and portable default/tuner changes remain separate #203
stages. No new ISA or platform-specific requirement is introduced.
