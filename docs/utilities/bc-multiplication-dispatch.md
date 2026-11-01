# Coefficient multiplication dispatch

Number retains sign and decimal scale; coefficient kernels borrow their inputs
and write separate output. One-limb operands keep the multiply-accumulate paths.
Multi-limb shared coefficients can use triangular squaring when the widened type
can hold a doubled product. Other traits retain ordinary multiplication.

The schoolbook experiment keeps the shorter operand as the row count after
single-limb cases have returned. Swapping borrowed iterator pairs or spans
changes no ownership or value. Every limb product is still evaluated, but fewer
rows reduce the potential carry-draining tails and row setup work. Recursive
schoolbook leaves use the same ordering. This is not a blocked multiplication
algorithm and does not change the Karatsuba threshold.

Balanced large products recurse through planned workspace and direct limb-offset
recombination. Lopsided shapes retain a portable schoolbook fallback rather than
splitting off empty high halves. The independent decimal-convolution tests check
both operand orders, dense/sparse inputs, aliases, snapshots, scales and mixed
thresholds at all three provided limb widths.

Square/workspace evidence is linked from the benchmark documentation. Operand
ordering remains a separate #203 experiment until paired measurements establish
its effects. Blocked/asymmetric kernels, robust candidate sweeps and stable
portable default selection remain open work.
