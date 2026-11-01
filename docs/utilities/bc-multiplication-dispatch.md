# Coefficient multiplication dispatch

Number retains sign and decimal scale; coefficient kernels borrow their inputs
and write separate output. One-limb operands keep the multiply-accumulate paths.
Multi-limb shared coefficients can use triangular squaring when the widened type
can hold a doubled product. Other traits retain ordinary multiplication.

The shorter-row ordering experiment is rejected. It reduced potential row
setup/carry tails but slowed the intended full/held-out shape by about 11%, with
confirmed Linux and Windows regressions. The accepted left-row scalar order
is restored. [All raw evidence](bc-203-asymmetric-order/README.md), including
losing classes and native confirmations, remains archived. Reversed-operand
oracle coverage is retained independently of the rejected optimization.

Balanced large products recurse through planned workspace and direct limb-offset
recombination. Lopsided shapes retain a portable schoolbook fallback rather than
splitting off empty high halves. The independent decimal-convolution tests check
both operand orders, dense/sparse inputs, aliases, snapshots, scales and mixed
thresholds at all three provided limb widths.

Square/workspace evidence is linked from the benchmark documentation. The
rejected operand-order experiment supplies evidence, not a selected runtime
path. Blocked/asymmetric kernels, robust candidate sweeps and stable
portable default selection remain open work.
