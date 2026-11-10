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

Large recursive products can use a direct sparse path when both
coefficients contain at most four nonzero limbs. The probe requires at
least 256 limbs per operand and a zero second limb, then stops at the
fifth nonzero limb. The direct path accumulates each nonzero pair into
the ordinary coefficient buffer with radix carries. Other operands
continue through the uniform-run or recursive kernels.

The GCC Linux x86-64 experiment emits the default 32-bit-limb sparse kernel
in a separate native object, with 64-byte function and loop alignment. The
object disables LTO and the function uses GCC's `noipa` attribute so unrelated
VM changes cannot clone its body or change its internal loop padding. A
configure-time attribute check selects this arrangement. Other platforms,
compilers and limb traits continue to instantiate the same generic header
algorithm. This code-placement experiment addresses repeated sparse-workload
losses after VM changes whose normalized sparse-kernel instructions were
unchanged. Alignment is a hypothesis until native full and holdout comparisons
confirm the effect; local Clang checks establish compilation and exact products
only.
