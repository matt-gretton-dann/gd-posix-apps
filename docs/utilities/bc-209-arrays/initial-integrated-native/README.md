# Hybrid-array candidate — Linux regressions

Native run 41279178886 compares verified-executor parent 357d15a4abd3736b432953b9269ba1057e234f02
with hybrid candidate 91b7b481178bebb6d8a9c660649d59c71527743a. Every
output/status/deadline check passes: macOS/Windows 1450 invocations each,
Linux 1660 including bounded confirmations. The advisory green result does
not establish acceptable performance.

Linux confirms slower loop (1.141), calls (1.112), high comparison (1.109),
low comparison (1.097), base-changing literals (1.087) and sparse arrays
(1.082). Full intervals, confirmations, raw rows and digests remain in the
ZIPs. macOS/Windows have no confirmed regressions in this run. The local
adjacent matrix shows dense gains but does not supersede the Linux losses.

Keep adoption on hold. Isolating container-specific methods from GCC's scalar
dispatch is a bounded next candidate, not a proven inlining or hardware cause.
Other compilers must retain their generic policy. The separate paged/flat/hash
evaluation remains open, including the dense-prefix peak-memory tradeoff.
