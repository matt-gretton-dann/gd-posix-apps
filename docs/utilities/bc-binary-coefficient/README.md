# Persistent binary coefficient probe

This #204 comparison keeps coefficients in GMP's binary representation
through each operation or recurrence. The separate scale field preserves
decimal multiplication scale, with signed truncation toward zero.
The experiment differs from #461, which converts decimal limbs to GMP
and back around each large product.

The probe and the GD decimal driver were built on macOS arm64 with Apple
Clang 21 and GMP 6.3.0. The GD driver uses the default Number traits
and a multiplication split point of 126. The linked libraries came
from the GMP-disabled scratch source integration of #461 and main
`8189f874`.

Copy the two driver sources and the Python runner to a scratch directory
with the `.txt` suffixes removed. Build `probe.cc` as `probe` with
`-std=c++20 -O3 -DNDEBUG`, the GMP include and library directories,
and `-lgmpxx -lgmp`. Build GD's `bc` target with Ninja in Release
mode and GMP disabled. Compile `decimal.cc` as `decimal` with the same
common flags, `-flto=thin -DENABLE_EXTENSIONS=1`, the source directories
`src/bc`, `src/include`, `libgdsup/include`, and the generated
`src/bc` include directory in the build. Link the build's
`src/bc/liblibbc.a`, `src/libutil.a` and `libgdsup/libgdsup.a`.
Run `python3 measure.py` beside the two binaries.

The runner checked 180 process invocations against independent Python
integer results. It normalized the GD driver's output line continuations
before comparing exact bytes. Each case had one warmup and nine measured
rounds, interleaved in fixed-seed order.

| Workload | Median binary / decimal elapsed time |
| --- | ---: |
| 100 × 100 digits | 1.019 |
| 1,000 × 1,000 digits, scaled | 1.011 |
| 10,000 × 10,000 digits | 0.997 |
| 100,000 × 100,000 digits | 0.884 |
| 100,000 × 1,000 digits, scaled | 1.375 |
| Factorial 1,000 | 0.214 |
| Factorial 5,000 | 0.120 |
| Fibonacci 1,000 | 0.466 |
| Fibonacci 10,000 | 0.097 |

The recurrence results show the potential of retaining a binary
coefficient across operations. They include differences in storage,
arithmetic and decimal output. The GMP driver does not emit bc line
continuations, and neither driver executes the bc parser or VM. This
experiment has no independent A/A control, memory measurements or
cross-platform data. The scaled unequal product is an adverse case
for a general-purpose replacement.

[Binary driver](probe.cc.txt) · [Decimal driver](decimal.cc.txt) ·
[Runner](measure.py.txt) · [Raw observations](timing.json)
