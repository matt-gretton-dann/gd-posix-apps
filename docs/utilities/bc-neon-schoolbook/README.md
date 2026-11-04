# ARM NEON schoolbook multiplication probe

The deferred-carry schoolbook tile in `ee72a0ab` accumulates independent
products within each row. The NEON probe replaces that inner accumulation
with two 32-bit products widened into 64-bit accumulators using
`vmlal_u32`. Scalar code handles the final odd column. Carry
normalization and the original tile bounds are shared.

Apple Clang 21.0.0 on macOS arm64 compiled three separate programs with
`-std=c++20 -O3 -DNDEBUG` and the project's `src/bc` include directory.
Copy [probe.cc.txt](probe.cc.txt) and
[neon-schoolbook.hh.txt](neon-schoolbook.hh.txt) to a scratch directory
with their `.txt` suffixes removed. Build the portable program with the
common options, the scalar control with
`-fno-vectorize -fno-slp-vectorize`, and the NEON variant with
`-DPROBE_NEON`. Disassembly of the NEON build contains `umlal.2d`.
Both portable roles use the unmodified production header.

Pass the two limb counts, repetition count and seed 20418 as arguments.
Use three repetitions to compare full limb arrays against an independent
row-wise scalar reference. That check passed in all three builds for
each of the six shapes below. For timing, one warmup and nine measured
rounds interleaved the three roles in a fixed-seed randomized order.
All timed process invocations returned matching digests within each
round. The timed repetitions omit the reference computation.

| Limb counts | Iterations | NEON / portable | Scalar control / portable |
| --- | ---: | ---: | ---: |
| 4 × 4 | 500000 | 1.134 | 1.071 |
| 16 × 16 | 250000 | 1.115 | 2.037 |
| 31 × 29 | 100000 | 1.017 | 1.438 |
| 63 × 63 | 25000 | 1.067 | 1.200 |
| 126 × 126 | 8000 | 1.093 | 1.193 |
| 512 × 21 | 8000 | 1.028 | 1.705 |

The ratios are paired medians. The scalar flags apply to the whole
probe translation unit, so the scalar-control differences cannot all
be attributed to the tile. This component probe excludes the VM, radix
conversion, end-to-end effects and other architectures. The explicit
NEON variant did not beat the portable build on these shapes, so it
remains an experiment.

[Raw timing and checked digests](timing.json)
