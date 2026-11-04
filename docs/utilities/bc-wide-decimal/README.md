# 18-digit decimal limbs in the full CLI

This #204 experiment compared a 64-bit limb with radix 10^18 and unsigned
128-bit intermediates against the default 32-bit limb with radix 10^9.
It exercised the full CLI, including the VM, parsing and output. The
[prototype patch](prototype.diff) applies to `bcf74931`. It is retained
as experiment evidence. The production limb types and dispatch are unchanged.

## Reproduction

Use `git apply --unidiff-zero prototype.diff` in a scratch checkout of
that revision. The patch widens the
argument to the power-of-ten helper before evaluating the radix. Widening
only the result computed an overflowing 32-bit radix and caused the
existing CLI suite to catch incorrect square roots.

Build the `bc` target in separate Ninja Release directories with
`BUILD_TESTING=OFF` and extensions enabled. Set
`BC_MULTIPLY_SPLIT_POINT=126` for the baseline and 63 for the wider build
to keep the nominal decimal crossover equal. Set
`CMAKE_CXX_FLAGS=-DGD_BC_PROBE_WIDE_DECIMAL=1` only in the wider build.
The wider instantiation requires compiler support for unsigned 128-bit
integers. This prototype also changes which trait-specific paths are
available, so it measures the whole candidate configuration.

On macOS arm64, Apple Clang 21.0.0 (clang-2100.3.34.2) produced a
568,080-byte baseline and a 584,672-byte wider executable. The corrected
wider build passed 36 CLI and 18 math checks with one optional math
check skipped.

Use `src/bc/scripts/bench-bc.py` with separate roles named `base`, `wide`
and `apple-howard` (Apple's installed `/usr/bin/bc` 7.0.3). For each of
the full and holdout arithmetic presets, use `--reference base --samples 7
--warmups 1 --seed 20418`. Each preset checked 1,584 outputs across its
66 workloads. Output hashes matched across roles within each workload.

## Timing

Ratios are elapsed time for wide divided by baseline. Brackets give the
harness's nominal 95% intervals. This is one local host without an
independent A/A build. It does not establish a Linux or Windows result.

| Workload family | Full | Holdout |
| --- | ---: | ---: |
| Sparse multiplication | 0.492 [0.488, 0.493] | 0.500 [0.499, 0.505] |
| Large multi-limb division | 0.714 [0.707, 0.725] | 0.716 [0.713, 0.741] |
| Large factorial | 2.140 [2.132, 2.144] | 2.165 [2.151, 2.172] |
| Temporary arithmetic chain | 1.839 [1.832, 1.850] | 1.747 [1.730, 1.818] |
| Startup | 0.985 [0.968, 1.171] | 1.006 [0.958, 1.026] |

The lower interval bound exceeded 1.05 for 21 full and 23 holdout
workloads. The division and sparse multiplication wins coexist with
substantial slowdowns, so this experiment does not support switching the
CLI's default representation. It did not measure peak scratch memory
or isolate the cost of 128-bit division from other trait-specific effects.

[Full samples](full.jsonl.gz) · [Full summary](full-summary.json) ·
[Holdout samples](holdout.jsonl.gz) · [Holdout summary](holdout-summary.json)

## Follow-up peak RSS

The same binaries were replayed under macOS `/usr/bin/time -l`. Use
the [GMP RSS runner](../bc-gmp-multiply/measure-rss.py.txt) with its
`gmp` role replaced by `wide` and the executable paths set to the
two builds above. Each role had one warmup and three measured
launches per shape. All 40 product outputs matched independent integer
oracles, including the negative scaled product. Peak RSS is process
memory, so this does not isolate scratch buffers.

| Operand digits | Median wide / baseline peak RSS |
| --- | ---: |
| 70,000 × 70,000 | 0.988 |
| 75,000 × 75,000, signed and scaled | 1.000 |
| 100,000 × 100,000 | 1.000 |
| 100,000 × 1,000 | 0.994 |
| 150,000 × 150,000 | 0.994 |

This memory check leaves the timing decision unchanged. The independent
[Toom evidence](../bc-native-controls-and-kernels/README.md) retained
adverse cases. The [exact transform](../bc-exact-transform/README.md)
is available for opt-in dense squares with portable fallback.
The [GMP bridge](../bc-gmp-multiply/README.md) traded speed for memory,
and the [persistent binary probe](../bc-binary-coefficient/README.md)
showed recurrence gains alongside an unequal scaled-product loss.
The [NEON tile](../bc-neon-schoolbook/README.md) did not improve its
tested component shapes.

[Raw RSS observations](rss.json)
