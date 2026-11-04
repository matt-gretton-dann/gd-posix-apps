# Exact transform experiment

Refs #204. Enable this experiment with
`-DCMAKE_CXX_FLAGS=-DGD_BC_EXPERIMENT_NTT=1` in a fresh CMake build.
Use Release and `-DBC_MULTIPLY_SPLIT_POINT=126` for both the candidate
and the reference. The option is off in normal builds.

Each base-10^9 limb supplies three base-1000 coefficients. Pad to a
power of two at least as large as the output chunk count, preventing
wraparound in the cyclic convolution. Moduli 998244353 and 1004535809
are prime and share primitive root 3. Trial division proves primality.
Raising 3 to (p-1)/q for each prime factor q of p-1 gives a residue
other than 1. The factors are {2, 7, 17} and {2, 479} respectively.
Both moduli support all transform lengths through 2^21.

For the largest allowed transform, a coefficient is bounded by
2^20 * 999^2. The product of the primes exceeds this bound, so the
least nonnegative CRT reconstruction is exact. Modular multiplication
and reconstruction fit in 64 bits. The carry pass restores base-10^9
limbs and the surrounding number code applies the usual sign and
scale. Two temporary vectors consume at most 16 MiB of limb storage.

The initial broad candidate selected balanced products with at least
2048 limbs per operand. At 200,003 decimal digits, a dense product took
1.474 times baseline, a sparse product took 6.763 times baseline, and a
dense square took 0.587 times baseline. Those results motivate keeping
the existing kernel for general products. The revised candidate applies
the transform only to aliased squares of at least 11112 limbs, with at
least half of the limbs nonzero. Uniform runs and all other traits
continue to use the existing kernels.

The measurements include process launch, decimal parsing and output.
The reference is main at `623441fe6d111cdd1f9b2ec8a569362539a7c559`.
Both GD builds use Apple Clang 21.0.0 on macOS arm64. The third binary
is the source-built Howard 7.0.3 from
`c8dabac65d39fd527a9264a5e505a99b1d8df3d8`. Each case uses one warmup
and seven interleaved measured launches per binary. Python integer
multiplication supplies the independent output oracle. This local
experiment uses no separate A/A control and has no native Linux or
Windows timing evidence.

The narrowed revision passed 34 CLI tests and 50 independent exact
product and alias cases through 200,003 digits. It retained the square
gains in the same timing grid. The 100,000- and 200,003-digit dense
squares took 0.798 and 0.581 times baseline, respectively. The broad
and narrowed grids checked 600 outputs each. See the raw paired
samples in [broad.json](broad.json) and [square.json](square.json).
The shared grid informed dispatch selection, so independent timing
and peak process RSS are still needed before enabling the tier.

An independent held-out grid uses five sizes from 99,990 to 300,011
digits and a new operand seed. It checked 360 output samples and 45
separate output-checked RSS launches. The [runner](holdout.py) takes
the base and candidate executables and a JSON output path. Pass
`--howard /path/to/bc --rss` to reproduce the macOS measurements.
The RSS option runs macOS `/usr/bin/time -l` in separate launches.
The [raw results](holdout.json) retain all timings and peak RSS.

| Dense square digits | Candidate / baseline time | Baseline peak RSS bytes | Candidate peak RSS bytes |
| ---: | ---: | ---: | ---: |
| 99,990 | 1.004 | 2506752 | 2473984 |
| 100,008 | 0.797 | 2506752 | 3276800 |
| 145,003 | 0.443 | 2686976 | 3325952 |
| 210,007 | 0.534 | 2932736 | 4472832 |
| 300,011 | 0.359 | 3375104 | 4603904 |

The transform traded more peak memory for lower elapsed time. The
210,007-digit square had the largest observed memory increase, about
53%. Held-out general products and sparse squares stayed within
about 3% of baseline median time. Linux and Windows measurements
with the transform enabled remain pending.

The enabled Debug build with AddressSanitizer and UndefinedBehaviorSanitizer
passed the 34 CLI and 50 large integer oracle cases. Release and
sanitized builds also passed three signed decimal squares with
scale-three truncation, including inputs on both sides of the
dispatch threshold.

The dedicated pull-request workflow builds the exact base and candidate
revisions on Linux and Windows. It enables the transform only in the
candidate, checks the CLI, and replays the holdout with a byte-identical
second baseline executable. The artifact records the CMake caches,
binary identities and checked samples. Native timing is advisory and
the workflow leaves ordinary builds on the existing kernel.
