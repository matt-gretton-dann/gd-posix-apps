# GMP coefficient multiplication retest

PR #461 converted large balanced decimal operands to GMP binary
coefficients for multiplication, then converted the product back to
decimal limbs. This #204 experiment retains the [prototype](prototype.diff)
without adding a production dependency.

Apply the patch with `git apply --unidiff-zero` to a scratch checkout
of `8189f874`. The patch is the source integration of #461 head
`75b2aa4`; the earlier PR workflow is outside the prototype. Build
`bc` with Ninja, `CMAKE_BUILD_TYPE=Release`, and `BUILD_TESTING=OFF`.
Use build directories named `base` and `gmp` with
`ENABLE_BC_GMP_MULTIPLY=OFF` and `ON`, respectively. Point
`CMAKE_PREFIX_PATH` at the GMP installation. Put the two archived Python
scripts in the scratch root with their `.txt` suffixes removed, then run
them there. Peak RSS uses the macOS `/usr/bin/time -l` utility.

Apple Clang 21 on arm64 macOS built both variants with the same source
and settings except for the option. The 568,080-byte baseline had no GMP
linkage. The 585,840-byte candidate linked GMP 6.3.0. Both passed all
38 CLI tests. The scripts independently checked signed and scaled
decimal outputs against Python integer products. There were 80 checked
timing observations and 40 checked RSS observations, including warmups.

| Operand digits | GMP / baseline elapsed time | GMP / baseline peak RSS |
| --- | ---: | ---: |
| 70,000 × 70,000 | 1.045 | 1.049 |
| 75,000 × 75,000, signed and scaled | 0.979 | 1.456 |
| 100,000 × 100,000 | 0.887 | 1.500 |
| 100,000 × 1,000 | 1.114 | 1.049 |
| 150,000 × 150,000 | 0.811 | 1.633 |

These are local paired medians after one warmup per workload. Time
includes parsing and output. RSS is process peak and does not isolate
conversion buffers. There was no independent A/A control. The unequal
case uses the generic multiplication path. Its observed difference
cannot establish a cost inside the GMP kernel. The two large balanced
wins came with substantially higher peak RSS, so this result does not
support adopting the experimental kernel.

[Timing script](time-products.py.txt) · [Timing samples](timing.json) ·
[RSS script](measure-rss.py.txt) · [RSS samples](rss.json)
