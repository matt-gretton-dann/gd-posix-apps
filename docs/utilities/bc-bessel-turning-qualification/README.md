# Standalone Bessel qualification

This workflow measures the exact #726 candidate `4fbcf17d` against main
`19ef8016` on Ubuntu 24.04 x64, Windows 2025 x64 and macOS 15 arm64.
It uses the existing qualification collectors from `1f189965`. Git archives
supply their canonical bytes on every host. Each job records the three
immutable revisions, the actual collector file hashes and its command plan.

The arithmetic jobs cover the complete existing full and held-out corpora
separately with 151 samples per paired stage, the sparse checks and matched
resource diagnostics. The startup job builds two independent copies of main
and one candidate and runs the existing startup calibration. Its time limit is
2,400 seconds with a five-second child bound.

The math jobs divide each original 60-case corpus by entry scale into three
disjoint partitions of 20 cases. Both modes and all ten regimes appear at each
scale. Use 151 A/A and 31 paired samples. Calibration targets half a second
per role and the measured duration floor is a quarter second. Each job
retains the ordinary primary Howard comparison, the portable library control,
the matched baseline and separate resource diagnostics. The per-child bound
is 600 seconds and the math command budget is 14,400 seconds. The outer
collector budget is 18,000 seconds and the hosted job limit is 330 minutes.

The study is prospective and opens once on the qualification branch. It
retains the original #726 advisory run separately and uploads every available
outcome, including failed stages. There is no retry for acceptance. The
existing performance, accuracy, startup and resource criteria still apply.
This work concerns the standalone Bessel change. The broader #197 integration
and its original qualification records remain separate.

For a read-only review of the partition and command plan, invoke
`collect.py --suite math-full-20 --output DIRECTORY --plan-only` from the
repository root. Plan mode builds no executable and runs no timed child.
