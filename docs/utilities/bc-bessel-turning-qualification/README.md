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

The first run opened once on the qualification branch. Its math jobs stopped
before calibration when the archived controller's CLI test path was absent.
The recovery event is restricted to the first qualification head as its
previous revision and contains only the math partitions. The CLI bridge
runs the exact candidate checkout's tests and records its own hash alongside
the collector inputs. The original arithmetic and startup jobs retain their
evidence and timing schedules. All outcomes, including the initial setup
failures and the earlier #726 advisory results, remain available separately.
The existing performance, accuracy, startup and resource criteria still apply.
This work concerns the standalone Bessel change. The broader #197 integration
and its original qualification records remain separate.

For a read-only review of the partition and command plan, invoke
`collect.py --suite math-full-20 --output DIRECTORY --plan-only` from the
repository root. Plan mode builds no executable and runs no timed child.
