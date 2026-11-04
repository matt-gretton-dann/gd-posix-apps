# Competitive arithmetic checkpoint at 851e2755

This #197 and #200 checkpoint compares GD `851e2755` with pinned
source-built Howard 7.0.3 and Apple's distinct installed Howard 7.0.3
on one macOS arm64 host. The source-built comparator hash matches the
[earlier integrated matrix](../bc-197-integrated-matrix/README.md).
See [binary identities](binaries.json) for executable hashes and sizes.

GD used Apple Clang 21, Ninja Release, default language and VM options,
and multiplication split point 126. Run `src/bc/scripts/bench-bc.py`
with the three roles `gd`, `howard-source` and `howard-apple`. Use
`--reference howard-source --samples 9 --warmups 1 --seed 197851` for
each of the full and holdout arithmetic presets. Each preset contains
66 workloads and 1,980 checked observations. Output hashes matched
across roles within every workload.

The proposed target requires a GD/source-built Howard median of at most
0.90 and a nominal paired interval ending below 1.0. The full run met
that target on 46 of 66 workloads and holdout on 49 of 66. These are
counts of the measured cases without an aggregate weighting. Startup
is tracked separately. Some local compilation overlapped the full run,
so its timing is provisional. Neither run had an independent A/A
control or supplies cross-platform acceptance.

| Workload family | Full GD / Howard | Holdout GD / Howard |
| --- | ---: | ---: |
| Function definitions | 2.115 | 2.046 |
| Sparse integer square root | 1.732 | 1.873 |
| Recursion | 1.370 | 1.365 |
| Function calls | 1.131 | 1.140 |
| Temporary arithmetic chain | 1.064 | 1.156 |

The results retain misses alongside wins. The largest repeatable gaps
in this run are function definitions and the sparse integer root.
The held-out large division case also took 1.426 times the comparator.

[Per-workload ratios](ratios.csv) · [Full observations](full.jsonl.gz) ·
[Holdout observations](holdout.jsonl.gz)

## Focused follow-up at c31e75b4

A local macOS arm64 run remeasured ten full and ten holdout workloads to
locate the remaining gaps after the sparse-root and scalar VM changes.
The current Release executable was built from the runtime sources in
`c31e75b4`, using Apple Clang 21 and split point 126. The run interleaved
that executable, its identical A/A role, the earlier GD executable, and
the same two Howard executables. The source-built Howard executable was
the reference. The harness used seed 197851, one warm-up and twelve
measurements per role. Each preset checked 650 outputs, with matching
output hashes across all roles for each workload.

| Workload family | Full GD / Howard | Holdout GD / Howard |
| --- | ---: | ---: |
| Function definitions | 2.161 | 2.090 |
| Recursion | 1.359 | 1.359 |
| Function calls | 1.141 | 1.142 |
| Temporary arithmetic chain | 1.064 | 1.157 |
| Larger multi-limb division | 0.956 | 1.425 |
| Sparse integer square root | 1.028 | 1.013 |

The older GD executable took 1.789 and 1.893 times the source-built
Howard time for sparse integer square root in these same runs. The
targeted selection retains both improvements and misses. The A/A role
repeats the same executable, so this checkpoint measures timing
variability on one host. Cross-platform acceptance requires separate native runs.

[Binary identities](current-gap-binaries.json) ·
[Full samples](current-gap-full-samples.jsonl.gz) ·
[Full summary](current-gap-full-summary.json.gz) ·
[Full manifest](current-gap-full-manifest.json.gz) ·
[Holdout samples](current-gap-holdout-samples.jsonl.gz) ·
[Holdout summary](current-gap-holdout-summary.json.gz) ·
[Holdout manifest](current-gap-holdout-manifest.json.gz)
