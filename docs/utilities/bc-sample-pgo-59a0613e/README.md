# Clang sample PGO with checked cold cohorts

[Hosted run 41828363664](https://github.com/matt-gretton-dann/gd-posix-apps/actions/runs/41828363664)
tested `59a0613e93cbf9eff7bb095b06deb234a3228c3f` on AMD EPYC 7763.
The artifact ZIP SHA-256 was
`8bad7f6089b336b91fe8f18a84431ff562a94940b4579c7eb0d74e9102aad2de`.
The archived holdout includes 66 workloads and 3,300 checked observations,
paired ratios against the portable Clang build and Howard, and section sizes.

Each cold cohort checked 2,048 children. Startup, error, and interactive
sessions mapped 94, 63, and 230 executable samples, respectively. The
earlier 256-child attempt mapped 22 error samples and stopped before
building a profile. Most samples in the larger cold traces were outside
the bc executable, including the training driver and libraries.

| Workload | VM-only / portable [95%] | Whole-program / portable [95%] |
| --- | ---: | ---: |
| Calls | 1.070 [1.055, 1.098] | 0.989 [0.971, 1.026] |
| Streaming statements | 1.174 [1.167, 1.225] | 1.095 [1.027, 1.109] |
| File statements | 1.192 [1.171, 1.211] | 1.091 [1.069, 1.124] |
| Multi-limb modulo | 1.007 [0.995, 1.014] | 1.548 [1.541, 1.564] |
| Startup | 0.956 [0.795, 1.041] | 0.962 [0.805, 0.999] |

The substantive rows shown had passing A/A controls. Startup was
inconclusive. VM-only text grew about 0.7% and whole-program text about
5.8%. Build wall times were 35.89 seconds for portable, 34.45 for A/A,
34.33 for VM-only, and 35.82 for whole-program PGO. The added cold
coverage did not remove the holdout regressions in either profile scope.
