# VM-only Clang sample PGO on Linux

[Hosted run 41821742672](https://github.com/matt-gretton-dann/gd-posix-apps/actions/runs/41821742672)
tested exact revision `74f513fdb1e31be37d97bc931064cf573603fb47` on
Intel Xeon 6975P-C. This archive retains the 66-workload holdout manifest,
3,300 checked observations, paired summaries and section measurements.
The original ZIP also includes training profiles, compiler commands and
build logs. Its SHA-256 was
`18c7d7c0c1a2c23b2b07e79c5d5e40bb06129489b9c32e7221ee909bfa18a16a`.

The two profiled builds used the same training profile. Compile commands
confirmed that the VM-only role applied it exclusively to `vm.cc`.
The VM-only build took 20.30 seconds, versus 20.13 and 19.99 for the
portable controls and 21.58 for whole-program PGO. VM-only text grew 2.4%
relative to the portable build. The two portable
builds had identical text hashes. The CLI training pass yielded only two
mapped executable samples, leaving cold and error coverage weak.

| Held-out workload | VM-only / portable [95%] | Whole-program / portable |
| --- | ---: | ---: |
| Loop | 1.102 [1.091, 1.114] | 0.900 |
| Calls | 1.027 [1.023, 1.035] | 0.853 |
| Sparse array | 1.091 [1.078, 1.105] | 0.921 |
| Streaming statements | 1.119 [1.114, 1.152] | 0.957 |
| File statements | 1.144 [1.126, 1.148] | 0.969 |
| Multi-limb modulo | 0.997 [0.989, 1.003] | 1.702 |
| Startup | 1.002 [0.924, 1.057] | 0.986 |

The substantive rows shown had passing A/A controls. Startup was
inconclusive. VM-only application protected multi-limb modulo on this
runner but lost several interpreter workloads by more than 5%.
Whole-program application again regressed modulo. The portable release
configuration remains the default.
