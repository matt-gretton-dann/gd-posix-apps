# Whole-program Clang sample PGO on Linux

[Hosted run 41820312252](https://github.com/matt-gretton-dann/gd-posix-apps/actions/runs/41820312252)
tested exact revision `ecbff8b7fe9a268cd589646279b1e30ace6ad98c`
with Ubuntu Clang 18.1.3 and ThinLTO on EPYC 9V74. The archive retains the
66-workload holdout manifest, 2,640 checked rows, both paired summaries and
section sizes. The original artifact also includes raw training samples,
profiles, compiler commands and build logs. Its ZIP SHA-256 was
`067e887d36934930bba8d2f63f9faf7d6bb1571a964bf48a66e5aa85018622bd`.

The portable and A/A builds had identical text hashes. Profile application
changed the text hash and increased the reported text size by 16.1%. The
builds took 24.81 seconds (portable), 24.20 (A/A) and 25.47 (profiled).
The eight training workloads and the holdout shared no program hashes.
The checked CLI suite yielded only one executable sample, so cold and
error paths had weak training coverage.

| Held-out workload | Profiled / portable [95%] |
| --- | ---: |
| Loop | 0.889 [0.866, 0.910] |
| Calls | 0.895 [0.892, 0.910] |
| Sparse array | 0.878 [0.867, 0.882] |
| Scale-1125 square root | 1.632 [1.570, 1.662] |
| Multi-limb modulo | 1.506 [1.501, 1.520] |
| Periodic modulo | 1.271 [1.224, 1.290] |
| Startup | 1.069 [0.983, 1.249] |

The substantive rows shown had passing A/A controls. The startup control
was noisy. The regressions rule out whole-program profile use as a default
under the Linux 5% allowance. A VM-only role tests whether the interpreter
gains persist with narrower profile application.
