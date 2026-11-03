# Retained VM divisor-reuse native experiments

This package preserves nine original GitHub artifact ZIPs, their exact timing
executables and native inspection output, and a failed Windows test log. It
supplements `../bc-divisor-native-evidence`; it does not replace that package's
complete or partial foundation records. References: #420, #426, #216 and #197.

| Checkpoint | Native paired run | Scope per host | Checked observations, all hosts | Confirmed A/B losses |
| --- | --- | --- | ---: | --- |
| #423 d89 | 41570060439 | 66 full + 66 holdout | 19,800 | 0 |
| #423 f2 | 41572198170 | 66 full + 66 holdout, including fixed confirmations | 20,100 | 8, all Linux |
| #427 fa | 41576701042 | Separate 44-case full subset | 6,600 | 0 |

The eight f2 Linux losses cover full sqrt, stable division, changing divisors
and fresh temporaries; held-out stable division, changing divisors, fresh
temporaries and periodic changes also lose. See `audit-summary.json` and each
archive's `decisions.json` for exact ratios, intervals and controls. An
inconclusive A/A control does not waive a confirmed A/B loss. The f2 candidate
remains rejected for adoption even though its workflow checks were green.
The earlier d89 result cannot cancel the later losses, including when the
Linux executable bytes are identical on different recorded CPU models.

The d89 native artifacts are complete, but that source's Windows extensions-OFF
build/test job failed a 30-second mathematical-controller test deadline. Its
decoded log is retained in `failed-test.zip`. The f2 source contains the later
test-only host-info mocking correction. Do not label the earlier source wholly
green or discard its failed test because a later source passed.

The #427 subset is a test/coverage foundation, not a broad VM reuse trial.
Linux and macOS timing executables match their respective base bytes. The
Windows code payload differs; all three hosts' exact executables and native
inspection logs remain available. No claim that all three binaries match is
made. Counts across these different scopes are an inventory, never a pooled
performance score or a substitute for the complete competitive matrix.

## Verification

Run from any directory with Python 3:

```
python3 docs/utilities/bc-vm-native-evidence/verify.py
python3 docs/utilities/bc-vm-native-evidence/test-verifier.py
```

The verifier reuses the foundation's frozen symmetric policy and strict sample,
source, workload/oracle, executable and inspection checks. The foundation's
original 58-case full/holdout default remains unchanged. This package supplies
immutable checkpoint-specific 66/66 or 44-case contracts; its 44-case subset
cannot stand in for either broad collection. It verifies 27 exact binaries,
54 native inspections and the archived failed test's identity.

Experimental candidates must have explicit ON policy metadata bound to the
recorded compile-command bytes, and matching definitions for both VM and CLI
translation units. Their baseline/A/A sources and the #427 sources predate the
VM option: missing activation stays unknown, rather than being relabelled OFF.
Their recorded commands must contain no VM reuse opt-in. ZIPs include build
recipes, tool/host provenance, full checked warmup/measurement rows, generated
programs/oracles and confirmation decisions. Source identities and artifact
SHA-256 values are listed in `checkpoints.json`; `sha256.json` binds all package
payload bytes. No archived executable is run or extracted by verification.

Verification replays recorded nominal intervals and control qualification; it
does not independently recompute bootstrap intervals or repeat measurements.
The records do not establish universal speedups, memory improvements, complete
coverage or acceptance of a newer candidate. Later experiments, including
#430's changed miss route, require their own source-bound native review.
