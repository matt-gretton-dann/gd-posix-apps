# Prepared-divisor native checkpoints

Refs [#216](https://github.com/matt-gretton-dann/gd-posix-apps/issues/216),
[#420](https://github.com/matt-gretton-dann/gd-posix-apps/issues/420) and
[#197](https://github.com/matt-gretton-dann/gd-posix-apps/issues/197).

These twelve original native ZIPs retain 67,477 checked observations, 36 exact
timing binaries and 72 native inspection outputs from the coefficient API
(#421) and bounded admission (#422) work. Ten artifacts have complete grids and
final decisions. Two Windows artifacts are partial and cannot count as completed
gates. Counts include warm-ups and any predetermined confirmation samples; they
are evidence accounting, not pooled performance acceptance.

| Checkpoint | Native run | Linux checked | macOS checked | Windows checked | Treatment |
| --- | --- | ---: | ---: | ---: | --- |
| `421-0d971` | 41551973970 | 5,800 | 5,830 | 4,246 partial | Superseded API head |
| `421-964e` | 41553409459 | 5,800 | 5,800 | 5,800 | Accepted coefficient foundation |
| `422-173` | 41556208349 | 5,800 | 5,800 | 5,171 partial | Superseded admission head |
| `422-39d` | 41558399367 | 5,800 | 5,830 | 5,800 | Accepted bounded admission foundation |

[checkpoints.json](checkpoints.json) binds each ZIP to its published artifact
hash, exact base/candidate source revisions, host, raw grid counts and treatment.
[audit-summary.json](audit-summary.json) preserves control qualifications and
missing sample counts separately per artifact. The two partial artifacts lack
final decisions and have incomplete held-out paired grids: 1,554 and 629 samples
respectively are missing. Neither is promoted to a successful reduced grid.
All complete artifacts have no confirmed A/B regression under their recorded
intervals. Many macOS A/A controls are inconclusive; absence of a confirmed loss
does not establish a precise gain or waive results from another checkpoint.

The accepted API/admission heads had full three-host audits before their merges.
They add foundations; they do not themselves activate automatic VM reuse.
The explicitly activated VM experiment, its expanded admission controls and its
adoption decision remain separate under #423. The
[local VM cost evidence](../bc-vm-reuse-evidence/README.md) likewise has a separate
source/mode protocol. None of these records proves a universal speedup or closes
the remaining #197 requirements.

The initial admission head also failed four macOS/Windows build/test jobs in
run 41556208140. [failed-ci.zip](failed-ci.zip) preserves all four provider-decoded
UTF-8 log snapshots, with job identities and hashes in
[failed-ci-index.json](failed-ci-index.json). macOS exposed a test assumption
about process-global allocation balance; Windows rejected narrowing padding
arguments. Follow-up `39d5259c` isolated armed fault-injection allocations and
used the trait's limb type. The later green checks do not erase these failures.
Log hashes bind the saved decoded snapshots, rather than claiming original
transport-ZIP byte identity.

Run:

```sh
python3 docs/utilities/bc-divisor-native-evidence/verify.py
python3 docs/utilities/bc-divisor-native-evidence/test-verifier.py
```

Verification checks source/build/threshold provenance, uninstrumented flags,
program/oracle hashes, producer-checked raw rows, unique bounded sample rounds,
complete grids and frozen partial counts, exact executable copies and inspection
outputs. It replays recorded nominal intervals through the retained symmetric
A/A policy and directional A/B rule, preserving inconclusive qualifications.
It does not regenerate bootstrap intervals, independently rerun the archived
programs, execute machine code, or infer adoption. Hosted contention and small
sample limitations remain.

Thirteen tests reject missing/duplicate rounds, unchecked samples, partial/full
misclassification, changed binaries/workloads/logs, removed decisions and altered
control qualifications. CI runs both commands on Linux, macOS and Windows.
Hash-bound payloads have explicit byte-preserving checkout attributes.
