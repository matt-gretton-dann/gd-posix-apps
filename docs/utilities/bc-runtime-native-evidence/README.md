# Retained runtime native experiments (#441, #197)

This package continues `../bc-vm-native-evidence`. Original GitHub artifact
ZIPs retain source/build identities, exact timing executables, native inspection
logs, workload programs and output oracles, checked observations, confirmation
decisions and A/A controls. Archived executables are never run by verification.

| Checkpoint | Scope per host | Checked observations, all hosts | Recorded outcome |
| --- | --- | ---: | --- |
| #430 5b | 66 full + 66 held-out | 19,820 | Earlier explicit opt-in checkpoint, superseded |
| #430 e2 | 66 full + 66 held-out | 19,840 | Opt-in adopted; production default remains OFF |
| #433 8e | 66 full + 66 held-out | 19,950 | Rejected: three confirmed Windows losses |
| #435 182 | Separate 52-case full subset | 7,800 | Language-mode foundation adopted |
| #433 5d | 66 full + 66 held-out | 19,840 | Parser terminal-state fix adopted |
| #436 5f | 66 full + 66 held-out | 19,860 | Rejected: confirmed Windows multiplication loss |
| #437 b6 | 66 full + 66 held-out | 19,840 | Compact verified execution adopted |
| #438 d8 | 66 full + 66 held-out | 19,800 | Rejected locally despite favorable native advisory results |
| #438 46 | 66 full + 66 held-out | 19,820 | Mixed benefit; remains an unmerged experiment |
| #440 b5 | Linux/macOS complete; Windows held-out partial | 18,873 | Failed CI; six reproduced full-preset Windows multiplication losses |
| #440 41 | 66 full + 66 held-out | 20,180 | Green correctness CI; rejected: ten confirmed Windows losses |

`checkpoints.json` pins every exact base/candidate revision and archive SHA-256.
`audit-summary.json` retains all replayed intervals, confirmed losses and control
qualifications. The counts above are an evidence inventory, never a pooled
timing score. The 52-case #435 foundation cannot replace a broad collection.

The #430 candidate binaries explicitly enable prepared-divisor reuse. Their
controls predate the option: absent activation stays unknown. Later checkpoints
record explicit default-OFF policy and compile definitions. Do not attribute the
#430 opt-in results to the production default, or rewrite earlier source modes.

The #436 Windows full small-multiplication confirmation ratio is approximately
1.0796, with nominal 95% interval [1.0655, 1.1603]. Inconclusive controls or green
workflow checks do not cancel that loss. The initial #438 d8 local loop loss
was reproduced at approximately 1.0920 [1.0892, 1.0974]; favorable native results
do not erase it. The revised #438 46 local loop ratio is approximately 1.0217
[1.0196, 1.0266]. It improves some Windows classes but does not establish an
overall benefit. These local observations are recorded separately in the PR
discussions; they are not silently relabelled as native workflow samples.

#437 uses compact execution only in the candidate, while #438 uses it in all
roles and enables fused loads only in candidates. Rejected #436 live-register
allocation is not part of #437. Each archived checkpoint retains its actual
configuration rather than inheriting labels from later code.

`failed-checks.json` preserves exact diagnostic-line excerpts from the initial
#440 b54f0c3b build/test run: an edited hash-protected historical README and
GCC 13's replacement-delete diagnostic. These excerpts are not complete job
logs or native timing samples. The source remains a failed CI checkpoint even
after the later correction. Its initial Windows collection is partial after a superseding push cancelled
the run: 5,643 checked observations and no completed decisions.json. Its full
initial/confirmation intervals nevertheless reproduce six multiplication losses,
with confirmation median ratios 1.0763–1.1325. These are replayed separately and
cannot be hidden by the incomplete held-out gate. The corrected 41873290 head has a complete independent collection: ten confirmed
Windows losses (four full, six held-out) prevent adoption despite green
correctness CI. The Windows code payload SHA-256 is identical between these
two sources: 023f1c9dea9856858632486922d3a24c80a0b8d120c5a6d54b1136adfc13327d.
Test/documentation corrections do not establish performance recovery. A later
native-driver isolation experiment requires separate source-bound evidence.

## Offline verification

Run with Python 3, without network access or numerical dependencies:

```
python3 docs/utilities/bc-runtime-native-evidence/verify.py
python3 docs/utilities/bc-runtime-native-evidence/test-verifier.py
```

The verifier reuses the foundation's frozen symmetric advisory classifier and
strict executable/sample/oracle checks. Immutable checkpoint-specific corpus
contracts leave the foundation's original 58-case default untouched. Additional
checks bind modern activation policy to exact compile-command bytes, require
the VM/CLI divisor and language modes, and distinguish VM-private compact,
live-register and fusion definitions. Duplicate definitions and undefinitions
fail closed. Transport hashes use portable slash-separated relative paths.

Mutation tests cover source and corpus substitutions, dropped mode definitions,
duplicate/contradictory options, stale command hashes, relabelling legacy or
modern activation, Windows option/path conventions, duplicate sample rounds and
attempts to drop or relabel failed-build diagnostics. Linux, macOS and Windows
build/test CI replay both verification and mutation tests.
`sha256.json` binds the package payload inventory and bytes; the index separately
binds the original artifact ZIPs. Verification does not extract archives.

Replay verifies recorded nominal intervals and decisions; it does not recompute
bootstrap intervals, repeat measurements, independently certify host stability,
or turn advisory classifications into merge acceptance. Many macOS A/A controls
remain inconclusive. Historical performance decisions retain their own source
identities. The later Linux/Windows allowance of at most 5% per-change slowdown
does not erase confirmed losses above that limit; the macOS requirement is
unchanged. No overall Howard target, portable math-library competitiveness,
process-wide allocation reduction or complete coverage is established here.

References: #197, #207, #216, #420, #426; PRs #430, #433, #435, #436, #437 and #438.
