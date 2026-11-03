# bc isolated coverage checkpoint

Measured source: `1d34776adfbdfede5e100d31fb76fa7b4a32df87`, clean working tree; 31 October 2026.

Apple Clang/LLVM 21.0.0, Debug, macOS 27.2 / arm64. Python 3.15.2 and CTest 4.4.5.
Separate extension builds and fresh asserted-test profiles; no build, tuner or manual profiles.
The [collector](../../src/bc/scripts/collect-coverage.py) preserves the complete JSON exports,
test inventory/logs, profile manifest, source/tool metadata, warnings and annotated reports.

## Raw totals

| Configuration / mappings | Lines | Branch outcomes | MC/DC conditions |
| --- | --- | --- | --- |
| ON / broad native/test-trait | 3987/4217 (94.55%) | 1896/2197 (86.30%) | 82/113 (72.57%) |
| ON / application | 3969/4203 (94.43%) | 1892/2195 (86.20%) | 79/113 (69.91%) |
| OFF / broad native/test-trait | 3909/4214 (92.76%) | 1850/2197 (84.21%) | 78/113 (69.03%) |
| OFF / application | 3891/4200 (92.64%) | 1846/2195 (84.10%) | 75/113 (66.37%) |

The application view uses only the application mappings, retaining the same asserted-test
profiles. It excludes instantiations mapped only in the unit executable. These scopes
are reported separately; neither silently replaces the original broad baseline.

## Raw per-file broad results

| File | ON lines | ON branches | ON MC/DC | OFF lines | OFF branches | OFF MC/DC |
| --- | --- | --- | --- | --- | --- | --- |
| bc.cc | 61/62 (98.39%) | 25/27 (92.59%) | 1/2 (50.00%) | 56/62 (90.32%) | 23/27 (85.19%) | 1/2 (50.00%) |
| bc.hh | 75/75 (100.00%) | 18/18 (100.00%) | 6/6 (100.00%) | 75/75 (100.00%) | 18/18 (100.00%) | 6/6 (100.00%) |
| instruction.cc | 381/382 (99.74%) | 310/360 (86.11%) | 2/2 (100.00%) | 381/382 (99.74%) | 310/360 (86.11%) | 2/2 (100.00%) |
| lexer.cc | 403/405 (99.51%) | 303/320 (94.69%) | 6/6 (100.00%) | 385/402 (95.77%) | 277/320 (86.56%) | 0/6 (0.00%) |
| number.hh | 1020/1110 (91.89%) | 385/486 (79.22%) | 23/36 (63.89%) | 1018/1110 (91.71%) | 386/486 (79.42%) | 25/36 (69.44%) |
| parser.cc | 913/938 (97.33%) | 315/341 (92.38%) | 17/27 (62.96%) | 893/938 (95.20%) | 308/341 (90.32%) | 18/27 (66.67%) |
| reader.cc | 75/75 (100.00%) | 24/24 (100.00%) | 5/5 (100.00%) | 75/75 (100.00%) | 24/24 (100.00%) | 5/5 (100.00%) |
| token.cc | 408/412 (99.03%) | 258/268 (96.27%) | 20/23 (86.96%) | 408/412 (99.03%) | 258/268 (96.27%) | 20/23 (86.96%) |
| vm.cc | 651/758 (85.88%) | 258/353 (73.09%) | 2/6 (33.33%) | 618/758 (81.53%) | 246/353 (69.69%) | 1/6 (16.67%) |

## Execution and limitations

- ON: 132 selected bc/unit entries, 87 CTest passes, 45 explicit unsupported imported skips, zero failures; 254 fresh profiles.
- OFF: 132 selected bc/unit entries, 69 CTest passes, 63 explicit unsupported imported skips, zero failures; 232 fresh profiles.
- The ON imported `misc` result remains an explicit expected oracle failure, described in [bc-testing.md](bc-testing.md); it is included in CTest passes and independently checked by the exact corrected oracle.
- Both runs have empty LLVM warning logs, including no missing-main/profile mismatch. OFF now executes asserted CLI and VM cases, unlike the original unit-only OFF baseline.
- OFF lexer MC/DC is reported raw even though those tracked compound blocks are extension-only and constant-inactive in that binary. Do not mix ON/OFF profiles to fill them.
- Source fixes and mapping changes since the original #197 baseline change denominators. These are reproducible checkpoint counts, not a pure test-only before/after speed or coverage claim.
- No eligible denominator or adjusted percentage is calculated. The [decision ledger](bc-coverage-ledger.md) preserves exact invariants, tool/configuration scope and open parser-reuse review; #199 and #215 retain arithmetic/VM gaps.
- Host LLVM instrumentation does not measure interpreted math-library algorithms. No performance conclusion follows from these tests.

Reproduce using the separate ON/OFF commands in [bc-testing.md](bc-testing.md), at the measured source revision. Later source or compiler changes require fresh reports.

## Arithmetic and VM checkpoint at cd0f96e

Measured source: `cd0f96e099b02e100644ad3105db6f92397790bd`, clean working tree; 31 October 2026.

The same Apple Clang/LLVM 21.0.0 Debug environment and isolated asserted-test procedure apply.
This revision includes the width-directed division, input conversion, signed-comparison,
leading-zero length, signal and result-type fixes. It predates the later control-flow
and marker verifier changes; it is a pinned checkpoint, not a measurement of those changes.

| Configuration / mappings | Lines | Branch outcomes | MC/DC conditions |
| --- | --- | --- | --- |
| ON / broad native/test-trait | 4193/4382 (95.69%) | 2114/2383 (88.71%) | 93/121 (76.86%) |
| ON / application | 4177/4368 (95.63%) | 2110/2381 (88.62%) | 91/121 (75.21%) |
| OFF / broad native/test-trait | 4131/4379 (94.34%) | 2075/2383 (87.08%) | 88/121 (72.73%) |
| OFF / application | 4115/4365 (94.27%) | 2071/2381 (86.98%) | 86/121 (71.07%) |

### Raw per-file broad results

| File | ON lines | ON branches | ON MC/DC | OFF lines | OFF branches | OFF MC/DC |
| --- | --- | --- | --- | --- | --- | --- |
| bc.cc | 61/62 (98.39%) | 25/27 (92.59%) | 1/2 (50.00%) | 56/62 (90.32%) | 23/27 (85.19%) | 1/2 (50.00%) |
| bc.hh | 75/75 (100.00%) | 18/18 (100.00%) | 6/6 (100.00%) | 75/75 (100.00%) | 18/18 (100.00%) | 6/6 (100.00%) |
| instruction.cc | 381/382 (99.74%) | 310/360 (86.11%) | 2/2 (100.00%) | 381/382 (99.74%) | 310/360 (86.11%) | 2/2 (100.00%) |
| lexer.cc | 403/405 (99.51%) | 304/320 (95.00%) | 6/6 (100.00%) | 385/402 (95.77%) | 280/320 (87.50%) | 0/6 (0.00%) |
| number.hh | 1042/1111 (93.79%) | 402/486 (82.72%) | 26/36 (72.22%) | 1040/1111 (93.61%) | 403/486 (82.92%) | 28/36 (77.78%) |
| parser.cc | 913/938 (97.33%) | 315/341 (92.38%) | 17/27 (62.96%) | 893/938 (95.20%) | 308/341 (90.32%) | 18/27 (66.67%) |
| reader.cc | 75/75 (100.00%) | 24/24 (100.00%) | 5/5 (100.00%) | 75/75 (100.00%) | 24/24 (100.00%) | 5/5 (100.00%) |
| token.cc | 408/412 (99.03%) | 258/268 (96.27%) | 20/23 (86.96%) | 408/412 (99.03%) | 258/268 (96.27%) | 20/23 (86.96%) |
| vm.cc | 835/922 (90.56%) | 458/539 (84.97%) | 10/14 (71.43%) | 818/922 (88.72%) | 451/539 (83.67%) | 8/14 (57.14%) |

### Evidence and remaining obligations

- ON: 146 selected entries, 101 CTest passes, 45 explicit unsupported skips, zero failures; 347 fresh profiles.
- OFF: 146 selected entries, 83 CTest passes, 63 explicit unsupported skips, zero failures; 324 fresh profiles.
- Both matching-tool runs have empty broad/application LLVM warning logs. The known imported misc oracle mismatch remains an expected failure and is independently checked by exact arithmetic.
- Matching Apple LLVM tools were selected explicitly. Earlier attempts selected Homebrew profile tools and failed with a raw-profile-version mismatch; those failed artifacts remain separate and supply no reported counts.
- Per-instantiation records confirm condition pairs for quotient correction, long binary-input chunking, leading-zero length traversal and signed comparisons. Broad and application mappings remain separate; raw template records are not summed to manufacture a denominator.
- The later source-tree output race (#271) is separate from these zero-failure results. Later runs must use build-local fixture copies or avoid overlapping imported suites sharing a dependency source.
- Remaining feasible numeric conversion, formatting and public-boundary obligations are still open, including #274/#275. The decision ledger records constraints; no eligible denominator, adjusted percentage or arithmetic/VM completion claim is made.
- Source and mapping changes alter denominators. These counts are not a pure test-only delta or a performance claim.


## Regression and VM execution checkpoint (ce52323)

Measured clean source `ce52323abbc035c34e6c3bd208507264a1075c7f`, included in
main by PR #289. Apple Clang/LLVM 21.0.0, Debug, macOS/arm64, separate ON/OFF
builds and fresh test-only profiles using the documented collector. Both
collections have zero test failures and empty broad/application warning logs.
No benchmark, tuner or manual-smoke profiles contribute.

| Configuration / mapping scope | Lines | Branch outcomes | MC/DC conditions |
| --- | --- | --- | --- |
| ON / broad native/test-trait | 4587/4730 (96.98%) | 2349/2606 (90.14%) | 127/163 (77.91%) |
| ON / application | 4571/4716 (96.93%) | 2344/2604 (90.02%) | 125/163 (76.69%) |
| OFF / broad native/test-trait | 4539/4727 (96.02%) | 2317/2606 (88.91%) | 122/163 (74.85%) |
| OFF / application | 4523/4713 (95.97%) | 2312/2604 (88.79%) | 120/163 (73.62%) |

### Raw per-file broad results

| File | ON lines | ON branches | ON MC/DC | OFF lines | OFF branches | OFF MC/DC |
| --- | --- | --- | --- | --- | --- | --- |
| bc.cc | 61/62 (98.39%) | 25/27 (92.59%) | 1/2 (50.00%) | 56/62 (90.32%) | 23/27 (85.19%) | 1/2 (50.00%) |
| bc.hh | 75/75 (100.00%) | 18/18 (100.00%) | 6/6 (100.00%) | 75/75 (100.00%) | 18/18 (100.00%) | 6/6 (100.00%) |
| instruction.cc | 381/382 (99.74%) | 310/360 (86.11%) | 2/2 (100.00%) | 381/382 (99.74%) | 310/360 (86.11%) | 2/2 (100.00%) |
| lexer.cc | 403/405 (99.51%) | 304/320 (95.00%) | 6/6 (100.00%) | 385/402 (95.77%) | 280/320 (87.50%) | 0/6 (0.00%) |
| number.hh | 1102/1127 (97.78%) | 439/504 (87.10%) | 32/42 (76.19%) | 1110/1127 (98.49%) | 445/504 (88.29%) | 34/42 (80.95%) |
| parser.cc | 926/949 (97.58%) | 318/343 (92.71%) | 17/27 (62.96%) | 906/949 (95.47%) | 311/343 (90.67%) | 18/27 (66.67%) |
| reader.cc | 75/75 (100.00%) | 24/24 (100.00%) | 5/5 (100.00%) | 75/75 (100.00%) | 24/24 (100.00%) | 5/5 (100.00%) |
| token.cc | 408/412 (99.03%) | 258/268 (96.27%) | 20/23 (86.96%) | 408/412 (99.03%) | 258/268 (96.27%) | 20/23 (86.96%) |
| verification.cc | 310/316 (98.10%) | 192/200 (96.00%) | 28/36 (77.78%) | 310/316 (98.10%) | 192/200 (96.00%) | 28/36 (77.78%) |
| vm.cc | 846/927 (91.26%) | 461/542 (85.06%) | 10/14 (71.43%) | 833/927 (89.86%) | 456/542 (84.13%) | 8/14 (57.14%) |

### Interpretation and open work

- ON: 184 selected entries, 139 CTest passes, 45 explicit unsupported imported skips, zero failures; 426 fresh profiles.
- OFF: 184 selected entries, 121 CTest passes, 63 explicit unsupported imported skips, zero failures; 403 fresh profiles.
- Both scopes include the newly linked `verification.cc`; its 316 lines, 200 branch outcomes and 36 MC/DC conditions were absent from the earlier nine-file checkpoint. New conversion/output/parser decisions also change mapping denominators. These are source-and-test deltas, not a pure test-only comparison.
- The source includes the constructor, parity, conversion, fatal-parser, diagnostic and output-boundary fixes, exact fraction helpers, bounded scale-folding powers, direct VM semantics and seeded grammar/bytecode oracles. The arithmetic ledger identifies the residual configuration/invariant obligations individually; assertions and defensive paths remain in the raw results.
- Newly exposed feasible verifier decision pairs, invalid store destinations and direct OFF array-length execution are covered by the separate follow-up PR #290, which is not included in this checkpoint. Measure those before closing the VM audit.
- Earlier imported `misc` expected-oracle behavior and the supported-test skips remain visible. Build-local fixture copies prevent cross-build output races. Other native platforms run behavior tests in PR CI; the MC/DC measurements here are from this macOS/arm64 compiler configuration.
- No eligible denominator or adjusted percentage has been calculated. Neither these raw results nor the exception ledger claim 100% raw coverage, complete conformance, larger #184/#186 limits, or a performance improvement.


## Arithmetic and frontend review checkpoint (c4c1415)

Clean measured source `c4c141546c92b6bc76df8607f133ee31fc5e3068` includes the
merged correctness/coverage changes through #295 and the mandatory verifier
mapping check. Apple Clang/LLVM 21.0.0, Debug, macOS/arm64; separate builds,
matching tools and fresh test-only profiles. No benchmark, tuner, manual-smoke
or fault-injection profiles contribute.

| Configuration / mapping scope | Lines | Branch outcomes | MC/DC conditions |
| --- | --- | --- | --- | --- |
| ON / broad native/test-trait | 4594/4731 (97.10%) | 2360/2606 (90.56%) | 136/163 (83.44%) |
| ON / application | 4580/4717 (97.10%) | 2357/2604 (90.51%) | 136/163 (83.44%) |
| OFF / broad native/test-trait | 4555/4728 (96.34%) | 2332/2606 (89.49%) | 133/163 (81.60%) |
| OFF / application | 4541/4714 (96.33%) | 2329/2604 (89.44%) | 133/163 (81.60%) |

### Raw per-file broad results

| File | ON lines | ON branches | ON MC/DC | OFF lines | OFF branches | OFF MC/DC |
| --- | --- | --- | --- | --- | --- | --- |
| bc.cc | 61/62 (98.39%) | 25/27 (92.59%) | 1/2 (50.00%) | 56/62 (90.32%) | 23/27 (85.19%) | 1/2 (50.00%) |
| bc.hh | 75/75 (100.00%) | 18/18 (100.00%) | 6/6 (100.00%) | 75/75 (100.00%) | 18/18 (100.00%) | 6/6 (100.00%) |
| instruction.cc | 381/382 (99.74%) | 310/360 (86.11%) | 2/2 (100.00%) | 381/382 (99.74%) | 310/360 (86.11%) | 2/2 (100.00%) |
| lexer.cc | 403/405 (99.51%) | 304/320 (95.00%) | 6/6 (100.00%) | 385/402 (95.77%) | 280/320 (87.50%) | 0/6 (0.00%) |
| number.hh | 1103/1128 (97.78%) | 442/504 (87.70%) | 32/42 (76.19%) | 1111/1128 (98.49%) | 448/504 (88.89%) | 34/42 (80.95%) |
| parser.cc | 926/949 (97.58%) | 318/343 (92.71%) | 17/27 (62.96%) | 906/949 (95.47%) | 311/343 (90.67%) | 18/27 (66.67%) |
| reader.cc | 75/75 (100.00%) | 24/24 (100.00%) | 5/5 (100.00%) | 75/75 (100.00%) | 24/24 (100.00%) | 5/5 (100.00%) |
| token.cc | 408/412 (99.03%) | 258/268 (96.27%) | 20/23 (86.96%) | 408/412 (99.03%) | 258/268 (96.27%) | 20/23 (86.96%) |
| verification.cc | 316/316 (100.00%) | 199/200 (99.50%) | 35/36 (97.22%) | 316/316 (100.00%) | 199/200 (99.50%) | 35/36 (97.22%) |
| vm.cc | 846/927 (91.26%) | 462/542 (85.24%) | 12/14 (85.71%) | 842/927 (90.83%) | 461/542 (85.06%) | 12/14 (85.71%) |

### Review outcome and limits

- ON: 197 selected entries, 152 CTest passes, 45 explicit unsupported imported skips, zero failures; 440 fresh profiles.
- OFF: 197 selected entries, 134 CTest passes, 63 explicit unsupported imported skips, zero failures; 417 fresh profiles.
- Both broad and application LLVM warning logs are empty. Removing `verification.cc` from a genuine export is rejected by the collector's mandatory inventory; that separate fault-injection run supplies no measurement profiles.
- The earlier claim that private empty multiplication ranges were infeasible was incomplete. Sparse recursive blocks can normalize half sums to empty vectors. Exact polynomial fixtures now record both conditions covered in the native application mappings, ON/OFF, with valid positive thresholds. Both scopes now agree on covered MC/DC counts; small-trait evidence is retained rather than discarded.
- #199's assertion repair, exact scaled arithmetic/quotient/remainder/sqrt oracles, threshold boundaries, copy/aliasing checks, native width cases and remaining ordinary numeric boundaries are implemented. The arithmetic ledger classifies residual guards/preconditions individually, including strict wrapping tails and working-scale invariants. Existing #184/#186 limits remain separate.
- #214's asserted CLI/frontend/reader paths, terminal parser behavior, independent-condition tests and reproducible collection are implemented. Remaining frontend decisions are configuration constants, constrained private grammar states, explicit constructor invariants or documented source-mapping/uncalled-private-code artifacts. Recoverable parsing (#185) is not added.
- #215 remains open at this checkpoint: OFF has four uncovered special-register restoration lines because CLI `-g` is disabled. The VM constructor still accepts that mode, so a direct ON/OFF test is required; this is not a configuration exemption for the public VM API. The separately documented execution-stage invariants do not excuse this path.
- The remaining 27 ON / 30 OFF raw MC/DC obligations retain their LLVM counts and source-specific classifications. Debug assertion failures are separate invariant/death-test scope. No boolean was rewritten, no invariant weakened, and no undefined/private state injected to improve counts.
- No eligible denominator or adjusted percentage is computed. Closure of the arithmetic/frontend test work is based on the planned exact tests and the source-specific feasible-gap review, not a claim of 100% raw coverage or conformance. Other native platforms run Release behavior CI; these MC/DC measurements are specific to the recorded macOS/arm64 compiler.


## Portable library checkpoint at 2579bfe

Measured source: `2579bfe23d2785d1bd0c3052b32275fc70b66a20`, 1 November 2026,
Homebrew Clang/LLVM 23.1.4 Debug on macOS 27.2/arm64. Raw profiles, profdata,
exports, HTML, test inventories/logs, binary identities and compile commands
are preserved in the [library validation archive](bc-217-library-validation/README.md).

| Configuration / mappings | Lines | Branch outcomes | MC/DC conditions |
| --- | --- | --- | --- |
| ON / broad native/test-trait | 5634/5783 | 2807/3103 | 190/241 |
| ON / application | 5603/5752 | 2805/3101 | 190/241 |
| OFF / broad native/test-trait | 5599/5780 | 2787/3103 | 187/241 |
| OFF / application | 5568/5749 | 2785/3101 | 187/241 |

ON has 257 passes and 45 explicit imported skips, with 955 fresh profiles;
OFF has 239 passes and 63 skips, with 931 profiles. Both select 302 entries,
have zero failures, and have empty broad/application LLVM warning logs. The
legacy exact-output math entries remain explicit skips while their calls run
through the independent numerical adapter.

The loader's `bc.cc` mapping remains 61/62 lines, 25/27 branch outcomes and
1/2 MC/DC ON; OFF is 56/62, 23/27 and 1/2. Library loading executes in both
directions. The null-instruction invariant, unreachable post-error break and
option-schema/extension constraints remain in the raw counts and source-specific
ledger; this checkpoint does not infer a new eligible denominator or completion
percentage. Changes in compiler and source preclude a pure test-only comparison
with the earlier LLVM 21 checkpoints. ON and OFF profiles remain separate.

Both source-status snapshots record an untracked `default.profraw` from manual
unit validation. It is excluded from every isolated input manifest; tracked
code equals the stated revision. The collector merges only its newly created
`test-profiles` directory.

Interpreted `.bc` decisions are a separate scope: 476 certified numerical
results observe 121/132 normal outcomes, 130/132 including nine independent
scale-clipping host models, with two proved infeasible negative-exponential
outcomes. That evidence does not fill C++ MC/DC obligations. Later source
changes require fresh reports, and the final roadmap review still needs its
current-source feasible-gap assessment.
