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
