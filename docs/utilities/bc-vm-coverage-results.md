# VM coverage closeout

This #215 checkpoint complements the arithmetic/frontend report in
[bc-coverage-results.md](bc-coverage-results.md) and its source-specific
[decision ledger](bc-coverage-ledger.md). Measured clean source:
`d8effdc8c8b339291517426b7a114d0e76451304`, Apple Clang/LLVM 21.0.0, Debug,
macOS/arm64. Separate ON/OFF builds and fresh test-only profiles use matching
Apple LLVM tools. Both exports include all ten production files, including
`verification.cc`; #291 additionally makes its presence mandatory in the common
collector. No benchmark, tuner, manual-smoke or fault-injection profiles count.

| Configuration / mapping scope | Lines | Branch outcomes | MC/DC conditions |
| --- | --- | --- | --- | --- |
| ON / broad native/test-trait | 4594/4731 (97.10%) | 2360/2606 (90.56%) | 136/163 (83.44%) |
| ON / application | 4580/4717 (97.10%) | 2357/2604 (90.51%) | 136/163 (83.44%) |
| OFF / broad native/test-trait | 4559/4728 (96.43%) | 2333/2606 (89.52%) | 133/163 (81.60%) |
| OFF / application | 4545/4714 (96.41%) | 2330/2604 (89.48%) | 133/163 (81.60%) |

## VM and verifier results

| File / configuration | Lines | Branch outcomes | MC/DC conditions |
| --- | --- | --- | --- |
| vm.cc / ON | 846/927 (91.26%) | 462/542 (85.24%) | 12/14 (85.71%) |
| vm.cc / OFF | 846/927 (91.26%) | 462/542 (85.24%) | 12/14 (85.71%) |
| verification.cc / ON | 316/316 (100.00%) | 199/200 (99.50%) | 35/36 (97.22%) |
| verification.cc / OFF | 316/316 (100.00%) | 199/200 (99.50%) | 35/36 (97.22%) |

ON: 198 selected entries, 153 CTest passes, 45 explicit unsupported imported
skips, zero failures; 441 fresh profiles. OFF: 198 selected entries, 135 passes,
63 skips, zero failures; 418 fresh profiles. Broad/application LLVM warning
logs are empty. The imported `misc` expected-oracle mismatch remains explicit
and independently checked against POSIX arithmetic; CTest pass totals include
that expected status and are not a blanket conformance claim.

## Public API gap closed

A direct VM test runs both special-register save modes in either build. Its
function changes scale, ibase and obase; exact caller outputs distinguish
restored from retained values after explicit radix normalization. Debug ON and
Release OFF each pass eight targeted assertions. The instrumented OFF run now
executes all four previously missing restoration lines and the corresponding
branch; VM file metrics match ON. The disabled CLI `-g` spelling is not used as
an exemption for this callable constructor argument.

The delivered suite also covers arithmetic/comparison/unary opcodes, branches,
register persistence, every supported diagnostic result alternative and stream,
arrays/functions/recursion/argument errors, verifier bounds/types/regions/joins/
dominance/markers, and bounded interrupt delivery/restoration. Seeded expression
and graph properties use independent integer/reachability oracles. Addition and
operand-2 dominance mutants are rejected; restored production passes. Arithmetic
and storage fixes were separately checked with ASan/UBSan and native CI.

## Residual constraints and future changes

The verifier's only raw branch/MC/DC gap is the optional-state test inside
`unconsumed`: its callers use the completed reachable set or successors already
assigned by marker-state BFS. VM's two remaining raw MC/DC obligations are the
allocated-empty array state unavailable through normal supported instructions,
and the function scan reaching stream end after matching boundaries were
validated. Their exact proofs, exceptional resource-failure boundary and
post-validation diagnostic/result-shape checks remain in the decision ledger.
Ordinary malformed streams and dynamic argument errors remain tested public
behavior. Function-end metadata, EOF return flow and explicit constructor/debug
assertion preconditions retain their raw mappings.

The planned #215 tests and current feasible-gap review are complete with #291.
No raw 100% coverage, synthetic eligible denominator or adjusted percentage is
claimed. Preserve the documented unsupported skips, numeric limits (#184/#186)
and recovery boundary (#185). Other native platforms run Release ON/OFF behavior
CI; the MC/DC counts here belong to this recorded compiler/platform. Future
validated/optimized execution formats (#207), activation frames (#208), arrays
(#209) and math-library work (#217) must carry these oracles and revisit affected
feasibility proofs. There is no optimized VM mode or performance claim in this
checkpoint.
