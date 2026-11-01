# Expanded sqrt corpus: bounded native acceptance

Refs #216, #212 and #197. PR #356 final source
`a678366d56a920fa466ada1befc326eebe808183`, native base
`704d8013c2ff877357bf9dc42a0ae68b571c6c3e`, run 41328592792.
All fourteen exact-head checks pass. Digest/source audits verify 2000
checked invocations per OS (6000 total), no confirmed regression.

The scale4096 tiny-square case uses one repeat and completes the unchanged
30-second deadline in GD and Howard on all hosts. The initial Windows
ten-repeat failure remains separately preserved in ../initial-native.
Do not relabel that failed workload or its partial samples as this one.

This changes measurement coverage, not production arithmetic. Native full
representatives now include scales 5000/10000, both tiny-fraction dimensions
and the huge-integer shape. MacOS intervals are noisy, including loop
1.089 [0.956,1.235]; infrastructure results are not production speed wins.
All controls, every class and source/compiler/binary identities remain in
the unmodified archives. PR #356 merged as
10d8a2726fef861925f9178d89d44fbe3ca061de and local main was immediately
fast-forwarded. Calibration and math-library classes remain #216 work.
