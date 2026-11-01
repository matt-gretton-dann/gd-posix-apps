# Expanded native fractional estimate: acceptance still fails

Refs #212, #200 and #197. Candidate
`f3a4707f3c07f7dd02d582070f9a66f397a00666`, actual native base
`704d8013c2ff877357bf9dc42a0ae68b571c6c3e`, run 41329122608.
The expanded bounded corpus is integrated; runtime arithmetic remains
the initial 444e53d experiment. All fourteen checks pass, but that does
not satisfy performance acceptance.

Digest/source audits verify 2030 Linux, 2000 macOS and 2000 Windows
checked invocations (6030 total). Linux decimal input again confirms a
regression at 1.120 [1.11472,1.15165] after bounded confirmation. The Linux
candidate binary hash is identical to the initial run's candidate, as
recorded in binary-comparison.json. Do not pool runs or select only a
favorable observation; the compiler mechanism remains unproven.

Tiny-root ratios 1024/4096 are about 0.008/0.004 Linux, 0.013/0.006 macOS
and 0.012/0.006 Windows. Those gains occur in affected native regimes,
but do not excuse the unrelated decimal cost. All controls, complete
classes and confirmation samples remain in the unmodified ZIPs.

This source was not adopted. PR #355 was subsequently closed without
merging; any resumed candidate with the separately accepted decimal kernel
requires fresh integrated acceptance. No instruction-level cause or
compiler-policy improvement is asserted from these timings.
