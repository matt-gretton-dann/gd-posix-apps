# Accepted portable dispatch/tuner

PR #333 merges source `2e46054b43af136569db76a242bfad7c9ee37888`. Native run
41285866028 compares exact base `222f34ca3c90994d23fea4a14ac1c00d5ca02f3a`.
All three platforms pass 1450 exact output/status/deadline checks, and no
confirmed regressions remain under the provisional native policy. Both
previously failing Windows extension jobs pass the steady-clock quick-sweep
protocol. All fourteen checks are green. ZIP digests and exact source identities
are recorded in adjacent audit files; raw programs, recipes, A/A observations,
paired samples and decisions remain in the ZIPs. Startup/noisy intervals remain
inconclusive; this is not a claim of universal neutral performance.

The portable default remains 126. Explicit numeric and native-auto choices
remain available; cross-auto retains 128. Three locally selected 32-limb
thresholds do not become a portable policy. This closes #203 together with its
previously accepted workspace, square and blocked kernels and its retained
rejected asymmetric ordering experiment. The earlier CPU-clock Windows failure
and every original local per-shape cost remain in the parent report.
