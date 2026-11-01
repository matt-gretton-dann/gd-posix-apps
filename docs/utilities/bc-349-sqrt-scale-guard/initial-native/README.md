# Initial square-root boundary fix — held native candidate

Refs #349, #212 and #197. Exact candidate
`c08b25957c3137585a58789211970827270db5ec`; native base
`34840e4fd1e350e2600f55210b6485d0f2f48c19`. All fourteen ordinary
CI/native/CodeQL checks pass, but native performance decisions are advisory.
Do not merge this source: the Windows artifact confirms a comparison regression.

The candidate fixes the generic guard-scale wrap and post-rescale length query.
Release passes 214 cases/692004 assertions and eight CLI/helper groups; OFF
ASan/UBSan passes 171 numeric/VM cases/507758 assertions and seven independent
oracle groups. Both failing test stages remain in the logs and commit history.

Digest/source-verified artifacts retain 1780 Linux, 1750 macOS and 1780 Windows
checked invocations (5310 total). Windows equal comparison initially measures
1.063 [1.05285,1.07660] of baseline; the bounded confirmation also exceeds 1.05,
so the policy reports confirmed regression. Its independent A/A control is
1.001 [0.99763,1.01479]. Linux equality remains inconclusive, and macOS intervals
are wide/noisy. Small-scale sqrt itself passes on Linux/Windows and remains noisy
on macOS. Complete raw intervals, controls, recipes and toolchains stay in ZIPs.

The comparison workload does not execute sqrt. A compiler inlining/layout
interaction is a hypothesis, not a proven cause. Evaluate a source-level MSVC
sqrt inlining boundary separately and retain all fresh measurements; do not
rerun the unchanged candidate until favorable or relabel this dataset.
