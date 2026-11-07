# Accepted-source Windows process controls

This #200 diagnostic investigates why fixed deployments can time differently
before another #197 runtime candidate is attempted. It cannot approve a runtime
change or revive a rejected qualification. No production elapsed benchmark runs
locally. The existing `profile-bc.py` process-handle adapter supplies native
child CPU accounting; unsupported hosts report unavailable without building or
executing bc.

The accepted runtime source is fixed at
`e2de128f090f1d3379927b0e31b6882ca41fdda0`. Each of three predetermined
`windows-2025` replicas independently builds that revision twice, with native
MSVC, extensions ON, split point 126 and ordinary default options. The four
roles are the first executable, an alias with the exact same file and argv, a
distinct physical copy of its unchanged bytes, and the second build. Both
builds and the copy finish before measurements. Different build/deployment
paths are recorded; their individual effects are not isolated by this design.

The fixed corpus contains eight full and eight held-out mathematical cases:
all six regular functions and the turning-point Bessel case at scale 20, plus
the argument-dominates Bessel case at scale 100. Each uses exactly 4,096 calls
per process, checked against the existing independent decimal enclosures and
scale/base/state oracles. Counts are not calibrated or adapted. Short observed
CPU durations remain explicit rather than triggering longer or repeated runs.

Each replica executes three warm-up and nineteen measured rounds per case and
role: 1,408 checked processes, including warm-ups. The full seed is
`19768000 + 10 * replica`; the held-out seed adds one. Each round shuffles cases
and then roles using Python's fixed-seed random generator. Raw rows preserve
phase, round, role order, checked output identities and native accounting.

Wrapper wall time spans process launch, communication and native accounting
queries, with output checking and identity hashing outside it. It differs from
the production elapsed harness and cannot replace its measurements. Total
child CPU time is the sum of native user and kernel ticks. Counter units are
not a claim of accounting resolution. A zero or missing counter prevents a CPU
ratio; missing data are retained. The declared duration diagnostic requires
every role's median CPU time to reach 0.5 seconds. Positive ratios for shorter
cases remain descriptive and do not satisfy that requirement.

All nineteen within-round pairs use the maintained paired IID/circular-block
interval calculation, with 2,000 resamples and their preset's fixed seed. These
are conditional descriptions of these specific files and replica, with no
multiple-comparison acceptance test, population-of-layouts estimate, pooling,
source-causal attribution, startup equivalence or runtime adoption decision.
There are no outcome-dependent confirmations, favorable restarts or replacement
replicas. Every adverse, unavailable, short or incomplete outcome remains.

The complete controller budget is 6,600 seconds, including two builds bounded
at 1,800 seconds each. Each measured child has at most 120 seconds, clipped to
the remaining budget. A timeout or incorrect output is a hard failure with
prior streamed samples retained. After every elapsed stage has ended, the
collector preserves available actual executable bytes, recipes and declared
inputs, with optional inspection limited to 15 seconds per command and the
remaining total budget. Expired inspection deadlines still preserve images.
Preservation failure prevents success. No missing measured image is rebuilt.

Before publication, #200 records the immutable controller commit, protocol and
workflow hashes, accepted runtime SHA and the three-replica cohort. Native
build/test, contract and CodeQL checks remain required for the tooling PR.
The diagnostic does not change #653's performance acceptance rules. Its results
inform the next attribution experiment; they do not alone explain any older
candidate's regression.
