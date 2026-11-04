# Portable library correctness and coverage checkpoint

Refs #217, #214, #216 and #197.

The ordinary mathematical definitions and their state/argument behavior are
validated independently of timing. This archive does not establish a competitive
performance target. The canonical definitions at this checkpoint initialize
scale with the original literal `20`; later source changes require new validation.

## Numerical, state and portability evidence

The original 336 plus 140 crossover enclosures certify 476 function/scale cases.
The integrated source `4136d2f` passes all four actual extensions-OFF ASan/UBSan
groups in 397.37 s: runtime 264.10 s, identity 0.04 s, interpreted decisions 125.43 s,
imported calls 7.79 s. Test elapsed times are correctness diagnostics, not speedups.
The identical ordinary source also passes 476 cases in pinned Howard 7.0.3
without its standard library. Fourteen oracle groups pass on Python 3.9/3.15.

Imported adapters account for 1581 calls: 1073 certified results, 507 explicit
undefined-domain inputs and one ordinary identity replacement. Generated
atan/Bessel input, generator hashes and the strict return/halt adjustments
remain in the committed fixtures and adapter. The earlier 360-invocation
three-role preflights use a non-frozen GD build and discard elapsed times;
they are preliminary correctness checks, not performance acceptance.

Interpreted probes preserve one evaluation of each original condition and
distinguish loop exhaustion, break and return. The report observes 121/132
normal outcomes and 130/132 including nine independently clipped setters.
The two remaining outcomes require a negative exponential coefficient and
are infeasible by positive-series/squaring/reciprocal construction. The
per-line report and portable test-side transformer are retained separately
from C++ source coverage. Source/hash and missing-outcome checks run in CTest.

## Isolated LLVM 23 C++ coverage

Both configurations measure exact source `2579bfe`, with Homebrew Clang 23.1.4
Debug on macOS 27.2/Apple M1. Empty warning logs, all asserted tests successful.
The ON run has 257 passes/45 explicit skips and 955 fresh profiles; OFF has
239 passes/63 skips and 931 fresh profiles. Each selects 302 CTest entries.
Legacy exact-output imported skips remain explicit; mathematical calls are
checked by the independent numerical adapter rather than those snapshots.

| Configuration / mappings | Lines | Branch outcomes | MC/DC conditions |
| --- | --- | --- | --- |
| ON / broad | 5634/5783 | 2807/3103 | 190/241 |
| ON / application | 5603/5752 | 2805/3101 | 190/241 |
| OFF / broad | 5599/5780 | 2787/3103 | 187/241 |
| OFF / application | 5568/5749 | 2785/3101 | 187/241 |

The driver `bc.cc` has 61/62 lines, 25/27 branches and 1/2 MC/DC ON;
OFF is 56/62, 23/27 and 1/2. Library loading is exercised in both directions.
The existing null-instruction invariant, unreachable post-error break and
option-schema/extension constraints remain visible in raw counts and the
feasibility ledger. No new eligible denominator or overall completion claim
is inferred. LLVM 23/source changes preclude treating older LLVM 21 percentages
as a pure test-only before/after comparison. Never pool ON/OFF profiles.

Both summaries honestly record an untracked `default.profraw` from manual
unit validation. Every isolated manifest was checked to exclude it; tracked
source equals the stated checkpoint. Only the fresh test-profiles directories
are merged. Raw profiles, profdata, exports, HTML, logs, inventories, source
revision/status and compiler metadata are retained in the two ZIPs.

For reproduction, check out `2579bfe`, use the recorded compiler/configuration
and the collector commands from [bc-testing.md](../bc-testing.md), in separate ON/OFF builds.
The original absolute paths in manifests/reports are retained, not relabeled;
recreate their build/source prefix or regenerate the profile list after
extraction. Rebuild the matching mapping binaries from that source before
using llvm-cov with retained profiles. Later source changes require fresh
coverage. The canonical source and test-side scripts here identify exactly
the interpreted definitions tested at this checkpoint.

## Existing arithmetic native control

Native run 41340731137 measures base `8bf47fa` and candidate `2579bfe`. All fourteen
checks pass and each OS verifies 2000 invocations, 6000 total, with no confirmed
regression. macOS and Windows retain noisy/inconclusive intervals; these are
not equivalence claims. The raw ZIPs and audits are retained. This checkpoint
uses its original IID interval policy and the arithmetic corpus; it does not
establish native mathematical performance or apply later inference policies.
