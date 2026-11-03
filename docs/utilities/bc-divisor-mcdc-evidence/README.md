# Separate divisor coverage collections

Refs [#426](https://github.com/matt-gretton-dann/gd-posix-apps/issues/426),
[#420](https://github.com/matt-gretton-dann/gd-posix-apps/issues/420) and
[#197](https://github.com/matt-gretton-dann/gd-posix-apps/issues/197).

[inventory.json](inventory.json) binds eight retained collections to exact
source revisions and compressed payload hashes. [summary.json](summary.json)
keeps their LLVM summaries and focused branch outcomes separate. These records
do not establish complete feasible coverage, a speedup, or VM cache adoption.
The experimental candidate in #423 has independently retained Linux timing
regressions; coverage success does not waive them.

## Application baselines

Four clean-source Debug collections at `f2bfdff902c49ed35ebb8f84a81f6b657499af0f`
use matching Homebrew Clang/LLVM 23.1.4 on macOS arm64, fixed multiplication split
point 126, profiling OFF and MC/DC instrumentation ON. Both extension and
experimental reuse modes have distinct builds, profiles and output directories.
Assertions remain enabled; debug symbols are omitted to limit artifact size.
All selected tests pass, with the imported suite's documented skips retained.

| Extensions | VM reuse | Broad lines | Broad branches | Broad MC/DC |
| --- | --- | --- | --- | --- |
| ON | ON | 6,277 / 6,462 | 3,012 / 3,341 | 221 / 280 |
| OFF | ON | 6,242 / 6,459 | 2,991 / 3,341 | 218 / 280 |
| ON | OFF | 6,234 / 6,419 | 3,003 / 3,333 | 220 / 280 |
| OFF | OFF | 6,199 / 6,416 | 2,983 / 3,333 | 218 / 280 |

These are pre-follow-up baselines. Their broad scope includes native and
small-trait unit instantiations; the application-only mapping scope has its own
LLVM totals in the summary. Neither scopes nor modes are pooled. Raw MC/DC
records are not summed: reported condition denominators come from LLVM's
summary. Adjusted coverage remains unset, and the remaining feasible coverage
and exception-ledger work is outstanding.

The first extensions-ON/reuse-ON attempt is retained separately as
`failed-ext-on-reuse-on`. It exited with test status 8 because two evidence
verifiers could not find fixtures excluded by the local sparse checkout. The
tracked fixture bytes were materialized before a fresh passing collection.
The failed attempt's profiles and export remain available, but it is not a
passing baseline and is not merged with its successor.

Each baseline archive retains the original collector output: selected test
inventory, test and merge logs, all raw profiles, merged profile, broad/native
LLVM exports, text/HTML views and tool version. Added snapshots retain the exact
bc and unit executables, cache, compilation database, generated threshold,
configure/build logs and matching source. Compiler/runtime/SDK/dependency and
archived document-fixture provisioning remain external; this is not a complete
OS image. Build-time and manual smoke profiles are excluded from these reports.

## Focused test development

The three focused collections come from a separate main-based test topic. They
run only `[divisor-cache]` unit cases and cannot substitute for application-wide
coverage or be used to claim a percentage increase in the baselines above.

| Collection | Source | Cases / assertions | Retained result |
| --- | --- | --- | --- |
| `focused-v1` | `19e22f70` | 21 / 843 | 16/32-bit capacity rejection; 8-bit fixture legitimately admitted a bounded value |
| `focused-v2` | `c7b8f6ac` | 24 / 900 | Capacity rejection, rejected retry and heap-zero rejection at all three widths |
| `focused-v3` | `fa2420cc` | 26 / 916 | Also maps maximal-padding rejection at 16/32 bits |

The refinement compacts the source through a public COW mutation before
alignment. Tests continue to allow bounded admission on standard libraries with
other vector growth policies; the retained LLVM evidence establishes which
outcomes this toolchain actually executed. Heap-backed zero is constructed
through public `mac(0, 0)`, preserving the numeric zero guard. The isolated armed
allocator separately verifies temporary cleanup and allocation-free rejected
retries. No production test accessor or fault-injection hook is introduced.

All three focused attempts retain their source, exact unit executable, raw and
merged profiles, export, test log and branch proof. The later two also retain
cache/compilation database snapshots. Those original snapshots were not captured
for the first focused iteration; their absence remains explicit. The full
ASan/UBSan run for the final test source passed 299 cases / 1,656,255 assertions;
these focused coverage collections are distinct from that sanitizer run.

## Verification and limits

Run from any directory:

```sh
python3 docs/utilities/bc-divisor-mcdc-evidence/verify.py
python3 docs/utilities/bc-divisor-mcdc-evidence/test-verifier.py
```

Archives use solid `tar.xz` compression to avoid repeating LLVM name metadata
across thousands of profiles. Conversion checked every member against its
original hash. Only newly created duplicate ZIP packaging was removed; all
original report directories and evidence bytes remain retained. The collector
and conversion scripts are included for provenance, with original local paths.

Verification reads archives without extraction or execution. It checks every
payload hash, exact binary identity, mode/cache/compiled definitions, profile
inventory, LLVM export/summary consistency, focused test counts and actual
per-instantiation branch outcomes. Git commit and scoped-tree object proofs
bind every source snapshot file to the named immutable commit; Git object names
use SHA-1 while archive integrity uses SHA-256. Sixteen tests reject altered
source or machine code, false activation, missing/duplicate profiles, changed
LLVM totals, lost branch outcomes, promoted incomplete attempts, missing modes
and unsupported full-coverage/adoption claims.

The verifier authenticates retained observations. It does not rerun compilers,
remerge profiles, regenerate LLVM coverage or execute archived tests. Matching
LLVM tools and the retained binaries/profiles are available for independent
replay. Archive members intentionally retain data bytes rather than executable
permission bits; set executable permissions explicitly before any such replay.
