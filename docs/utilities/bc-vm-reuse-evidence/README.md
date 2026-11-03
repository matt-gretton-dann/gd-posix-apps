# Experimental VM divisor reuse costs

Refs [#420](https://github.com/matt-gretton-dann/gd-posix-apps/issues/420),
[#216](https://github.com/matt-gretton-dann/gd-posix-apps/issues/216) and
[#197](https://github.com/matt-gretton-dann/gd-posix-apps/issues/197).

The [archive](rss-v1.zip) and [summary](rss-v1.json) retain 156 checked local
processes comparing prepared-divisor VM reuse OFF and ON at the same immutable
source revision, `d89d17c51ea3e9792df80256ee9553a192c54760` (PR #423).
This is a mode-cost diagnostic. Native main-baseline timing and the adoption
decision remain separate requirements.

Both binaries use Apple Clang 21 Release, extensions OFF, fixed multiplication
split point 126 and the default automatic IPO policy. Profiling and coverage
are OFF. The archive includes both exact executables, CMake caches, compiler
identities, compilation databases, threshold/IPO reports, build logs, semantic
guard results, source snapshot, collector and post-measurement native inspections.
Compiled VM and CLI definitions verify each requested activation mode. The
collector uses existing local source/build directories; it does not claim to
exercise the immutable builder's fresh-clone entry point.

There are 26 fixtures across full and held-out corpora. Each has three fixed
samples per mode, alternating mode order between trials. They include stable
multi-limb quotients/remainders, fractional remainders, changing divisors, fresh
temporaries, periodic changes after warm-up and cold first requests. Added
controls print both the last result and the checksum of all results. All owned
builds and semantic guards finished before resource sampling began. No sample
was dropped or rerun based on its result.

`/usr/bin/time -l` reports maximum resident set size for the whole process,
including startup, parsing and output. Its raw duration fields remain in the
resource logs and are not added to native paired timing results. The host was
unisolated, and three observations per role do not establish a general memory
improvement. These RSS observations and allocator counters from other experiments
have different scopes and must not be pooled.

| Cost | Reuse OFF | Reuse ON | ON minus OFF |
| --- | ---: | ---: | ---: |
| Executable file bytes | 515,680 | 534,272 | +18,592 |
| Mach-O `__TEXT,__text` bytes | 268,996 | 275,780 | +6,784 |

Across the 26 fixtures, per-case median peak-RSS differences range from -49,152
to zero bytes. Every observation is retained in the archive, and every per-case
median is listed in the summary. This does not establish universal RSS reduction,
a speedup, or a production default change. Code growth remains a cost of the
experimental mode even when an individual RSS median is lower.

Run:

```sh
python3 docs/utilities/bc-vm-reuse-evidence/verify.py
python3 docs/utilities/bc-vm-reuse-evidence/test-verifier.py
```

Verification checks the published ZIP identity, all 528 payload hashes, both
activation modes, exact binaries/inspection logs, complete full/held-out class
and sample grids, independent Fraction quotient/remainder oracles for every
request, raw numeric outputs, resource values and recomputed medians/code sizes.
It reads archived bytes without extracting or executing source or binaries.
Eleven tests cover duplicate/missing samples, forged matching outputs/oracles,
an incorrect intermediate checksum, false activation, changed machine code,
misreported RSS, removal of owner changes, false adoption and replacement of
code growth with a favourable summary. Passing verification reproduces the
retained accounting; it does not rerun the experiment or establish performance
acceptance.
