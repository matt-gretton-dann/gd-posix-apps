# Accepted grouped output candidate

PR #337 accepted exact source dac2d221c2aba6426768b4223307e86f3f2ebb8e
against main 422cbe90c6b4ae0bc7f4d15b43858e30165ed4c3. Native run
41293118564 has 1550 checked samples per OS and no confirmed regressions.
ZIP SHA-256 values, identities and the formerly failing division workload are
recorded in `audit.json`; complete manifests, recipes, rows and advisory tables
remain inside the original ZIPs. All 14 head checks passed before merge.

Linux divide_scale100_x100000 is 1.028 [1.0195,1.0473] and passes the policy.
macOS is inconclusive-noisy; Windows passes. These do not establish universal
neutrality. Earlier held candidates and their confirmed Linux regression remain
in the typed-bound-native directory. The GCC-only output wrapper is part of the
changed accepted candidate; it does not prove a general layout mechanism.

Refs #206 and #197. Merged as 9669ef7; the root checkout was immediately
fast-forwarded to origin/main. Remaining conversion work keeps #206 open.
