# Final conversion acceptance

PR #339 accepted exact source ffcda6a202c60c55cf45bd5c4f6c72654288e89b against
main dd8c60c274511e26ed453eb1d8e175936ea6f576. Native run 41297754143 has 1650
checked rows each on Linux/macOS and 1680 on Windows, including bounded
confirmation. All 4980 rows pass exact output/process checks; no confirmed
regressions remain. All fourteen head checks passed, including both CodeQL
workflows and the aggregate alert check. Merge d493faf was followed immediately
by fast-forwarding the root checkout to origin/main.

Original digest-verified ZIPs preserve every actual source/build identity,
checked row, advisory interval and noisy/inconclusive outcome. Decimal-output
candidate/base is Linux 0.756 [0.7167,0.8143], macOS 0.687 [0.5513,0.7899],
Windows 0.623 [0.5808,0.6601]. Large binary/hex input classes also pass. These
are native end-to-end observations, distinct from local component timings.

The locally measured runtime commit 68c22f9 and native head ffcda6a have an
identical src/bc source tree; subsequent changes only archive evidence. Both
actual identities and their binary hashes remain recorded, without relabeling.
Earlier held candidates, losing decimal microcases, inconclusive intervals and
rejected DC output retain their own sources and measurements.

Refs #206 and #197. This completes the staged conversion issue: decimal chunk
input and arithmetic scalar paths (#322), nondecimal input groups (#332),
bounded writes (#334), grouped nondecimal output (#337), measured large input
DC with generic fallbacks (#338), and fixed-width formatting/indexed decoding
(#339). No particular speedup for every possible input or platform is claimed.
The overall competitive goal, remaining roadmap issues and final coverage rerun
remain tracked by #197.
