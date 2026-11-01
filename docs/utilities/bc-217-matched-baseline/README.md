# Matched portable-math baseline evidence

Refs #217, #216, #200 and #197. These are pre-optimization observations, with
per-class competitive misses retained. They do not complete the speed goal.
No elapsed samples from different hosts or configurations are pooled.

## Local frozen source

`local-frozen7ed.zip` preserves all 120 full/held-out programs and enclosures,
calibration trials, exact replay manifests, independent-build A/A controls,
paired samples, build identities, host metadata and command logs. Source is
7ed25fb1eea0a46a056480c9432c276b88e9bccf; the two GD recipes are retained under
`recipes/`, with pinned Howard source c8dabac65d39fd527a9264a5e505a99b1d8df3d8.
All 16,830 observations pass numerical/state checks, including calibration.
Each comparison uses fifteen measurement rounds after three warmups. The full
A/A report has 44 passes and 16 inconclusive observations, with no outside-band
control; neither preset required confirmation. Full repeated cases meet the
duration target, while two held-out repeated cases have explicit shortfalls.
Only four repeated classes per preset meet the nominal competitive observation.
These counts are an inventory, not an aggregate timing result.

All builds/tests/profiles finished before this elapsed experiment. The agent
ran no concurrent local builds/tests/resource diagnostics during timing. Git
and test-fixture edits left the executed benchmark/checker source unchanged.
The host used normal OS scheduling without CPU isolation; unrelated host
activity, frequency changes and cold-process variability remain limitations.
The original local runner and exact source snapshots are included. Original
absolute paths identify the actual run; rebuilding elsewhere needs new argv
paths and matching observed executable hashes rather than reusing stale recipes.

## Native broad and subset observations

`native/` preserves the checksum-verified GitHub artifacts and structural audit
reports for cb0383da32e08339b4f3e993b2caadb179ad6e1e. Its production src tree is
identical to 7c30eb3 (e7627bb6e9abc2329cbbb54ba404136dc70b453b); the new commit
only adds an opt-in workflow branch prefix and its guide.

Broad push run 41354033150 covers all 120 classes on each of Linux, Windows and
macOS: 17,085 / 16,455 / 16,626 checked observations respectively, or 50,166
including calibration. PR subset run 41354092859 records another 8,754 checked
observations across twenty classes per OS. The supplied audit verifies exact
program/oracle hashes, common frozen counts, source/binary/library identities,
fifteen paired rounds, three warmups and every checked-result flag. Actual
numerical checks ran in the identified harness on each native runner.

All controls remain pass or inconclusive. Competitive misses and measured
shortfalls are present in every per-class report; source and host identities
remain separate. This protocol compares GD with pinned Howard standard and
identical ordinary-source roles plus independent GD A/A builds. It has no
separate old-GD mathematical baseline; candidate/base regression reporting for
math remains follow-up work in #216. The arithmetic workflow already reports
same-job base/candidate ratios. No mandatory elapsed-time gate is enabled.

Windows source-file bytes use CRLF. Each job verifies its actual declared raw
file hash; normalized LF content matches the other hosts. Mechanical embedding
and the source identity test normalize text newlines. Both identities and CRLF
counts remain in the audit, rather than relabeling all raw inputs with one hash.

## Earlier native checkpoint

`native-checkpoint/` retains source 7c30eb3 mathematical run 41352157336 and
arithmetic run 41352157539. All checks on that head passed. Mathematical artifacts
contain 8,748 checked observations including calibration; arithmetic artifacts
contain 6,000 with no confirmed regression. They are separate historical runs,
with their own VM noise and per-class conclusions, not substitutes for cb or
local observations. The parent source's benchmark/compiler validation archive
is maintained separately in PR #359.

## Reproduction and preservation

The archived scripts identify their original input paths. To repeat their
structural ZIP audit, adjust that input directory or place the ZIPs at the
recorded paths; no interpreter timing is rerun by those audits. The local runner
shows the preparation-only calibration and frozen replay commands. Rebuild
immutable sources using the included builder snapshots and fixed split 126,
keep strict executable/file identities, and rerun independent enclosure checks.
The optional FLINT generator remains a test-only dependency; runtime code is
ordinary bc source and uses no host mathematical callback.

`source-inventory.json` binds exact script snapshots to source commits. Raw ZIPs,
JSON, logs and payloads are preserved byte-for-byte under -text attributes.
`sha256.json` covers every payload except itself. The repository's
verify-bc-evidence-checkout.py validates these bytes and Git Windows conversion.
