# Startup study for the bounded logarithm

Run the existing startup calibration once on Linux, Windows, and macOS
for #733 at `3711f6b0` against `4479b0ef`. This study reuses the
immutable #728 collector at `4e749f37` and records its source hash.
The wrapper pins the two runtime revisions and exposes only startup.
The runner retains both independent base builds, the candidate, and
every available raw observation.

Use the established 60 blocks, 151 pairs per stage, and fixed null and
candidate decisions. The collection budget is 2,400 seconds with a
five-second child bound. The hosted job limit is 30 minutes. This
study covers startup only. Other arithmetic, math and resource gates
still apply.

From the repository root, invoke `collect.py --suite startup --output
DIRECTORY --plan-only` with a new output directory to inspect the
frozen recipe without building or measuring an executable.
