# Offline control precision diagnostics

Issue #200 requires reproducible controls before performance adoption. The
`src/bc/scripts/audit-bc-controls.py` tool reads an existing provider ZIP; it
does not execute its binaries, rebuild them, collect timings or grant acceptance.
It supports ordinary arithmetic and math A/A stages collected by the matching
`bench-bc.py` harness. Startup-null calibration remains a separate study under
[the startup protocol](bc-startup-calibration.md).

For example, audit a retained arithmetic archive with the independently recorded
provider SHA-256 and the immutable source revision of its **control roles**:

```sh
python3 src/bc/scripts/audit-bc-controls.py \
  --archive retained.zip --sha256 PROVIDER_SHA256 \
  --source CONTROL_SOURCE_REVISION --prefix portable-results/evidence/ \
  --stage full-aa --stage holdout-aa --control-role aa \
  --output new-control-audit.json
```

Both digest and source arguments are mandatory. Choose a new output path; the
tool refuses to overwrite an existing observation. Normal trend uploads flatten
the evidence directory into the ZIP root: use `--prefix .` for those archives.
An empty prefix is also accepted. This root-prefix handling does not allow empty,
absolute or parent-traversing file members. Nested archives retain their explicit
prefix, as in the example above. For math, use the stage's
`gd-aa` control role and its independently built candidate source. Arithmetic
A/A usually rebuilds the base source. The control revision can therefore differ
from the candidate/cohort revision. The stage manifest supplies the reference
role. Exactly two roles must be present, both declaring the requested source.
Source revision and corpus identities come from the explicit inputs and archive.
The auditor does not substitute fixed cohort counts or measurement durations
for those declared by the stage manifest.

The auditor verifies archive digest/CRC, unique members, retained binary and
external-input bytes, stage/binary metadata and argv agreement, workload and
oracle hashes, uninstrumented complete role provenance, declared sample counts,
fixed-seed sample order and stable checked-output identities. It then replays the
complete summary using the matching local harness. The harness hash must match
the collector's native LF or Windows CRLF checkout bytes; a different harness
revision is rejected. The only rounding tolerance is 1e-15 absolute for the
non-decision lag-one autocorrelation diagnostic. Paired intervals must match
exactly. Numeric stdout is not independently replayed by this tool.

Each requested stage retains its own identities, platform, declared parameters,
pair count, original interval, interval width, role duration medians and counts
by duration band. Bands below 10 ms, 10–50 ms, 50–250 ms and at least 250 ms are
descriptive. Interval locations relative to [0.95, 1.05] are descriptions, not
replacement policy labels. Confirmation stages can be requested explicitly;
they stay separate and are never substituted for an original stage. This tool
does not establish that all stages required by a controller were collected.

Full-file byte identity is separate from timing precision: identical binaries
can still have different observed process latencies. Equal source or file length
does not prove equal instructions. PE code payload hashes include encoded
addresses and padding without relocation/address normalization. Other formats
retain full-file identity and an explicit unavailable PE description.

The report preserves complete declared build metadata. Configuration equality
is unknown when either configuration is absent; even equality does not prove
compiler/cache/linkage or recipe equivalence independently. Different declared
configurations remain visible and cannot qualify a control. The report always
sets performance acceptance and merge eligibility false. It never pools effects
across stages/hosts, changes historical decisions, estimates measured power,
chooses a future sample size or waives an inconclusive result. Full correctness,
controller, startup, resource and [acceptance review](bc-performance-acceptance.md)
remain necessary.

## Motivation and next measurement

The complete rejected #668 cohort retained seven confirmed Windows ON math
regressions, including six full cases and one held-out case, without the Howard
margin. Passing correctness/collection checks did not qualify that candidate.
Its arithmetic controls also showed uncertainty with byte-identical Linux and
macOS binaries; Windows independently built images differed. Its macOS OFF and
Windows ON startup studies remained inconclusive. All original evidence and
labels remain binding to that source, which must not be restarted to seek a
favorable outcome.

Use this diagnostic to make such limitations reviewable before declaring a
separate accepted-main null-method investigation. Fix that investigation's source,
binary deployment, host/cohort requirements, corpus, repetition/duration rules,
budgets and analysis before collection. Distinguish aliases of one measured
binary from independent same-source builds and retain every outcome. Longer
child durations alone are not established to resolve all observed uncertainty.
Windows CPU/code-layout attribution remains separate from these descriptive
controls; neither counters nor executable-size changes alone establish a cause.
