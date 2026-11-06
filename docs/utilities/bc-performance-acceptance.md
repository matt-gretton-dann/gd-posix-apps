# bc performance acceptance

The user approved this policy on 6 November 2026 in #652, updating #197.
It applies to current reviews and future changes. Historical source, raw
measurements, regression labels and control classifications remain unchanged.

For each agreed substantive workload, retain the existing platform cap:
Linux/Windows allow at most 5% additional elapsed time; macOS retains its
zero-regression requirement. A candidate may exceed that cap when its paired
candidate/Howard **95% upper confidence bound is <=0.90**, retaining a proven
10% elapsed-time margin. The same source, host, workload and checked build roles
must supply the evidence; do not combine ratios from different machines.

The exception requires passing A/A controls, valid finite ordered intervals,
the declared paired sample counts and all required measurement-duration checks.
Unavailable or biased controls cannot be waived. Startup and one-shot latency
retain their separate requirements. For native math, the competitive comparator
is the existing primary `howard-standard` role; never select whichever Howard
library happens to give a favorable ratio. Required GD candidate/base and primary
Howard durations must pass. Portable-library controls remain separately recorded.

Otherwise the original caps apply. A missing competitive interval supplies no
exception. An allowance on one row does not cancel a loss or inconclusive result
on another. The change must deliver a worthwhile measured gain elsewhere, and
the complete cohort, correctness, startup, memory, allocation, code-size and final
integration reviews still apply. No aggregate score hides losing classes.

Collectors preserve `decisions.json` observations and write a separate
`acceptance.json` with a policy identifier/hash and per-row assessment. It is
advisory and explicitly does not declare merge eligibility. The shared helper
can also assess retained collector rows without rewriting their archives or
restarting a frozen cohort. Bind such derivative reviews to the source,
original archive digest and policy hash, and preserve the original labels.

Tests cover the exact competitive boundary, existing platform caps, failed
controls, missing/nonfinite or unmatched statistics, excluded observations,
fixed-sample duration obligations, both collector integrations, and independent
mixed eligible/ineligible rows. A manual decision must still establish that the
gain elsewhere is worthwhile; this is deliberately not inferred from a row flag.
