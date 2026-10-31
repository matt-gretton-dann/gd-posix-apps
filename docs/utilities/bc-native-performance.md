# Native paired bc performance CI

Issue #216 coordinates native regression trends; #200 owns checked execution
and provenance. Performance results start **advisory**. Build failures, missing
provenance, wrong outputs, stderr, crashes and deadlines fail the job. A green
job does not mean every workload is faster, nor that unimplemented math passes.

The `bc native paired trends` workflow uses explicit `ubuntu-24.04` x64,
`windows-2025` x64 and `macos-15` arm64 images. See GitHub's
[runner specifications](https://docs.github.com/en/actions/reference/runners/github-hosted-runners)
and [image provenance](https://github.com/actions/runner-images). Labels select
software images, not identical physical CPUs. Actual image variables, OS,
architecture, native CPU details/features where exposed, compiler identities,
header-library markers, compile/link flags and binary hashes are artifacts.
CPU affinity and frequency are left to the OS; no hardware isolation is claimed.
No package-wide upgrade runs inside these jobs.

## Sources and controls

For a PR, build the exact base SHA and **head SHA**, not the synthetic merge
commit. The workflow passes these as environment data. A PR uses ordinary
unprivileged execution, a read-only token and checkout without persisted Git
credentials. Candidate code receives no repository secrets.

Push-to-main and the weekly scheduled run compare against deliberately accepted
source anchor `2ddf749e8b313f620ad248d680e801c409bbd3f5`. Manual dispatch can supply
an explicit full immutable baseline SHA. The anchor is rebuilt on the same VM;
no earlier run's binary or timing is a baseline. Promote it only with recorded
review of full/held-out and A/A evidence, retaining the previous anchor in Git.
The anchor already supports fixed dispatch; older unsupported revisions fail
rather than silently using independent tuning.

`benchmark-ci.py` builds base, head and a second independent build of base in
fresh trees, using the same compiler, Release flags, extensions ON and fixed
multiplication threshold 126. This threshold is a reproducible experimental
control, not a universal optimum. No PGO training or binary cache is used.
Howard 7.0.3 is built natively from the pinned recipe as competitive reference;
GNU/WSL is not substituted on Windows. Dependencies and actual commands are
recorded by the immutable build recipes. Portable C++ is the only kernel here.

All builds finish before measurements. The runner executes processes serially,
with one checked warm-up and nine paired rounds, seeded order interleaving
inside each workload block. Independent base/base runs precede base/head/Howard
runs. Short and long workloads remain separate; quick correctness dimensions
are not presented as performance evidence. Scheduled/manual runs include all
full and held-out shapes; PR and push runs select representative full shapes.
Held-out dimensions and seed 197201 are reserved from PGO/tuning; the full seed
is 197200. Timing includes process startup; arithmetic component drivers are a
separate experiment.

## Decisions and artifacts

Every output/status/deadline check applies to warm-ups and measurements. Raw
CSV/JSONL, exact source programs/oracles, manifests, recipe logs, environment,
per-workload medians/dispersion and paired bootstrap ratios remain available.
Artifacts have OS/run/attempt-specific names and 30-day retention using
[upload-artifact](https://github.com/actions/upload-artifact). Download evidence
before retention expires when promoting a source anchor. No cross-CPU absolute
aggregate is computed. Compare longer-term trends only within matching
OS/compiler/architecture/CPU/ISA/image strata.

`decisions.json` and the readable job summary use these advisory labels:

- **pass**: the measured interval's upper bound is at most 1.05; this means no
  material slowdown signal in this sample, not a universal performance claim.
- **confirmed regression**: the interval is wholly above 1.05 in both the first
  batch and one bounded independently seeded confirmation batch.
- **inconclusive-noisy**: other intervals, unconfirmed signals, insufficient
  pairs, or startup (whose threshold is not yet calibrated).

The same diagnostic labels apply to independent A/A builds. Report false alarms
and the worst losing classes rather than suppressing them in an average. The
5% boundary is provisional, not a mandatory merge gate. Percentile intervals
resample paired round ratios; they do not model arbitrary host autocorrelation
or adjust multiple-workload false alarms. Repeated native A/A artifacts must
inform that policy before any mandatory performance threshold is enabled.
There is at most one confirmation batch per preset/comparison; no rerunning
until favorable. Superseded PR jobs cancel, each job has a 90-minute cap, and
individual child/recipe commands have bounded deadlines.

## Remaining scope

CPU/RSS/allocation diagnostics belong to the separate diagnostic collectors,
not instrumented timing binaries. Native timing artifacts do not claim complete
heap or hardware counter coverage. POSIX math function/regime/scale classes,
cold library loading and the identical-source Howard comparison are pending
#217 and must be added before closing #216. Cross-run calibration and all three
native artifact results must also be reviewed. A dedicated stable runner may be
needed to distinguish small changes reliably.
