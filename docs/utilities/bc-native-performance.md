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

Build/test, CodeQL and paired trends check pull requests targeting `main` or a
`codex/**` topic branch. This supports stacked performance changes: a storage
PR can compare against its workspace dependency without attributing that
dependency's gains to storage. CodeQL also analyzes topic pushes to index those
base branches. Build/test and paired trends retain main-only push triggers;
topic changes use their PR checks. Existing job permissions are unchanged.

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
full and held-out shapes; PR and push runs select at least one full-size
representative of every corpus group, including conversion, modulo and frontend
work. The selection follows the corpus automatically and retains extra large,
imbalanced, square and comparison shapes. An inventory test checks both presets
so new workload groups cannot silently escape the native PR subset.
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

## Nondecimal output coverage

The corpus adds separate `binary_output` and `octal_output` groups, each printing
a dense 1000-decimal-digit integer 300 times in full mode. Quick and held-out
presets vary dimensions/iterations normally. Python integer formatting supplies
expected digits; an independent test decodes each oracle back to the original
source integer. Both groups enter every representative PR subset through the
same per-group selection rule. This makes native runs exercise whole-number
radix conversion, rather than inferring it from decimal output or local-only
component timings. Output capture, continuation joining, status, stderr and
deadline checks remain active.

These cases are appended after the existing corpus; earlier programs, oracles
and seeded operands retain their identities. Full corpus size grows from 40 to
42. Historical reports keep their original corpus sizes and source revisions.
Binary/octal digits fit the existing numeric-output validator; hexadecimal
letters are covered by all-platform arithmetic tests and separate component
oracles, and are not silently added to that validator's contract.

## Preserve measured machine code

Issue #397 adds `evidence/codegen/` to the arithmetic native workflow after all
elapsed children finish, including bounded confirmations. It copies the exact
base, candidate and independently built A/A executables and verifies every copy
against the existing recipe's SHA-256 before and after inspection. No timing
compiler/linker flags change, and the collector never executes the copied bc.
Partial build failures retain only the completed roles. A missing or changed
declared executable is a hard integrity failure, with a manifest retained.

Native tools are discovered rather than required: Windows uses `dumpbin`, macOS
uses `otool`, and Linux uses `llvm-objdump` or `objdump`. Header and disassembly
commands run after timing with bounded process-tree cleanup. The manifest records
the tool hash, commands, deadlines, output hashes and byte counts. Missing,
failing or timed-out tools are explicitly unavailable; the exact executable and
its full-file identity remain the generic fallback.

A bounded, read-only PE section decoder also records code/executable-section
bytes, RVAs, sizes and hashes on Windows. Its layout follows Microsoft's
[PE format](https://learn.microsoft.com/windows/win32/debug/pe-format). It separates
the raw COFF timestamp field and non-code data from code payload identity.
Payload hashes include encoded addresses and file padding; they are not
relocation-normalized, and virtual zero fill is not stored payload. Other image
formats retain native-tool output and the full-file hash without claiming PE
section coverage. Truncated or unsupported images have no partial code identity.

These diagnostics help investigate code layout, compiler choices and build
reproducibility. Equal code bytes do not establish equal behavior: data,
relocations, runtime placement and host contention still matter. Neither a green
advisory job nor matching source/code hashes waive a confirmed timing regression.
Original rejected batches remain separate evidence. Memory/CPU diagnostics and
paired elapsed measurements retain their existing scopes.

Draft or ready PRs whose head branch starts `codex/bc-native-broad/` run the
existing broad arithmetic full/held-out plan, as scheduled/manual runs do.
Ordinary PRs retain their bounded subset. This permits an explicit diagnostic
checkpoint without a manual-dispatch connector or hard-coded runtime pins in
an otherwise mergeable workflow. The PR base/head remain the immutable runtime
inputs; recipes, raw samples and post-sampling executable inspection retain
those identities. The broad branch name does not change sample counts, output
checks, tuning policy or bounded-confirmation rules.

## Symmetric independent-build controls

A/A has no expected winning direction. Its complete interval must lie inside
[0.95,1.05] to pass. An interval wholly below 0.95 or above 1.05 selects at most
one independent-seed confirmation. Persistent same-direction evidence is
`confirmed A/A bias`; missing, overlapping, invalid or reversing controls remain
inconclusive. Startup remains uncalibrated. Arithmetic and math orchestration
share this policy, whose exact source SHA-256 is recorded with decisions.

Arithmetic paired rows retain the raw candidate/base regression status and
include the matching `aa_control_status` separately, also shown in the job
summary. A noisy or biased control limits source attribution and competitive
claims; it does not erase a raw adverse signal. Runtime C++, sample counts,
workloads, tuning and the bounded-repeat rule are unchanged by this correction.

Earlier arithmetic reports used the one-sided candidate/base rule for A/A and
could incorrectly label a fast or lower-band-overlapping control as pass. Keep
those original artifacts unchanged. Requalification must identify the policy
and original evidence, retaining absent confirmations as unavailable rather than
manufacturing a new pass. In particular, #400's held-loop control [.937,1.017]
is inconclusive under the symmetric policy. Its separate full-loop control lies
inside the band and does not waive the retained full-loop regression.

## Stable internal diagnostic source names

The VM's internal error checks retain `src/bc/vm.cc` as a repository-relative
file name, together with the actual function and source line. Independent
checkout roots therefore do not change this diagnostic string. The bounded
malformed-stream tests require the canonical file, a positive line and the
existing rejection, identically in checked and verified modes. This is portable
C++ and needs no compiler-specific path-remapping switch. Ordinary language
errors and all verifier/runtime checks keep their existing contracts.

Absolute source roots previously differed between baseline, candidate and A/A
build roles. Removing that one retained string variation makes this diagnostic
more consistent; it does not establish full binary reproducibility or prove
that path length caused timing or code-layout differences. Object metadata,
other libraries, toolchains and native addresses can still differ. Keep the
actual executable hashes and code inspections, and retain all control outcomes.

Branches under `codex/bc-native-broad/` explicitly select the existing full and
held-out native arithmetic cohorts on pull requests. Ordinary topic branches
retain the smaller representative cohort; schedule/manual runs remain broad.
The branch convention changes corpus selection, not measured treatments or
acceptance rules. Every first/confirmation loss and inconclusive control remains
an adoption gate. No timing advantage is claimed by diagnostic normalization.
