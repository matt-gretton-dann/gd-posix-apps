# Startup calibration

Issue #659 follows #200 and the startup requirement retained by #653. The
ordinary trend collectors still label their nine-pair startup observations
uncalibrated. This separate study does not rewrite those observations or apply
the Howard-margin exception to startup.

Use the existing checked `startup` program, which includes process launch,
minimal bc parsing/evaluation/output and shutdown. It is not a loader-only
measurement. The base and independent A/A executables must have the same source,
compiler, explicit flags and recorded execution policies. A self alias uses the
exact base executable. All binaries must be ordinary and are rehashed before
each child. Every child output is checked against the existing oracle.

The fixed protocol is 60 null blocks, each with initial and confirmation stages.
Each stage has five warmups and 151 interleaved rounds of base/self/A/A, followed
by the existing 2,000-resample IID/block union intervals. Seed 200659 and fixed
per-stage increments define ordering and resampling. Retain every checked raw
sample, including warmups, and all source/binary/configuration/timer identities.
Process timeouts and a total collection budget are explicit. On failure, raw
rows are flushed and the manifest remains incomplete.

A persistent same-direction interval difference from 1.0 in both stages counts
as a null confirmation, in either direction. A qualified empirical study needs
no such confirmations, at least 54/60 blocks with all self/build intervals inside
[0.95,1.05], and at least 54/60 blocks detecting a separately labelled 5% scaling
of their intervals. The scaled-data probe changes no raw measurements and is
not measured sensitivity or a power guarantee. The measured clock must be
monotonic/nonadjustable, with resolution at most one-thousandth of each block's
minimum base median. Missing, nonfinite, mismatched or incomplete evidence stays
inconclusive.

These are empirical criteria on one host and source. Serial correlation,
nonstationarity and finite study size remain limitations. They do not establish
a universal false-positive rate. Do not reuse a qualified pilot across machines
or promote it into evidence about another candidate. A future candidate needs
its own study and controls in the same invocation and environment.

Two separate candidate stages are always retained. Only a qualified study with
passing, unbiased A/A controls can assess them: positive significant intervals
in both stages confirm a startup regression; conflicting positive/nonpositive
stages are inconclusive. With neither stage showing a positive significant
difference, report that none was observed, not equivalence or a speedup.
Reports remain advisory and never grant merge eligibility.

The first immutable pilot builds accepted runtime 1881c6ae twice and uses the
base executable also as its candidate. One Linux/macOS/Windows job each studies
this null comparison, with a 2,400-second collection budget, five-second child
deadlines and 90-minute job limit. Build recipes and raw/provenance archives are
retained. No local production timing run is used. An inconclusive pilot remains
inconclusive; never restart it to seek a favorable outcome.
