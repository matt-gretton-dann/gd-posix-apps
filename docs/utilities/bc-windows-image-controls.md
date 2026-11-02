# Windows executable-deployment controls

Issue #410 extends #200/#216 control evidence before further compiler and
layout experiments. Earlier native controls sometimes have persistent A/A
bias despite identical executable code. For example, run 41485585402 has
Windows loop/array/comparison A/A ratios around 1.07–1.16; its base/A/A
.text sections match byte-for-byte, with 24 differing .rdata bytes and different
COFF timestamps. This limits attribution. It does not establish an ASLR cause,
and no earlier candidate regression is waived.

## Fixed experiment

Build one immutable accepted source with the existing Release recipe and fixed
multiplication threshold 126. Make six regular executable copies in a common
directory, with same-length neutral names. Every complete executable hash must
match the build recipe, and all six file identities must differ. The tool does
not relink or edit the images. Compiler commands, source revision, build logs,
threshold header, exact deployed bytes and host/image metadata are retained.

Before elapsed sampling, launch every image three times in a predetermined
alternating order. Keep its stdin open while querying the module list, then
supply `1+1` and require normal exit, output `2`, and empty stderr. This permits
module initialization without running an artificial busy loop. Probe retries
are bounded, and one overall child deadline includes the probe phase. Owned
children are killed/reaped on failure. Runtime BC/DC overrides and profiling
output variables are excluded from these diagnostic launches.

The native 64-bit Windows reader uses documented EnumProcessModulesEx,
GetModuleFileNameExW and GetModuleInformation APIs. It matches the main
executable's canonical path instead of assuming that the first module is bc.
It checks bounded module extents, path truncation, nonzero address/image size
and consistency between the module handle and reported base. Initialization
races, access denial, unsupported bitness/APIs and other hosts have explicit
unavailable outcomes. These are separate checked launches: their addresses
are not observations from the later timed children.

Freeze seven full and seven held-out historical classes: startup, loops,
arrays, high/equal comparisons, decimal input and binary input. These are
known diagnostic/regression data, not newly unseen tuning workloads. Measure
all six deployments with nine samples and three warmups, randomized/interleaved
by the existing harness. Image zero is the reference selected before results.
At most one independent-seed confirmation is run for the union of suspicious
nonstartup classes. Symmetric A/A qualification retains faster and slower
persistent bias, inconclusive cases and uncalibrated startup.

The workflow runs three predetermined independent Windows jobs, retaining all
CPU models and outcomes without filtering or favorable retries. The controller
has a 100-minute total command budget inside a 120-minute job. It runs only on
explicit dispatch or relevant PR file changes, with read-only repository access.

## Interpretation and validation

Intervals describe repeated launches of each fixed deployment. They do not
estimate a population of image layouts, establish a source-code cause, or prove
that addresses cause timing differences. File identity, image placement, host
contention and other deployment effects remain possible factors. Results are
not pooled across CPUs, and the artifact always keeps performance_acceptance
false. Invalid output/provenance is a hard failure; missing capabilities are
unavailable rather than a performance pass. The broader #197 goal remains open.

API-boundary and process-lifecycle tests use mocks on all hosts; they do not
claim Windows ABI validation. Native Windows integration separately probes a
real bc copy under a Unicode path. CTest registers both those tests and the
controller's immutable-copy, fixed-corpus, bounded-confirmation and failure
retention tests. Local non-Windows runs explicitly skip the native integration.

Primary references:

- [Module enumeration and initialization limitations](https://learn.microsoft.com/en-us/windows/win32/api/psapi/nf-psapi-enumprocessmodulesex)
- [Unicode module filenames and access rights](https://learn.microsoft.com/en-us/windows/win32/api/psapi/nf-psapi-getmodulefilenameexw)
- [MODULEINFO fields and handle/base equivalence](https://learn.microsoft.com/en-us/windows/win32/api/psapi/ns-psapi-moduleinfo)
- [Windows image-placement context](https://devblogs.microsoft.com/oldnewthing/20170120-00/?p=95225/)
