# Cancel common decimal factors before division

Refs #368, #200, #217 and #197. This is a general integer-coefficient
transformation, with the existing portable division kernel retained. It does
not recognize library text or mathematical function names.

For signed decimal coefficients A*10^-sA and B*10^-sB at quotient scale T, the
magnitude is floor(A*10^(T+sB-sA)/B). Form the positive or negative exponent by
ordered differences, rather than first padding both operands to a common scale.
This avoids common powers of ten without overflowing a scale sum. Sign, result
scale, operand isolation, truncation, zero-divisor handling and Algorithm D stay
unchanged. The negative-exponent path preserves initialized zero storage needed
by the existing coefficient comparison contract.

## Source identities and test-first evidence

The immutable release baseline is 96d900b8e66c299d35e7a93c63b7fb5ef5131654.
The measured candidate executable is 7d538de7813d024995465fc3b98334851a7fbcb3.
Protocol/corpus revision fe61ffb515b0055e2a6c371abb51a6f287621873 adds six
scaled-numerator classes and tests/documentation, with no compiled production
change after 7d. Do not relabel the measured executable as fe61 or the later
archive commit. Both recipes use Apple Clang 21, matching Release flags and
fixed split point 126; actual compiler, library, flags and executable hashes are
preserved in the recipes and manifests.

The test-first profiling regression on edd4292 failed native Linux ON CI run
41354530736/job 124991653591 in all twelve intended sign/scale subcases after
checking exact quotient output. Its failing log excerpt is retained. The
unreduced native-rational matrix covers 45,000 cases across 8/16/32-bit models,
including signs, differing scales, target scales, zero and operand/alias safety.
Existing zero-divisor subprocess tests remain separate.

Candidate Debug extensions OFF ASan/UBSan validation passes all 220 bc unit
cases (876,309 assertions) and eight independent numeric-oracle groups.
The separate profiling build passes seventeen profiling groups. The expanded
56-case harness and native policy checks pass Python 3.9 and 3.15. The new core
also passes all 476 certified ordinary-source library cases, nondecimal loader
checks and silent-clamping checks in Release and ASan/UBSan. These latter tests
load the unchanged library file as ordinary input; they do not validate a new
embedded loader. Leak detection is unavailable on this macOS runtime and was
disabled explicitly; no leak-sanitizer result is claimed.

## Paired uninstrumented release observations

`paired-release.zip` preserves 12,528 checked observations across all 232
full/held-out arithmetic and mathematical classes: 56 + 56 + 60 + 60. Each has
fifteen paired measurement rounds after three warmups, with balanced seeded
serial ordering and exact input/oracle identities. The audit regenerates the
arithmetic programs and rechecks the exact frozen mathematical replay inputs.
No substantive class has an interval wholly above candidate/base 1.05, so no
bounded confirmation was triggered. All one-shot latency rows remain visible.

| New scaled numerator classes | Candidate/base median range |
|---|---|
| Full, six classes | 0.0334–0.5313 |
| Held-out, six classes | 0.0248–0.5400 |

The thirty repeated mathematical classes per preset have candidate/base medians
0.1133–1.0033 (full) and 0.1146–1.0100 (held-out). These ranges describe the
inventory; the complete per-class intervals and losing competitive classes are
in `audit.json` and the raw summaries. They are not an aggregate speed result.

Both GD binaries load the identical ordinary library file from source 7ed25fb,
SHA-256 e4095342fc587ec43f964f9a6314b4a68a1762cac15b2d0a3016f34fd4565de7,
without -l. Howard uses its standard -l library from pinned source c8dabac.
This isolates the generic divider change and is not an embedded-library cold
startup comparison. Independent mathematical enclosures and caller-state checks
apply to every invocation; Howard output is not the accuracy oracle.

Mathematical invocation counts were frozen before this core change. Only 19/30
full and 23/30 held-out repeated classes meet the measured duration target in
every role. Shortfalls remain explicit. Candidate/Howard intervals are retained,
with many competitive misses; this patch does not complete #217's speed goal.
No new local A/A batch was added to this frozen-core comparison. Earlier frozen
library controls and the new native independent-build controls remain separate.
Normal OS scheduling, unrelated host activity and frequency changes remain
limitations. No builds, tests, profiling, resource diagnostics or FLINT generation
ran concurrently with these elapsed measurements.

## Separate diagnostics

`software-counters.zip` has 132 checked instrumented invocations across sixty
frozen mathematical and six arithmetic classes. All corresponding baseline and
candidate output bytes are identical. The equal-scale dense division class
reduces 5,000 requests from 1,115,000/560,000 total dividend/divisor limbs to
560,000/5,000. Its tracked allocation calls fall from 40,017 to 10,017 and
requested bytes from 20,893,254 to 2,493,254. The request counter site now observes
coefficients after common-factor cancellation. These are software event and
allocator-request counts, not hardware operations, total process malloc or RSS.

`native-resources.zip` separately preserves 108 output-checked native resource
invocations: eighteen classes, two roles and three alternating trials. They
include high-precision one-shot math, the six division classes and two one-op
100,000-fractional-digit memory stressors. Darwin time-l RSS is in bytes; raw
instruction/cycle observations remain optional native diagnostics. All trials
remain visible, including unchanged peak storage and process-memory variation.
Resource-wrapper elapsed values are not substitutes for paired release times.
All diagnostic collection finished before the elapsed experiment began.

## Native CI checkpoint

`native/` preserves checksum-verified Linux, macOS and Windows artifacts for
candidate fe61 versus actual base 96d900b, run 41358302878. Each OS has 2,100
checked observations (6,300 total), with no confirmed regression. The two new
representative equal-scale classes have median ratios 0.042/0.043 on Linux,
0.038/0.045 on macOS and 0.064/0.062 on Windows; all interval upper bounds are
below 0.067. Each artifact retains its own host, compiler and per-class results.
These hosted samples are not pooled with local elapsed measurements. Build/test
and CodeQL checks also pass that source; math CI explicitly records unavailable
embedded library source, with no acceptance claim for that absent feature.

## Preservation and reproduction

Payloads are byte-preserved under -text attributes, with SHA-256 inventory and
ZIP integrity checks. The repository evidence verifier checks Windows checkout
conversion as well. Source snapshots bind the numerical core and harness to
immutable revisions. Collector, runner and audit scripts retain the exact
original command/data paths. Rebuilding elsewhere requires updated argv paths
and newly observed executable identities, not reuse of stale machine paths.
The accepted frozen library baseline is preserved separately in
`bc-217-matched-baseline`; compiler coverage and final roadmap acceptance remain
separate work. No denominator adjustment or inferred coverage is supplied here.
