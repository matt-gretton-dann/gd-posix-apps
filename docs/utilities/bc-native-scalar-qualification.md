# Native scalar activation qualification

This #207/#200/#197 candidate changes the native scalar option's source
default from OFF to ON, comparing against accepted opt-in-backend main
`a039b667f527d6e97080130d85d6ac1919029de1`. Before publication, that merge's
own CI and retained evidence must complete review. No cohort is declared by
this draft document alone: record the immutable source, workflow, protocol,
library and acceptance-helper hashes in #207 before pushing the final source.

The candidate requests bounded native lowering on supported Apple Arm64 and
x86-64 POSIX/Windows targets. Unsupported architectures, streams and operations
retain generic execution. See `bc-native-scalar.md` for the eligibility proof,
private scratch, checked arithmetic, fuel, interrupt, executable-page and
unwind boundaries. The candidate makes no function-name substitutions and uses
the unchanged ordinary bc math library. This is restricted top-level scalar
lowering, not full-function JIT or an established performance improvement.

The intended large gain is removal of interpreted per-iteration work for
eligible scalar loops. Costs include eligibility scans, machine-code buffers,
page protection and instruction-cache work, and larger binaries. Fuel fallback
can repeat private work before generic execution. Measure all classes; neither
one large loop win nor a historical Apple result proves broad acceptance.
Keep the frozen private-scalar #680 experiment separate and preserve its entire
original cohort. Do not transfer its controls, gains or resources here.

## Preflight and actual activation

The opt-in backend's exact source `561dbd80` passed 28 hosted jobs before #681
merged, including three OSes in ordinary/profile modes. Native allocation
faults, overflow, fuel, interruptions, checked-interpreter agreement and Windows
virtual unwinding have source-bound guards. These are predecessor correctness
results. The final activation source must pass its own complete hosted checks.

Build metadata distinguishes the configured option from actual VM compilation,
the selected backend and shared proof. An unsupported-target fallback, ignored
cache key, zero presence definition or inconsistent native project cannot
qualify as enabled native execution. `--require-candidate-native` rejects such
ordinary and diagnostic candidate builds before any elapsed sampling. Source
defaults select modes: candidate ON, accepted base OFF. Preserve compilation
databases or native VS Release projects and cache bytes under `native-policy/`.

Thirty-seven builder, twenty-two resource, sixteen arithmetic-controller and
twenty-five math-controller fixtures pass locally. These include hard rejection
of a disabled candidate before sampling and preservation of every outcome.
Candidate ordinary and profile modes must match. Metadata proves compilation,
not dynamic entry for every workload. The arithmetic resource loop's profile
must show a native attempt/completion; base counters predating those events
remain missing rather than invented zeros. Discard all instrumented durations.

The local default-enabled Apple Arm64 build passes 388,354 assertions in
58 VM/native cases, twelve focused CTests, 38 CLI checks (one expected skip),
27 VM CLI checks and PTY interrupt/recovery guards. This is correctness evidence
only; no local production elapsed measurements were run.

## Fixed nine-job cohort

Run `ubuntu-24.04`, `macos-15` and `windows-2025`, each with arithmetic,
full-math and held-out-math jobs. Keep maintained extension, typed, image, IPO
and arithmetic settings. The sole production change from the accepted backend
is the native option's default. Ordinary arithmetic A/A independently rebuilds
base; math A/A independently rebuilds candidate. Never mix those controls.

Arithmetic retains 66 full and 66 held-out cases, nine measured pairs, one
warm-up, corpus seeds 197200/197201, fixed sparse supplements and bounded
confirmations. Retain the existing 900-second command and sparse-stage bounds.
Math retains sixty cases per preset, fifteen measured pairs, three warm-ups,
seed 1972170, a .25-second repeated duration target, 600-second child bounds and
an 18,000-second controller budget per preset. Calibrate invocation counts
before paired samples and preserve later duration shortfalls. Fixed A/A and
candidate confirmations retain seeds 1972171 and 1972161. The outer limit is
360 minutes per job, attempt one only, with no cancellation or favorable retry.

Complete all ordinary and diagnostic builds before elapsed samples. Bind source,
pinned Howard 7.0.3 (`c8dabac65d39fd527a9264a5e505a99b1d8df3d8`), compiler/SDK,
standard library, flags, argv, executable images and library bytes. Check every
role's outputs and state, including warm-ups. Never pool hosts, sources or modes,
choose a favorable Howard role, or conceal a losing class behind an aggregate.

## Resources, startup and preservation

Use matched `--compare-resources` diagnostics after all ordinary elapsed stages.
Retain selected-container allocation/refcount/clone events and five alternating
cold ordinary-image RSS pairs. Arithmetic covers startup, loops/calls/recursion,
arrays, sparse storage, input, temporary chains and finite array/closed-scalar
frame reuse. The latter diagnostic cases do not enable private scalar storage.
There are thirteen arithmetic cases per preset and sixteen math cases per
preset, 174 across the nine jobs. Math retains scale-100 regimes and scale-20
arctangent/Bessel boundaries, with each role's own ordinary `-l` source.

Retain both profiling execution policies: Apple Arm64 profiling can use scalar
argument packing where ordinary forwarding is eligible. Matching source and
native mode does not prove full execution-path identity. Native code buffers,
executable pages and OS unwind bookkeeping are outside selected-container
counters. Cold RSS is separate, descriptive evidence; retain unavailable
adapters, incomplete pairs and adverse observations. Keep actual diagnostic
images/recipes under `profile-images/` and elapsed images/inputs under `codegen/`.
Never rebuild missing measured assets. Native project/cache evidence is not a
complete toolchain or build-tree snapshot.

After arithmetic, bind its exact base/A/A/candidate images into the unchanged
startup protocol: sixty two-stage null blocks, 151 pairs per stage, fixed seeds,
candidate stages, a 2,400-second total bound and five-second child bounds.
Preserve timer, precision, sensitivity and null-bias assessments. Other studies
cannot substitute. Startup qualification does not qualify math one-shot latency.

## Acceptance and the ten-day stop

Keep `bc-performance-acceptance.md` unchanged. Linux/Windows allow 5% per change;
macOS retains its existing cap. A beyond-cap exception requires primary Howard
upper 95% bound at most .90, valid controls/samples/durations and a worthwhile
gain elsewhere. Competitive targets (median at most .90, nominal upper below
one) differ from that stricter exception. Startup and one-shot rules remain
separate. Preserve collector and advisory labels together.

Independently replay every retained count, order/output identity, input/oracle,
source/recipe/image binding, interval and decision, then review correctness,
startup, resources, code size and integration. Green collection is not adoption.
Replace only this topic's redundant narrow PR timing; main/manual/scheduled
trends remain. Preserve older experiment artifacts with their missing-image
and provenance limits explicit.

Allow queue, collection and review within the user's ten-day TOTAL goal-time
limit (`get_goal().goal.timeUsedSeconds <= 864000`), not ten additional days.
At the limit stop qualification and report accepted gains, rejected candidates
and unmet requirements. Do not waive policy, extend the timebox, mark #197
complete or close unmet issues. Merge only a fully reviewed green candidate;
immediately update primary main and review its own CI before another merge.
