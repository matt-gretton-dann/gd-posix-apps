# Empty result initialization native qualification

This independent #207/#197 experiment compares the generic `ResultSlot`
constructor with accepted main
`64e7ee6763b1b7a0731d88d72b7d9e9c2397dc19`. Its lifetime contract and static
code-generation observations are in `bc-result-slot-initialization.md`.
Constructing the empty variant explicitly may avoid inactive payload stores,
but instruction count, code layout and cache effects may offset that work.
No elapsed gain is established by the static inspection or correctness tests.

Preserve every original PR #675 job under its frozen source and protocol. This
independent VM topic may qualify in parallel using its own job group, source
identity and within-job controls. Publication does not replace or restart any
#675 job. Complete that study's original review and disposition before an
integration merge; never transfer its observations to this source.

Record the final immutable source, protocol and workflow hashes in #207 before
publication. Never edit that source, cancel an adverse job, or restart the cohort
for acceptance. Other experiments' gains, losses, incomplete and inconclusive
observations retain their own identities. No local production elapsed benchmarks
are permitted. This prospective sequencing plan was revised before publication;
all measurement and acceptance requirements remain unchanged.

## Correctness preflight

Apple arm64 Release C++20 passes 54 VM cases (388,244 assertions) and ten
focused CTests with typed results OFF, then the same cases and CTests with
typed results ON. These cover full and verified execution, mixed owning-result
reuse, allocation-failure recovery, lifetime/IPO checks, signals, diagnostics
and CLI/math behavior. Existing contracts check active values and empty states;
no test relies on inactive bytes or a standard-library layout. Native hosted
correctness on the final head remains required. Normal generic fallback,
variant lifetimes and checked access remain available on every platform.

With typed results and allocation profiling both enabled, the Release build
also passes 58 VM cases (388,302 assertions) and six focused profiling,
consumer and lifetime CTests. This checks the instrumented mode used by the
resource collector without using its durations as elapsed evidence.

The reused opt-in resource collector has 17 fake-process fixtures. Instrumented
observations and their durations never become ordinary performance samples.

## One declared native cohort

Run `ubuntu-24.04`, `macos-15` and `windows-2025`, each with arithmetic,
full-math and held-out-math jobs: nine qualification jobs. Use maintained
default build options, including extensions ON and native typed/image/IPO
defaults. Record those options; an explicit local typed test does not change a
platform default or establish optional-mode performance acceptance.

Arithmetic retains 66 full and 66 held-out cases, nine measured pairs,
one warm-up, seeds 197200/197201, existing bounded confirmations and fixed
sparse supplements. Retain the maintained 900-second command bound and
existing sparse-stage bounds. Math retains 60 cases per preset, fifteen
measured pairs, three warm-ups, ordinary seed 1972170, a .25-second repeated
duration target, 600-second child bounds and an 18,000-second controller
budget per preset. Freeze counts after separate calibration and before paired
samples; retain later duration shortfalls. Existing bounded A/A and candidate
confirmations use seeds 1972171 and 1972161. These declared confirmations are
not favorable restarts. Each job has a 360-minute outer bound and must be
attempt one.

Arithmetic A/A independently rebuilds accepted base; math A/A independently
rebuilds candidate. Complete every ordinary and instrumented build before
sampling. Bind source, native Howard pin, compiler/SDK/standard-library
identity, flags, actual image hashes, argv and mathematical libraries. Check
outputs, mathematical oracles and state for every measured role and warm-up.
Never pool hosts, sources or modes, select a favorable Howard role, or hide
losing classes behind an aggregate.

## Resources, startup and preservation

Use `--compare-resources`. After all ordinary elapsed stages, matched
instrumented builds collect selected-container allocation/refcount/clone events,
then five alternating cold native RSS pairs. Arithmetic representatives cover
startup, loops/calls/recursion, arrays/sparse storage, streaming/file input,
temporary chains, early return and dense frame reuse. Math uses ten one-shot
regimes at scale 100 plus scale-20 atan and all five Bessel regimes; each role
uses its own `-l` library. Discard instrumented elapsed values.

Counters are not process malloc totals; cold RSS is not retained memory or a
confidence interval. Keep unavailable adapters, incomplete pairs and adverse
values. Preserve actual instrumented images/recipes under `profile-images/`,
separately from ordinary images/recipes/declared inputs under `codegen/`.
Retain binary/hash evidence where native inspection is unavailable. Never
rebuild a missing measured image or describe these archives as comprehensive
native project/cache snapshots.

After arithmetic collection, use its exact base/A/A/candidate executables in
a fresh `startup-calibration.py` study: unchanged 60 two-stage null blocks,
151 pairs per stage, candidate stages, fixed seeds, 2,400-second budget and
five-second child bound. Preserve timer, precision/sensitivity and null-bias
qualifications. No earlier startup study substitutes for this one. No-library
startup does not establish equivalence or qualify mathematical one-shot latency.

## Review and adoption

Apply unchanged `bc-performance-acceptance.md`: Linux/Windows retain 5% per
change and macOS its existing cap. Beyond-cap exceptions require the primary
Howard upper 95% bound at most .90, valid controls, declared samples/durations
and a worthwhile gain elsewhere. Startup and one-shot rules remain separate.
The competitive goal, median at most .90 and nominal upper bound below one
with valid controls/durations, differs from that stricter regression exception.
Preserve original collector labels and separate advisory acceptance labels.

Independently replay raw stage counts/order/check identities, program/oracle
bytes, source/recipe/image/input bindings, intervals and decisions. Review all
correctness, startup, resources, code size and integration evidence. A green
collection job cannot qualify an incomplete or inconclusive row. Preserve the
older two-platform threaded workflow separately if CMake registration triggers
it; its observations are not this cohort's adoption evidence. Only this topic's
redundant narrow PR timing is replaced; main/scheduled/manual trends stay enabled.

Merge only a completely reviewed green candidate, immediately synchronize the
primary checkout to origin/main, and review the merge's own CI before another
integration merge. #200/#207/#197 remain open until their complete scopes and
competitive obligations are fulfilled.
