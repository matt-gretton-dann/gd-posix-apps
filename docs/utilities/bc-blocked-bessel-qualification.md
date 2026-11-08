# Blocked Bessel native qualification

This independent #217/#197 experiment compares the bounded four-term
ascending algorithm with accepted main
`64e7ee6763b1b7a0731d88d72b7d9e9c2397dc19`. Its proof and fallback are in
`bc-portable-math.md`. For absolute arguments through eight and entry scales
through 100, two divisions add and advance four terms at working scale S+12.
The extra fractional-product errors are bounded below .001 output ULP before
rounding. Ambiguous rounding repeats the original single-term recurrence at
generic precision with the original input. Other domains use that recurrence.
Only the six public function names remain; all implementation state is local.

The integer-only grouping experiment #498 and the single-term bounded pass
#674 are separate sources and studies. Their gains, losses, incomplete and
inconclusive observations stay under their original identities. Do not transfer
their qualification or pool their ratios with this experiment. More source,
two additional automatic scalars, larger result frames and cold loading may
offset fewer divisions. No elapsed benefit is established by preflight.

Finish and preserve every original #674 job and record its complete disposition
before publishing this source or starting this cohort. Before publication,
record the immutable final source, protocol and workflow hashes in #217.
Never edit that source, replace an incomplete/adverse result, cancel a losing
job, or restart the cohort for acceptance. Earlier rejected VM experiments and
#673's short CPU observations do not qualify this source or explain their losses.

## Correctness preflight

Commit independent contracts before implementation. All 27 CLI tests pass on
unchanged accepted main and the candidate, including 110 Arb enclosures and
ordinary-source execution on Apple Howard 7.0.3. Exact-rational tests compare
the block identity with four independently advanced terms, bound each extra
product truncation, and check every entry scale through 100. The numerical
proof covers input truncation, coefficient construction, block propagation,
underflow/omitted tails and rounding fallback.

Six focused math CTests pass with extensions ON and OFF. The interpreted
source-bound ledger certifies 727 results, observes 214/226 outcomes and
accounts for 224 with explicit scale-clamp models. The two unchanged
exponential-sign outcomes remain infeasible. These counts are not compiler
MC/DC. Seventeen resource fixtures and eleven focused integration checks pass.
Hosted correctness on the final published head remains required. No local
production elapsed benchmark is allowed.

## One declared native cohort

Run `ubuntu-24.04`, `macos-15` and `windows-2025`, each with arithmetic,
full-math and held-out-math jobs: nine qualification jobs. Use maintained
default build options (extensions ON and the native typed/image/IPO defaults),
recorded by the builders. Extensions OFF has separate correctness evidence;
neither source introduces an optional backend mode.

Arithmetic retains 66 full and 66 held-out cases, nine measured pairs,
one warm-up, seeds 197200/197201, existing bounded confirmations and fixed
sparse supplements. Retain the maintained 900-second command bound and
existing sparse-stage bounds. Math retains 60 cases per preset, fifteen
measured pairs, three warm-ups, ordinary seed 1972170, a .25-second repeated
duration target, 600-second child bounds and an 18,000-second controller
budget per preset. Counts freeze after separate calibration, before paired
samples. Keep later duration shortfalls. Existing bounded A/A and candidate
confirmations use seeds 1972171 and 1972161. These declared confirmations
are not favorable restarts. Each job has a 360-minute outer bound and must
be attempt one.

Arithmetic A/A independently rebuilds accepted base; math A/A independently
rebuilds candidate. Finish every ordinary/diagnostic build before sampling.
Bind source, native Howard pin, compiler/SDK/standard-library identity, flags,
actual image hashes, argv and mathematical libraries. Do not pool hosts,
sources or layouts, choose a favorable Howard role, or report an aggregate
that hides a losing class. Numerical output/oracle/state checks apply to
every measured role and warm-up.

## Resources, startup and preservation

Use `--compare-resources`. After all ordinary elapsed stages, matched
instrumented builds collect selected-container allocation/refcount/clone events,
then five alternating cold native RSS pairs. Arithmetic representatives cover
startup, loops/calls/recursion, arrays and sparse storage, streaming/file input,
temporary chains, early return and dense frame reuse. Math uses ten one-shot
regimes at scale 100 plus scale-20 atan and all five Bessel regimes. Both math
roles use their own `-l` library. Discard instrumented elapsed values.

Counters are not process malloc; cold RSS is not retained memory or a confidence
interval. Keep unavailable adapters, incomplete pairs and every adverse value.
Preserve actual profile images/recipes in `profile-images/`, separately from
ordinary images/recipes/declared inputs in `codegen/`. Generic binary/hash
preservation applies when native inspection is unavailable. Do not rebuild a
missing measured image. Source libraries, build logs/metadata and inspections
remain source-bound; these are not comprehensive native project/cache snapshots.

After arithmetic collection, use its exact base/A/A/candidate executables in
a fresh `startup-calibration.py` study: unchanged 60 two-stage null blocks,
151 pairs per stage, candidate stages, fixed seeds, 2,400-second budget and
five-second child bound. Preserve timer, precision/sensitivity and null-bias
qualifications. This no-library startup study neither establishes equivalence
nor automatically qualifies mathematical one-shot latency. No earlier startup
study or #673 diagnostic substitutes for it.

## Review and adoption

The competitive goal uses a paired median at most .90 and nominal interval
upper bound below one, with valid controls and declared measurement checks.
This differs from the stricter regression exception below: observing the
competitive goal does not automatically provide a beyond-cap allowance.
The collector's original goal labels, including inconclusive controls and
duration shortfalls, remain recorded separately from regression acceptance.

Apply unchanged `bc-performance-acceptance.md`: Linux/Windows retain 5% per
change and macOS its existing cap. Beyond-cap exceptions require primary-Howard
upper 95% bound at most .90, valid controls, declared samples/durations and a
worthwhile gain elsewhere. Startup and one-shot rules remain separate. A green
collection job cannot qualify an incomplete or inconclusive row.

Independently replay every raw stage's counts/order/check identities,
program/oracle bytes, source/recipe/image/input bindings, intervals and
decisions. Review the full correctness, startup, memory/allocation, cold RSS,
code-size and integration evidence. Preserve every loss and limitation. The
older two-platform threaded workflow may run because CMake registers the
resource fixture; retain it separately without pooling it into this cohort.
Main, scheduled and manual trend workflows remain enabled. Only this topic's
redundant narrow PR timing is replaced by its declared full cohort.

Merge only a completely reviewed green candidate, immediately synchronize the
primary checkout to origin/main, and review that merge's own CI before another
integration merge. #217/#200/#207/#197 remain open until their actual scope
and competitive obligations are fulfilled.
