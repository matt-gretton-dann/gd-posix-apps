# Declared mathematical input calibration

Refs #710, #200 and #217. The checked benchmark distinguishes two phases:

- `--input-corpus DIR` reads a declaration of unmeasured math programs and
  independently prepared oracles for one initial duration calibration. Use
  `--suite math --calibrate-seconds TARGET --calibrate-only` with the usual
  implementation config, fresh output directory and child deadline.
- `--replay-corpus DIR` reuses the prepared directory's exact hashed programs,
  oracles and frozen common iteration counts for measured comparisons. It
  remains incompatible with new duration calibration.

The options are mutually exclusive. Inspect a declaration without running
children using `--suite math --input-corpus DIR --list`. Calibration does not
establish comparative performance, passing acceptance gates or untouched
holdouts. No declaration or marker proves that values have never been seen.
Prospective source/protocol declarations and independent review remain needed.

For initial preparation:

```sh
python3 src/bc/scripts/bench-bc.py --suite math \
  --input-corpus declared-inputs --config preparation.json --output prepared \
  --calibrate-seconds .25 --calibrate-only --timeout 600 --require-provenance
```

Then measure the frozen inputs without recalibration:

```sh
python3 src/bc/scripts/bench-bc.py --suite math \
  --replay-corpus prepared --config paired.json --output paired \
  --samples 31 --warmups 3 --timeout 600 --require-provenance
```

These examples describe CLI phases; they do not declare a new qualification
cohort, alter controls or authorize retrying a frozen study.

## Declaration format

`DIR/manifest.json` contains `kind: "unmeasured-math-inputs-v1"`, `suite: "math"`,
the preset/corpus seed, a 64-character lowercase SHA-256
`reference_fixture_sha256`, and a nonempty `corpus` array. Each record uses the
existing replay layout: unique safe `name`, `group`, `parameters`,
`program_sha256` and `oracle_sha256`. Actual UTF-8 program bytes are at
`workloads/NAME.bc`; the decimal-ULP oracle JSON is at
`workloads/NAME.expected`. The reference identity records declared provenance;
it does not independently prove the enclosure or its mathematical accuracy.

Every member must have `parameters.calibration: "fixed count; not calibrated"`,
mode `one_shot` with integer iteration count one or mode `repeated` with an
integer count of at least two. Oracles use `kind: "decimal-ulp"` and the usual
scale, enclosure and expected caller state. Validate the entire declaration,
including input/oracle hashes, before applying any workload filter.
Repeated programs and their `_xCOUNT` names must match the existing canonical
mathematical loop built from their function, arguments, scale and count. This
ensures calibration changes the count rather than replacing an inconsistent
declared program body.

Reject duration-calibration metadata, duration-target dimensions and existing
`samples.jsonl`, `summary.json`, `calibration/` or `calibration-only.json`
artifacts in the declaration directory. This structural validation prevents
accidental phase mixing; it is not an attestable history of prior observations.

Prepared artifacts record the input manifest's actual hash, source directory,
declared preset/seed/reference identity and initial-calibration scope under
`declared_input`. They separately retain the checked calibration trials and
frozen program/oracle hashes. Frozen replay records `replayed_corpus` and never
creates another calibration phase. Existing default corpora, output checks,
deadlines, statistics and acceptance policy are unchanged.

The original #709 source `18a25510` and its failed run remain immutable. This
tooling capability does not repair that run or qualify its numerical algorithm.
