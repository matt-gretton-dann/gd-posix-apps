# Testing bc arithmetic

Arithmetic optimization is gated by correctness tests; agreement between two
implementations of multiplication alone is not an independent oracle.

`test-number.cc` uses the following complementary checks:

* NumberTraits8 exhausts all 100 by 100 one-limb pairs for addition,
  subtraction, multiplication, quotient and remainder. A second set gives each
  operand an extra low limb to force carries and borrows across a limb boundary.
* A shared NumberTraits8/16/32 suite checks signed, directed multi-limb inputs
  and reproducible dense inputs against exact native integer arithmetic.
  Inputs stay at or below 3,000,000,000, so all products fit in `int64_t`.
  Thresholds 1, 2, 3 and the trait base exercise Karatsuba, mixed and schoolbook
  dispatch. Shared copies and self-aliasing operations have separate assertions.
* Conversion and width tests retain small-trait coverage while checking the
  largest valid limb, widening bounds, error sentinels and nondecimal fractions
  in each trait. No enormous allocations are needed to test these boundaries.
* Larger randomized multiplication tests compare the same operand pair in
  schoolbook, Karatsuba and mixed modes, and check the third result explicitly.

CTest uses seed `197199`. To replay or investigate with a different seed:

```sh
build/src/bc/test-libbc '[number]' --rng-seed 197199
build/src/bc/test-libbc '[oracle]' --rng-seed 197199
```

Failures include operands and seed/sample information. The exhaustive cases do
not depend on a seed. Tests which alter the global multiplication threshold
restore its initial library default (1) on scope exit, including assertion
failures. Successes are not individually logged, keeping exhaustive CI runs
manageable.

The small trait is an algorithmic model, not a substitute for width-specific
overflow checks or native-platform validation. Current coverage also does not
prove conformance for every possible scale or operand length. Independent exact
rational CLI oracles and the power/modulo defects found while developing them
are tracked in [#199](https://github.com/matt-gretton-dann/gd-posix-apps/issues/199),
[#219](https://github.com/matt-gretton-dann/gd-posix-apps/issues/219) and
[#220](https://github.com/matt-gretton-dann/gd-posix-apps/issues/220).

Raw line, branch and MC/DC measurements and feasibility classification remain
separate work in [#214](https://github.com/matt-gretton-dann/gd-posix-apps/issues/214)
and [#215](https://github.com/matt-gretton-dann/gd-posix-apps/issues/215). No coverage
percentage is inferred from assertion counts.

## Calibration input and replay

The multiplication tuner and arithmetic generators share a valid limb domain
`[0, base-1]`; the inclusive radix itself would yield decimal digit 10. Small
traits test that domain directly, including all base-100 values. Timing helpers
test normalization at an exact one-second carry and unequal nanosecond operands.

`find-multiply-split-point` uses input seed `197234` by default. To replay a
different input stream, run:

```sh
build/src/bc/find-multiply-split-point --seed 197234 output.hh
```

The seed is flushed to the log before calibration and recorded in the generated
header. It replays inputs on the same toolchain/trait configuration; measured
times and the selected threshold still vary with host load. This calibration
does not establish a portable optimal threshold or a performance win.

Invalid arguments, unavailable CPU clocks and output errors fail explicitly.
Fast `bc-tuner/cli` checks validate arguments without starting calibration. They
are separate from production bc coverage; benchmark-support helpers are test/
calibration code. The old unseeded CI abort cannot be attributed conclusively,
but invalid generated limbs and incorrect timing are independently reproduced
and corrected. See [#234](https://github.com/matt-gretton-dann/gd-posix-apps/issues/234).

Calibration grows each timing batch by at most four times, doubling when the
process CPU clock reports zero for a short batch. Batch arithmetic checks the
container limit before multiplication and reports invalid durations or exhausted
capacity as failures. Timing samples are flushed for failure diagnosis. These
rules handle coarse clocks without assuming a platform-specific resolution.

## Imported zero-scale oracle discrepancy

The imported Howard `tests/misc` fixture has four saved answers which discard
precision when subtracting a tiny power truncated to zero. POSIX requires
addition/subtraction to preserve the maximum operand scale; GNU agrees with
that rule. The imported test still runs and reports an explicit expected
failure against its old answers. Reverting to those answers would be an
unexpected pass and fail the suite.

`bc/number-oracle` independently checks all four expressions, including escaped
newlines and base-9 output, using exact fractions, decimal truncation and integer
radix conversion. It requires status 0, empty stderr and the complete corrected
output. This replaces the incorrect oracle without adding an unsupported-test
skip or accepting arbitrary failures. See
[#233](https://github.com/matt-gretton-dann/gd-posix-apps/issues/233).

## Exact scaled arithmetic and long division

`bc/number-oracle` checks values and result scales with Python `Fraction` and
integer arithmetic. Scaled multiplication/division include signed, fractional,
balanced, asymmetric and shared-operand expressions, with seeded dense inputs up
to 2,000 decimal digits. This complements the forced multiplication-threshold
unit checks; a calibrated CLI threshold alone does not guarantee every dispatch
path. Python's integer string conversion limit need not be disabled: the largest
product has at most 4,000 digits.

Square roots use an integer floor-root calculation and independently assert
`r*r <= x < (r + 10^-result_scale)^2`. Cases straddle exact squares, including
the former adjacent-iterate cycle (#237), and retain zero/fractional scale checks.
No floating-point tolerance or agreement between bc kernels supplies the oracle.

`test-number-division.cc` checks native-integer quotient and remainder oracles
with all sign combinations, shared copies and self-aliasing on Traits8/16/32.
The base-100 model makes multi-limb normalization and correction witnesses small
enough for independently checked 64-bit arithmetic:

| Dividend / divisor | Base-100 normalization | Directed path |
| --- | --- | --- |
| `3128583549859727 / 34970` | 17 | Second-limb estimate correction |
| `6687218857121369 / 63964738` | 1 | Repeated estimate decrement; already normalized |
| `8623134770622992 / 819692222` | 7 | Borrow and add-back after multiply/subtract |
| `1626423182298612 / 81326081630` | 7 | Base-sized estimate, repeated correction and add-back |

For the final case, at quotient position 2 the estimate is 2 with the second-limb
comparison `184 > 184` false. At position 1, a later estimate is 100 (the base)
with `9200 > 9213` false: the first condition of the correction decision must
change the result even when its second condition is false. The first case also
supplies estimate 9 with `396 > 85` true, followed by estimate 8 with
`352 > 5985` false. These are decision-construction witnesses; confirm execution
and recorded independence pairs in fresh LLVM exports before claiming coverage.
The expected quotient/remainder still comes from native division, not this path
model. The same numeric cases supplement wider traits without claiming identical
limb paths at their different radices.

### Quotient estimates at each native limb width

The parameterized quotient-estimate fixtures construct independent decimal limb
blocks at each of the three radices, including the native 10^9 radix. They do not
rely on rare seeded coincidences or on numbers small enough to fit one/two native
limbs. With `B` the limb base, their exact identities are:

- `V=(B/2)B^2+B+1`, `U=VB-1`: quotient `B-1`, remainder `V-1`. The initial base-sized estimate has a false second-limb comparison.
- `V=(B/2)B^2+(B-1)B+1`, `U=(B/2)B^3`: quotient `B-2`, remainder `2B^2+(B-3)B+2`. The estimate needs repeated correction.
- `V=(B/2)B^2+1`, `U=V(B-1)-1`: quotient `B-2`, remainder `V-1`. Low-limb subtraction forces borrow and add-back.

All sign combinations and independently reconstructed original operands are
asserted. The CLI Fraction oracle also checks these algebraically constructed
inputs at the supported radices. Fresh per-instantiation LLVM evidence is still
required when reporting whether a specific native branch/condition was covered.

A clean test-only MC/DC checkpoint at `ecfa2a084b01efba310e3116dbbe6de72b5dd1cb`
records both quotient-estimate conditions independently covered in each of the
three limb-width instantiations, in ON and OFF builds. This confirms the
parameterized fixtures' native condition coverage; it does not replace their
independent quotient/remainder assertions or imply complete arithmetic coverage.

The input conversion suite also includes 35-bit binary fractions. Their exact
terminating decimal values exercise multiple conversion chunks even with native
32-bit limbs, complementing the shorter fractions which end at the scale guard.

## Isolated LLVM coverage

Use a Clang/LLVM toolchain which supports MC/DC. `ENABLE_BC_COVERAGE=ON`
checks compiler support and instruments bc, its implementation library and its
unit tests with `-fprofile-instr-generate -fcoverage-mapping -fcoverage-mcdc`.
Coverage is opt-in; ordinary compiler configurations are unchanged. Both asserted
CLI and unit tests are required, including when extensions are disabled.

Configure and build ON and OFF separately, preserving configure/build logs:
Use the default static libraries so both executable mappings contain the linked
implementation. Missing baseline implementation mappings are an error, not a
smaller coverage denominator.

```sh
cmake -S . -B build/bc-coverage-on -G Ninja -DCMAKE_BUILD_TYPE=Debug \
  -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ \
  -DENABLE_NON_POSIX_EXTENSIONS=ON -DENABLE_BC_COVERAGE=ON
cmake --build build/bc-coverage-on --target bc test-libbc test-util \
  test-bc-offsets test-bc-signal
python3 src/bc/scripts/collect-coverage.py build/bc-coverage-on build/bc-report-on
```

Repeat with a distinct `bc-coverage-off` tree, `ENABLE_NON_POSIX_EXTENSIONS=OFF`
and a new `bc-report-off` output directory. The collector refuses an existing
output directory, runs the discovered bc tests, and merges only profiles written
by that test run. Build-time tuner, benchmark and manual-smoke profiles cannot
enter the report. Unsupported imported entries retain their explicit skip
results. It returns failure if tests fail or MC/DC instrumentation is absent.

On macOS, select matching tools with `--llvm-cov "$(xcrun --find llvm-cov)"`
and `--llvm-profdata "$(xcrun --find llvm-profdata)"`. Other hosts can pass explicit
matching LLVM executable paths. Keep compiler/tool versions and build logs with
the report, including any instrumentation limit or exclusion warnings.

Artifacts include the selected test manifest, test logs, raw profiles and merge
manifest, full LLVM JSON, per-file/raw summary counts, text and annotated HTML,
source revision/dirty state and LLVM warnings. Header coverage retains native
and small-trait instantiations for continuity with the original broad baseline.
An additional application-mappings view excludes instantiations found only in
the unit executable; its counts remain separate from the broad baseline.
Test/support/generated/tuner files are excluded by role and filename, not by
whether they were covered. Never sum repeated template `mcdc_records`; LLVM's
summary supplies the tracked condition denominator.

Report raw counts alongside any reviewed eligible result. The collector computes
no adjusted denominator or feasibility exclusions. Single-condition decisions,
constant-folded/uninstrumented expressions, and condition/vector limits are tool
scope, not covered MC/DC. Review actual short-circuit independence pairs and
maintain the decision-specific feasibility ledger before claiming coverage goals.

The [coverage decision ledger](bc-coverage-ledger.md) records asserted condition
pairs, source invariants and open feasibility reviews. It does not calculate an
eligible denominator or replace the raw LLVM reports.

The [committed coverage checkpoint](bc-coverage-results.md) records raw ON/OFF
and application/broad counts, the measured revision and remaining limitations.
Rerun the collector for later code; the checkpoint is not an adjusted-goal claim.

## Unnormalized digit lengths

`test-number-length.cc` checks length after documented digit operations leave
high zero limbs. Multiplying by a power of ten and then applying `mac(0, value)`
preserves allocated high zero limbs without modifying private state. Length must
skip them toward the reverse end, report one digit for zero and leave shared
copies unchanged. The fixtures cover empty/all-zero storage, decimal/limb
boundaries and all three traits. AddressSanitizer rejects the former reverse
iterator traversal before the fix; ordinary number tests retain the normalized
production cases.

## Signed comparison oracle

`test-number-compare.cc` interprets a directed table of native integers as
thousandths and compares every pair with `BasicNumber` at all three limb widths.
Extra decimal zeros vary the two operand scales independently. Equality and both
strict orderings must agree with the independent integer comparisons, including
two negative values, mixed signs, zero, equal values and different magnitudes.
Negative zero remains equal to zero. The CLI rational oracle checks all six
relational operators in POSIX conditional syntax against exact Python Fractions.

The initial table rejects the former magnitude-order bug for two negative
operands. Scale alignment may swap the magnitude operands, and negative signs
reverse their order again; equal magnitudes remain equal in either orientation.

## Zero digit parity

Parity tests cover both valid zero representations: absent storage and an
allocated empty vector from constructing zero. They also check ordinary limbs,
shared values, reset storage and zero-valued multiply-accumulate results at each
trait width. These inputs use documented constructors/operations; the former
empty-vector read is rejected by AddressSanitizer/UBSan before the fix. Source
power evaluation already checks a zero exponent before calling this helper.

## Imported fixture output isolation

Imported scripts may write generated outputs beside their fixtures. CMake copies
the imported test inputs into `gdpat-test-source` in each build tree, excluding
Git metadata and preexisting output directories. Test discovery and execution
use that copy, so worktrees and ON/OFF builds can share a downloaded dependency
source without sharing mutable results. The upstream source is left unchanged;
no platform-specific copy facility is required.

`harness/gdpat-isolation` configures two small build trees against one mock
source, waits for both writers to signal readiness, then lets them read. Both
must retain their own marker and the shared source must remain untouched. The
protocol uses bounded waits and reaps its children. This guarantees isolation
between build trees; concurrent invocations of the same imported test in one
build tree still share that tree's output path.

## Full native constructor values

The native-value constructor accepts the underlying unsigned type, including
its maximum value and values at/above the limb radix. Directed tests compare
those values with independently parsed decimal text at Traits8/16/32, then
check ordering, subsequent addition and preservation of the original shared
value. Multiply-accumulate must drain its complete carry into radix-sized limbs;
appending a full native carry as one limb gives a noncanonical value even when
its decimal spelling happens to look correct.

## Fatal parser reuse

A syntax diagnostic terminates that parser object. Every later `parse()` call
returns a fresh canonical stream containing the original diagnostic, stderr
print, `quit(1)` and one EOF. Previously returned streams remain unchanged.
Abandoned loop/function context is cleared when the fatal stream is emitted.
This prevents duplicate EOFs and partially suppressed definitions when a caller
inspects or reuses a fatal parser. It does not add recoverable parsing (#185).

The repeated-parse table checks EOF, newline, unfinished loops/functions and a
definition at the error cursor in both interactive modes. A separate positive
case executes two nonfatal interactive items and then EOF to guard normal reuse.
