# Checked bc benchmarks

`src/bc/scripts/bench-bc.py` runs a maintained process-level corpus for #200 and
#197. It replaces the old two-positional-argument `timeit` helper. Custom
`f(a)` scripts are no longer timed without an output oracle; the Fibonacci and
factorial scripts remain inputs to the checked corpus.

## Run a comparison

Create a JSON file with named argv lists. Paths containing spaces or quotes are
ordinary arguments; the runner never constructs shell commands.

```json
{
  "implementations": [
    {"name": "gd", "argv": ["/absolute/path/to/gd-bc"]},
    {"name": "howard", "role": "reference", "argv": ["/absolute/path/to/howard-bc"]}
  ]
}
```

```sh
python3 src/bc/scripts/bench-bc.py --list
python3 src/bc/scripts/bench-bc.py --config implementations.json \
  --reference howard --output build/bc-comparison --samples 9 --warmups 1
```

The output directory must be new. The default `full` preset retains the sixteen
#197 workload parameters and adds dense/sparse/unbalanced multiplication,
genuine multi-limb division, modulo, powers, a larger square root, changed-base
literals, recursion, sparse arrays, definition registration, decimal input and
output-heavy cases, plus equal-scale comparisons with high/low unequal limbs
and equal values. Aliased squares at 100, 1000 and 10000 decimal digits
exercise triangular and recursive square dispatch. A repeated large literal
mutates each result to check cache snapshot independence. `--list` shows all
50 names and exact parameters. Repeat
`--workload NAME` to select cases. Startup is a separate class.

`--preset quick` reduces dimensions and iterations for fast correctness checks.
Its parameterized names and manifest identify the changed experiment; quick
results are not substitutes for full-workload timings.
`--preset holdout` increases dimensions by one eighth (at least one) and reduces
iteration counts by one eighth, with distinct parameterized case names. Use a
separate `--corpus-seed`, such as 197201, and reserve this preset from tuning or
PGO training. Startup remains a fixed control. The historical full-case program
and oracle hashes are checked against the recorded baseline; adding held-out
shapes does not rewrite the original experiment.
Math acceptance remains issue #217 work. Select its separate corpus explicitly
as described below; an unselected suite is never a performance pass.

## Optional mathematical corpus

`--suite arithmetic` is the default and preserves all fifty existing cases.
`--suite math` selects sixty one-shot/repeated cases: all six public functions
at scales20/100/1000, plus Bessel turning, order-dominated, argument-dominated
and near-root regimes. Quick mode uses scales2/20/100. Held-out inputs use
different argument magnitudes and Bessel orders, with
different repeated-call counts. Names and parameters retain each actual input,
scale, call count and mode. Default fixed counts are **not calibrated**; short
cases do not establish a substantive timing advantage. Use the explicit
calibration/replay protocol below when substantive repeated-call duration is
required.

Each configured command must load its chosen library explicitly. For example:

```json
{"implementations": [
  {"name": "howard-standard", "role": "reference", "argv": ["/path/to/howard", "-l"]},
  {"name": "gd-portable", "argv": ["/path/to/gd-with-library", "-l"]},
  {"name": "howard-portable", "argv": ["/path/to/howard", "/path/to/math-library.bc"]}
]}
```

```sh
python3 src/bc/scripts/bench-bc.py --suite math --list
python3 src/bc/scripts/bench-bc.py --suite math --config math-implementations.json \
  --reference howard-standard --output build/bc-math-comparison
```

The standard library and portable-source comparison remain distinct roles.
The identical program and requested scale are used in all roles. One-shot
and repeated processes include loading, constant generation and output; the
repeated cases still include preload/startup, rather than subtracting a fitted
startup cost. Every warmup and sample must satisfy the independent outward
enclosure at both endpoints within one decimal ULP, and preserve scale/ibase
and returned scale. Howard's output is checked independently, never used as
the mathematical oracle. An enclosure crossing the decision boundary reports
that refinement is required; it is not accepted or labeled a proven failure.

Mathematical `.expected` artifacts contain JSON rational enclosures and state
requirements. The manifest hashes their actual bytes, the numerical checker
and the reference fixture. Ordinary arithmetic `.expected` artifacts retain
their previous exact-output format. Larger arguments and scales remain in
the broader accuracy corpus even when omitted from this bounded timing subset.
This harness support alone makes no competitive-performance claim.

The held-out mathematical inputs use independently certified enclosures for
1.125 regular arguments, log(2.25), Bessel turning/order/argument cases at
(23,22.5), (113,1.125), (1,112.5), and a near-root order-one case. These replace
the initial sign/symmetry-only held-out shapes before native math timing.
The sixty full cases retain their exact program/oracle/parameter dictionaries,
verified against the recorded pre-change hashes. The dedicated benchmark
fixture contains eighty full/held-out enclosures across four scales; the
broader mathematical correctness corpus remains a separate scope.

## Calibrate once and replay frozen dimensions

`--calibrate-seconds 0.1` calibrates repeated math cases separately for each
configured role. Starting at two calls, double the count until the median of
three checked fresh processes reaches the target, subject to the explicit
`--calibration-max-iterations` cap and per-process timeout. Freeze the largest
required count as the common count for every role. One-shot latency cases are
kept at one call. No paired measurement is used to select a count.

`--calibrate-only` writes the validated setup, calibration trials, frozen
programs, oracles and manifest without paired samples. Subsequent A/A controls,
primary comparisons and bounded confirmations can use `--replay-corpus DIR`.
Replay verifies every program/oracle hash and retains the original preset,
operand seed, input transport and actual dimensions. It neither regenerates
inputs nor recalibrates counts. Supply the original suite explicitly.

```sh
python3 src/bc/scripts/bench-bc.py --suite math --config math-implementations.json \
  --output build/math-prepared --calibrate-seconds 0.1 --calibrate-only
python3 src/bc/scripts/bench-bc.py --suite math --config math-implementations.json \
  --replay-corpus build/math-prepared --output build/math-measured \
  --samples 15 --warmups 3
```

Calibration and paired samples are separate artifacts. A cap or timeout failure
retains its checked partial trials, exact programs and validated setup. A later
measured median below the target is recorded as a duration shortfall; the
harness does not change counts or rerun until favorable. Such a class does not
establish substantive-duration acceptance. Counts calibrated on one machine
need not meet the target on another; report actual measured adequacy.

Ordinary library files passed in a role's argv can be declared explicitly:

```json
"input_artifacts": [{"path": "/path/to/math-library.bc",
                     "sha256": "ACTUAL_64_HEXADECIMAL_DIGITS", "purpose": "ordinary library"}]
```

Each declaration must identify an actual argv file and match its bytes. The
harness archives it and rechecks it before each invocation, outside the timer.
This preserves the portable-source comparison's actual library identity;
the executable hash alone cannot identify an additional source file. File
validation warms caches, so these are process/preload measurements without a
cold-filesystem claim. Commands still choose their libraries normally.

## Archived evidence byte stability

Archived evidence directories disable Git text conversion so recorded program,
recipe and metadata hashes survive Windows checkouts. This preserves actual LF
or CRLF payloads; it does not relabel measured source identities. The native build
workflow verifies every committed SHA-256 payload and simulates Git checkout
conversion with `core.autocrlf=true`:

```sh
python3 docs/utilities/verify-bc-evidence-checkout.py
```

Use `--index` to validate staged packaging changes before committing. These checks
change neither worktree files nor persistent Git configuration. Historical
measurement payloads remain unchanged when packaging rules are corrected.

## Correctness and deadlines

Every warm-up and measured process must exit successfully, leave stderr empty,
and match its independent expected output. Each invocation has a finite
`--timeout` (30 seconds by default); Python kills and waits for its direct child
on expiration. Ordinary classes send programs on stdin followed by EOF, without
`quit` or an open-input dependency. The `file_streaming` class instead appends the
staged `.bc` path as one argv entry and supplies empty stdin, so the interpreter
cannot wait for another input source after the file. Its manifest records
`input_mode: file`. Missing/changed artifacts and unknown input modes fail before timing.
The recorded interval covers launch, input transfer/opening, parsing, calculation
and output capture. Environment preparation, encoding, artifact staging and
validation, oracle construction/comparison, hashing and statistics are outside it.
File validation reads the exact bytes before each trial: this is cached file
input, not a cold-filesystem or disk-bandwidth measurement.

Oracles use exact Python integers, exact scaled integer division/modulo/power,
and an integer floor-root construction. They do not use bc itself or floating
point tolerances. Continuation lines, CRLF and omitted fractional leading zero
are normalized. Nonzero fractional trailing digits, output count and values
remain significant; numerical zero uses bc's zero spelling. Environment BC_/DC_
options are removed, with explicit C locale and line-length settings recorded.

A failure stops the experiment with status one and `failure.json`; a successful
summary is written only after every invocation passes. Partial samples retain
their individual checked status. Do not treat an incomplete run as a passing
performance comparison.

## Paired measurements and raw evidence

The default schedule seed is `197200`. Within each round, workloads and the
implementation order within each workload block are shuffled. Implementations
therefore have matching nearby round observations. `--corpus-seed` separately
controls generated operands, allowing declared training and holdout inputs.
Both seeds, exact programs, parameters and oracle hashes are preserved.

Each implementation gets median/min/max wall time and median absolute
deviation. Comparisons report both the ratio of medians and the median paired
ratio. Two nominal 95% percentile-bootstrap intervals use the recorded seed:
IID resampling of whole paired ratios and circular resampling of neighboring
paired rounds in blocks. The prespecified block length is
`max(2, floor(sqrt(pair_count)))`. Both intervals and lag-one correlation are
reported. The final interval is their union, so it cannot become narrower than
the IID interval. Constant series with sufficient pairs have zero reported
lag-one correlation.

The block design follows the general circular-resampling approach of
[Politis and Romano](https://statistics.stanford.edu/technical-reports/circular-block-resampling-procedure-stationary-data).
The block-length choice and union rule are this harness's explicit protocol;
they are not an optimal-length claim or a finite-sample coverage theorem.
Fewer than three pairs have no interval. Small samples, nonstationary host
noise and drift remain limitations, and a small observed correlation does not
prove independence. Keep A/A controls and noisy/inconclusive outcomes visible.
No aggregate score or hard timing pass is produced.

Artifacts:

- `setup.json`: validated identities before calibration/trials, retained on
  bounded calibration failure.
- `calibration/`: separate checked diagnostic trials, exact programs/oracles
  and the frozen count decision, when requested.
- `declared-inputs/`: archived argv-file payloads with verified hashes.
- `manifest.json`: argv, binary hashes, declared build metadata, platform/clock,
  environment, seeds and exact corpus/oracle hashes.
- `workloads/*.bc` and `*.expected`: exact UTF-8 LF payload bytes matching the
  program/oracle hashes on every platform; reviewable programs and answers.
- `samples.jsonl` and `samples.csv`: every warm-up and measured observation,
  pairing/order, checked status, elapsed time and output hash/size.
- `summary.json`: per-case dispersion and paired comparisons, after success.

## Source and build provenance

When a recipe declares `binary_sha256` in its metadata or normalized entry,
the harness checks that identity against the actual executable before running.
Every provided declaration must agree; conflicting or malformed hashes fail.
Controlled `--require-provenance` runs need a matching declaration as well as
source/compiler/flags/version metadata and an uninstrumented binary. A supplied
installed comparator without a declared identity still records its actual hash,
but does not gain controlled provenance. Hash agreement checks the executable
identity; the preserved build recipe remains the evidence for its source and
compiler configuration.

The minimal configuration above is suitable for exploratory correctness checks.
Controlled competitive runs require `--require-provenance` and actual per-entry
`metadata` fields: `source_revision`, `compiler`, `flags`, `version`, and an
explicit `instrumented: false`. Record target architecture, compiler/library
versions, tuning thresholds and build options there as well. These fields are
declarations checked for completeness; source/build recipes must independently
establish their accuracy. The executable checksum is calculated automatically.
Timing rejects explicitly instrumented binaries; profiling uses separate runs.

An OS-installed comparator uses `role: installed`, remains visibly incomplete
when its build is unknown, and cannot be the controlled reference in strict
mode. Keep Apple's installed Howard separate from the pinned source-built
Howard. [Howard 7.0.3's build guide](https://github.com/gavinhoward/bc/blob/7.0.3/manuals/build.md)
documents its native POSIX and MSBuild configurations; exact checked-out commits
and actual compiler flags must accompany results. GNU is a separately identified
source-built comparison, not an interchangeable Howard version.

## Validation and scope

`bc/benchmark-cli` runs in both extension configurations on native CI and checks
all 40 quick workloads against that job's actual GD executable. Fake executables exercise failed exits, stderr, wrong output, warm-up validation,
timeouts, quoted argv, provenance requirements and timing/profiling separation.
Paired statistics use exact proportional samples to detect unpaired resampling;
repeated runs check schedule replay without asserting noisy elapsed times.

This corpus measures complete processes. Definition registration includes
parsing, validation and VM registration; decimal cases include the surrounding
interpreter/I/O work. They are not pure parser or arithmetic-kernel measurements.
The streaming case feeds sequential statements and checks every output line
through EOF. It measures bulk input/output throughput, including parsing and
execution; it does not assert interactive first-response latency. A separate
file-streaming case increments a scalar across complete file input items and
checks the final exact integer; it measures file opening plus frontend/VM work.
Existing stdin case definitions remain unchanged. The file class runs in the
native representative matrix as well as the full, held-out and quick corpora.

CPU/allocation profiles and component drivers are separate #200 follow-ups,
before choosing architectural changes. The corpus and this infrastructure make
no performance-improvement claim by themselves.

[Component drivers](bc-components.md) separate arithmetic API, parse-only and
conversion intervals from the end-to-end corpus, with explicit control costs.

The [31 October baseline and attribution report](bc-performance-baseline-20261031/README.md)
retains paired raw samples, A/A noise checks, allocation counts, release CPU
samples, native memory/counters and explicit platform/measurement limits.

Pinned native build commands, compiler/flag provenance and offline cache options
are documented in [comparator builds](bc-comparator-builds.md).


## Freeze multiplication dispatch during comparisons

Configure base and candidate with the same explicit threshold, for example
`-DBC_MULTIPLY_SPLIT_POINT=126`. Fixed mode writes the generated dispatch header
without executing calibration. This isolates unrelated optimizations from host
load and from a changed calibration result. Record the chosen point and generated
header with compiler/source metadata; 126 is an experiment setting, not a claim
of a portable optimum.

The default `portable` uses a stable 126-limb threshold on native and cross
hosts, avoiding expensive host-dependent calibration during normal builds.
This is the setting exercised by the native correctness/performance matrix;
it is not a portable optimum claim. Existing build caches retain their explicit
or previously cached selection until reconfigured.

Explicit `auto` retains native calibration and its seeded input stream.
Its cached result is a separate generated file, so switching back from a fixed
choice cannot mistake that fixed header for a calibration result.
When cross-compiling, `auto` uses a positive portable fallback of 128 limbs and
does not execute a target tuner on the build host. An explicit integer overrides
either mode. Accepted fixed values are decimal integers from 1 through the
current native limb radix, 1,000,000,000; invalid, zero and injectable values fail
at configuration before entering generated C++. Cross-selection logic is tested;
actual foreign-architecture binaries still require their target toolchain and
native/emulated correctness checks.

The [portable square evaluation](bc-203-squares/README.md) records independent
full/held-out measurements, diagnostic counters and native artifacts. It retains
the initial Linux ordinary-product regression and distinguishes the revised
dispatch results from inconclusive small-product/division observations.

## Explicit multiplication calibration

`find-multiply-split-point [--seed INTEGER] [--quick] [output-header]` evaluates
all points in the finite set 16, 32, 64, 96, 126, 192, 256 and 512 limbs. Full
runs cover 1/2/10/12/112/128/256/512/1112-limb shorter operands, balanced, 1:2,
1:8 and aliased-square shapes. Exact-width seeded inputs are parsed before
measurement. Each trial starts from the original operands; results never grow
from one repetition to the next. A schoolbook/square reference is prepared
outside timing, and every timed result is checked against it afterwards. The
independent arithmetic oracles remain the authority for kernel correctness.

Each shape receives an exact-checked warm-up for every point and five shuffled
candidate sweeps. The tool records every monotonic elapsed-clock sample, batch and correctness
flag as CSV rows in stdout, followed by per-candidate scores. It uses median
within-round ratios against 126 for each shape, then an equally weighted
geometric mean across shapes. Scores need not be monotonic in the threshold.
Within 1% of the lowest score, prefer the point closest to 126. This tolerance
reduces churn; it is not a confidence interval or a performance acceptance gate.
The emitted header retains the replay seed. Preserve stdout with that header
and the actual source/compiler recipe when using a calibrated setting.

Batch growth targets 2 ms but is bounded by both 256 results and approximately
65536 input limbs, so tiny cases can remain below the target. A zero-duration
measured sample is rejected. The portable steady clock avoids coarse process
CPU-accounting quanta on Windows; scheduling remains part of elapsed time.
Copies, input parsing and exact result comparisons
are outside the multiplication interval; copy-on-write detachment, temporary
workspace and result allocation during multiplication remain inside it. Clock
resolution, scheduling, frequency and corpus choice can still affect selection.
A finite sweep does not establish a global or universal optimum. Repeated runs
and other hosts may prefer different points; the default stays portable.

`--quick` is a smoke protocol covering three small sizes, all four shapes and
three rounds, with the same correctness and selection rules. It checks the
executable protocol in automated tests and should not tune production builds.

[Repeated tuning evidence and retained per-shape costs](bc-203-tuning/README.md)
record the stable-default decision and exact compiler/source provenance.

The expanded sqrt matrix (#212/#216) appends four cases to the previous 46,
retaining every earlier complete case dictionary and seeded operand. The full
corpus now has 50 cases. Additions cover sqrt(2) at scale 10000, tiny exact
squares with input scales 1024 and 4096 (requested scale zero), and a 4001-digit
integer square at scale 100. Quick and held-out presets report their actual
dimensions; tiny-square scales are rounded down to an even value so their
roots remain finite exact decimals. Independent integer-floor or power-of-ten
oracles do not use the executable's arithmetic.

Native representative checks retain the existing scale-5000 sqrt case, the
new scale-10000 class, both tiny-fraction dimensions and the large-integer
class. This prevents a first-per-group selection from exercising only the
small-scale sqrt winner. These are process workloads including parsing,
startup and output; they do not isolate Newton division or establish a speed
claim. Separate diagnostic counters and RSS measurements remain necessary.
The committed `fixtures/benchmark-corpus-before-sqrt.json` records hashes of
all 46 previous full/held-out/quick dictionaries, including programs, oracles,
parameters and transport, at source 704d8013c2ff877357bf9dc42a0ae68b571c6c3e.

The scale-4096 tiny-fraction case uses one repeat in full/held-out trials.
The initial ten-repeat source exceeded the existing 30-second deadline in
Howard on Windows. Preserve that failed run separately; reduce repetitions
without reducing precision, weakening its exact oracle or raising deadlines.
Initial local experiments with ten repeats remain labeled with their actual
programs and source identities; they are not relabeled as this workload.
