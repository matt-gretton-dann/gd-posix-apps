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
exercise the triangular kernel and recursive square dispatch. `--list` shows all 39 names and exact parameters. Repeat
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
 POSIX math functions are
explicitly pending #217 and cannot appear as a performance pass before their
accuracy-checked implementation exists.

## Correctness and deadlines

Every warm-up and measured process must exit successfully, leave stderr empty,
and match its independent expected output. Each invocation has a finite
`--timeout` (30 seconds by default); Python kills and waits for its direct child
on expiration. Programs arrive on stdin followed by EOF, without `quit` or an
open-input dependency. The recorded interval covers launch, input transfer,
parsing, calculation and output capture. Environment preparation, encoding,
oracle construction and comparison, hashing and statistics are outside it.

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
ratio. The approximate 95% paired percentile-bootstrap interval resamples whole
paired ratios using the recorded seed. Fewer than three pairs have no confidence
interval; the default nine pairs are still observations on the recorded host,
not a guarantee about another machine. No aggregate score or hard timing gate
is produced. Keep losing cases visible and use separate A/A runs to understand
noise before introducing CI regression thresholds (#216).

Artifacts:

- `manifest.json`: argv, binary hashes, declared build metadata, platform/clock,
  environment, seeds and exact corpus/oracle hashes.
- `workloads/*.bc` and `*.expected`: exact UTF-8 LF payload bytes matching the
  program/oracle hashes on every platform; reviewable programs and answers.
- `samples.jsonl` and `samples.csv`: every warm-up and measured observation,
  pairing/order, checked status, elapsed time and output hash/size.
- `summary.json`: per-case dispersion and paired comparisons, after success.

## Source and build provenance

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
all 39 quick workloads against that job's actual GD executable. Fake executables exercise failed exits, stderr, wrong output, warm-up validation,
timeouts, quoted argv, provenance requirements and timing/profiling separation.
Paired statistics use exact proportional samples to detect unpaired resampling;
repeated runs check schedule replay without asserting noisy elapsed times.

This corpus measures complete processes. Definition registration includes
parsing, validation and VM registration; decimal cases include the surrounding
interpreter/I/O work. They are not pure parser or arithmetic-kernel measurements.
The streaming case feeds sequential statements and checks every output line
through EOF. It measures bulk input/output throughput, including parsing and
execution; it does not assert interactive first-response latency.

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

The default `auto` preserves native calibration and its seeded input stream.
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
