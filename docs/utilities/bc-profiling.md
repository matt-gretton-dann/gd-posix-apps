# Separate bc diagnostic builds

`ENABLE_BC_PROFILE=ON` builds allocation and ownership counters for #200. Use a
separate build directory from release timings and Clang coverage. It is off by
default; ordinary builds retain their original standard allocator/container
types and direct shared allocation calls. The disabled backend test links
without counter storage to detect accidental counter dependencies.

```sh
cmake -S . -B build-profile -DCMAKE_BUILD_TYPE=Release -DENABLE_BC_PROFILE=ON
cmake --build build-profile --target bc
GD_BC_PROFILE_OUTPUT=/absolute/path/to/counters.json build-profile/src/bc/bc < case.bc
```

On Windows use the generated `Release/bc.exe` and set `GD_BC_PROFILE_OUTPUT` in
the process environment. The report uses the native wide environment and file
API for Unicode paths. Without that variable, the profiling build emits no
extra output. A requested report that cannot be opened or written causes a
failure diagnostic and nonzero process exit. Existing report files are replaced;
choose distinct output paths for each invocation.

The JSON explicitly marks `instrumented: true`. These runs perturb allocator
layout, locking and instruction counts: **never use their elapsed time as a
release-performance measurement**. The checked benchmark configuration must
identify their instrumentation and rejects them for timing. Keep the release
executable and its provenance separate from the diagnostic executable.

## Counter meanings and limits

The allocation backend uses `std::allocator` with stateless rebinding, preserving
alignment and portable node/control-block allocation. It does not replace global
`new`, intercept libc allocation, or use platform-specific allocation headers.
A portable `atomic_flag` guards the 64-bit totals, including on targets lacking
native 64-bit atomic arithmetic.

Categories cover digit vectors, recursive multiplication scratch and shared
control blocks, array dense-prefix buffers/sparse nodes/shared control blocks,
instruction vectors/shared control blocks, reusable parameter/local/frame
vectors, instruction-result vectors, decoded literal memo payloads, and
`execution_plans` byte buffers. Memo payloads account for their typed object
size at construction/destruction. Plan owner headers, and temporary ordinary
vectors used by verification/plan construction, are outside those byte counters.
Other byte counts are storage requested through these allocators; they exclude allocator metadata, strings, I/O buffers, other
containers and operating-system storage. They are **not process malloc traffic
or RSS**. `allocated_bytes` is cumulative traffic; `live_bytes` is currently
outstanding requested storage. `peak_live_bytes` for the total is the simultaneous
peak, not the sum of category peaks. Standard-library node/control-block sizes
may differ, so record the compiler and library with every run.

Small coefficients use inline limbs and do not request digit-allocator storage.
Their bytes are part of the containing Number/object: tracked frame/array vector
requests include their element sizes, while stack values are outside allocator
counters. Large coefficients retain heap COW storage. Fewer allocation calls do
not by themselves establish a lower process RSS or a smaller value object.

With `ENABLE_BC_INTEGER_FUNCTIONS=ON`, diagnostic builds execute the selected
integer tier. Its scratch vectors and immutable spill bank use `frames`, and
owned integer IR uses `execution_plans`; heap coefficients still use `digits`.
The `integer_function_*` events distinguish attempts, produced results,
abandonments, dispatched instructions, attempted self-calls, promotions and
demotions. Abandoned speculative work can be repeated by generic fallback, so
these counters are not semantic instruction/call totals. Existing generic VM
counters keep their interpreter-entry meaning. The logical 1 MiB guard differs
from retained capacity, selected-allocator peaks and process RSS; see the
[integer-tier contract](bc-pure-integer-functions.md#diagnostic-execution).

Events count BasicDigits copy/move construction and assignment, copies retaining
a non-null shared digit owner (`owned_digit_copies`), the historical
`iterator_owner_copies` event (now zero because mutable iterator checks borrow
the caller-owned vector), COW clones and payload bytes, copied iterator
range payload bytes, basic/Karatsuba/triangular-square calls, literal conversions, cache hits/misses and single-digit literal fast paths,
function calls, executed instructions and array clones/entries copied.
`temporary_consumptions` counts heap first-operand Number transfers after a
single-use/basic-block proof. The initial candidate also moved inline values;
its archived counters retain that historical meaning. A transfer does not by
itself prove a COW clone was avoided. Compare clone/payload counts and separate Release
timings; checked execution retains ordinary copies as a semantic oracle.
`radix_dc_input_calls` counts large integer input dispatches;
`radix_dc_input_leaf_conversions` counts their grouped leaf constructors.
`literal_conversions` includes those recursive text constructors, so it is not
a count of VM cache misses or source-level literals. The separate cache events
retain their VM meanings. Power caches are local to each conversion.
`comparison_limb_positions` counts aligned pairs visited by comparison,
`zero_scan_limbs` counts zero-predicate visits, and `length_scan_limbs` counts
leading positions inspected including the first significant limb. These are
logical software positions, not hardware memory loads or cycles. Length's
amount expression and all increments are removed in ordinary builds.
 These
are counts at the named source sites, not a hardware count of atomic operations.
Copies of empty allocated vectors still retain an owner and are counted.
Self-assignment and the library's shared-pointer implementation can affect actual
reference-count instructions. Pair the events with CPU profiles/disassembly
before attributing a fraction of runtime to ownership operations.

At normal EOF, main's VM and parser are destroyed before the registered report
runs: tracked storage should be released. A fatal `std::exit` can bypass local
object destruction; outstanding storage on that path is not evidence of a leak.
A crash or forced termination may produce no report. Check process status and
expected calculation output independently before accepting any profile.

The report also records limb width/base and the configured multiplication split
point. The tuner remains uninstrumented so counter overhead does not choose an
arithmetic threshold. CPU sampling, code size and process RSS require separate
platform measurements; unavailable measurements must remain explicitly unknown.

## Validation

`bc/profiling-allocator` checks vector lifetime, map rebinding, shared ownership,
over-alignment and balanced storage. `bc/profiling-disabled` verifies the
ordinary allocator type and links without the backend. A profiling build also
registers `bc/profiling-cli`, checking real arithmetic, COW isolation, array
cloning, frames, Unicode report paths, EOF cleanup and report failures. Native
Linux, macOS and Windows CI builds the diagnostic mode separately from the
ordinary extension ON/OFF release tests.


For matched release/diagnostic attribution, pass the same explicit
`-DBC_MULTIPLY_SPLIT_POINT=126` to both configurations. The report records the
compiled point, and `test-profiling-cli.py --split-point 126` checks that it is
the requested value. Native CI uses this fixed diagnostic configuration while
ordinary builds continue to exercise automatic calibration.

The square-root experiments also record `sqrt_iterations` and
`sqrt_working_scale_sum`. The latter sums decimal working scales over Newton
steps; divide it by the iteration count only when a mean is useful. It is not
an allocation size or a maximum. Zero and rejected negative roots take no steps.

`division_requests`, `division_dividend_limbs` and `division_divisor_limbs`
record Number division requests after aligning operand scales and scaling the
numerator for the requested quotient. The limb amounts are stored coefficient
lengths before comparison and integer-division normalization. They include
requests resolved by comparison; they do not count only general multi-limb
kernel dispatches, normalization workspace, the Newton averaging divide-by-two,
CPU work or process RSS. Isolate sqrt from other division expressions when
attributing request totals to Newton steps. General division workloads can also
use these dimensions. All counters are diagnostic-only and their amount
expressions are discarded in ordinary builds.


## Checked release CPU and memory diagnostics

`src/bc/scripts/profile-bc.py` accepts the same source-built release config as
`bench-bc.py`. Choose explicit workload names and provide the checked timing
summary used only to estimate CPU sampling repetitions:

```sh
python3 src/bc/scripts/profile-bc.py --config release-implementations.json \
  --implementation gd --summary timed/summary.json --output diagnostics \
  --suite math --replay-corpus timed --workload WORKLOAD_NAME
```

The default suite is `arithmetic`. Without `--replay-corpus`, the collector
regenerates the full suite; replay instead verifies and uses the exact archived
programs and oracles, including calibrated iteration counts. Mathematical
outputs use the independent decimal-ULP enclosures and check the result scale
and caller state for every repetition. Arithmetic retains exact output checks.
Any incorrect output, diagnostic stderr, failed child or deadline rejects the
measurement. Unsupported native adapters retain an explicit unavailable status.

The archive contains the original LF program, exact expected-output or enclosure
payload, their SHA-256 identities, replay manifest identity, declared library
inputs, and the expanded CPU input hash and byte count. Declared inputs are
rechecked before each launch. File workloads receive a temporary LF source file
and empty stdin; stdin workloads retain their original transport. Repeating a
program for CPU sampling happens in one interpreter, with startup and any
library argv file evaluated once. This measures repeated execution and parsing,
not repeated cold startup. RSS runs the original workload once. Neither CPU
samples nor native RSS observations replace the separate paired release timing
experiment; software allocation counters remain a separate instrumented build.


The [coefficient-prefix square-root experiment](bc-sqrt-prefix.md) distinguishes
fractional working scale from significant coefficient width. Integer-prefix
Newton requests contribute zero to `sqrt_working_scale_sum`; their work is
still counted by `sqrt_iterations` and division dimensions. New
`sqrt_precision_stages` and `sqrt_root_digits_sum` counters describe stage count
and per-iteration root-coefficient precision. They are zero on the legacy path.
These diagnostics do not substitute for paired timing or native RSS.

`sqrt_precise_seed_fallbacks` counts large-root requests whose exact input
residual shows that the existing seed is already precise enough to retain the
original path. It does not count Newton iterations or claim an elapsed saving.

## Immutable diagnostic recipes

`build-gd-bc.py --source REPOSITORY --revision FULL_SHA --split-point 126
--diagnostic-profile --output diagnostic-recipe` builds the same immutable
source with software counters enabled. Its metadata always says
`instrumented: true`; the checked elapsed harness rejects that configuration.
Ordinary recipes still require counters and compiler instrumentation disabled.
Only this explicit diagnostic mode permits the counter macro; coverage,
sanitisers and compiler profile generation remain rejected by this builder.
The native mathematical workflow builds this separate recipe before any timing
and discards instrumented elapsed values while preserving checked outputs and
logical counter reports.

Native resource reports retain CPU accounting separately from elapsed samples.
Darwin parses the user/system fields printed by time-l; GNU time emits explicit
USER_SECONDS and SYSTEM_SECONDS records. Missing or malformed CPU fields stay
unavailable even when RSS exists, and valid zero CPU times retain their printed
precision limitation. RSS units and optional hardware counters keep their native
meanings. Unsupported hosts retain an explicit unavailable result; this adapter
does not infer CPU time from wall-clock duration.

On Windows, the standard-library `ctypes` adapter reads the kernel's peak
working set and peak commit charge in bytes through
[K32GetProcessMemoryInfo](https://learn.microsoft.com/en-us/windows/win32/api/psapi/nf-psapi-getprocessmemoryinfo).
These are distinct counters; commit charge is not resident memory. The adapter
queries the actual child's retained CPython `Popen` process handle after the
child finishes, rather than reopening a possibly reused PID or polling current
working set. A native test touches and releases 64 MiB before exit to check that
the peak remains available. The process handle stays owned by `Popen`.

[GetProcessTimes](https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-getprocesstimes)
supplies user/kernel CPU counters. Reports retain the raw 100-nanosecond ticks
and their conversion to seconds; the operating system's accounting resolution
can be coarser than that unit. Zero CPU counters remain valid. API failures
retain their Windows error code independently for memory and CPU accounting;
Python implementations without the process handle retain explicit unavailable
data. No external Python package or change to the measured interpreter is
required. Counts cover this process, not a descendant process tree.

Windows raw counter observations use `.rss.json`; Linux/macOS keep their native
`.rss.txt` reports. Every supported adapter still checks exit status, stderr,
all numerical output and caller state before accepting an observation. These
cold resource observations are separate from paired release elapsed samples,
software allocator counters and cross-platform performance acceptance.

On Linux, add `--perf /absolute/path/to/perf` to capture userspace software
CPU-clock samples for the same checked workload. The selected executable must
be able to open the event under the caller's existing permissions. The
collector runs `perf record` and `perf script` with bounded child processes.
It checks the interpreter's status, stderr and every expected output before
decoding the trace. The diagnostic directory retains the raw perf data,
decoded samples, decode diagnostics and their data identities in the manifest.
An explicit perf request fails if collection or decoding fails. Without this
option, Linux still collects the available RSS and CPU accounting diagnostics.
