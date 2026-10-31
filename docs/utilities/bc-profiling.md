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

Categories cover digit vectors/shared control blocks, array map nodes/shared
control blocks, instruction vectors/shared control blocks, parameter/local list
nodes, and instruction-result vectors. Byte counts are storage requested through
these allocators; they exclude allocator metadata, strings, I/O buffers, other
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

Events count BasicDigits copy/move construction and assignment, copies retaining
a non-null shared digit owner (`owned_digit_copies`), the historical
`iterator_owner_copies` event (now zero because mutable iterator checks borrow
the caller-owned vector), COW clones and payload bytes, copied iterator
range payload bytes, basic/Karatsuba multiply calls, literal conversions,
function calls, executed instructions and array clones/entries copied. These
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
