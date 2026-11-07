# Experimental native scalar regions

`ENABLE_BC_NATIVE_SCALAR=ON` enables the prototype for Apple Arm64 builds.
The ordinary VM is the default. Other targets use the interpreter even if
the option is requested.

After full bytecode verification, a top-level input item can use the
prototype when it has a backward edge, fits in 64 instructions, uses a
supported subset of scalar operations, and begins with decimal input and
zero scale-zero values for every referenced scalar. The supported operations
are integer literals, scalar locations, loads, stores, unsigned checked
addition, unsigned less-than, and branches. The generated code works on
private scratch state and has a four-million-backedge budget. Output,
calls, arrays, fractional literals and unsupported bytecode use the ordinary
VM. Literal overflow, arithmetic carry, a denied executable mapping or a
spent budget also fall back.

The emitter writes a private writable page, switches it to read and execute,
then invalidates the instruction cache before entry. It releases the page
after one attempt. It generates code from verified instructions and never
loads a compiled program from disk. Scalar results are staged as ordinary
numbers before being published. If an interrupt is observed before commit,
the staged values are discarded and the ordinary interruption path runs.
Generated backedges poll the existing VM interrupt flag every 1,024
backedges. The emitter preserves the live registers and return address
across the callback. No native code is cached across function or
variable changes.

In a local matched Release ON/OFF screen, the checked million-iteration
scalar loop took 8.0 ms with the option versus 165.3 ms with it disabled.
Calls and arrays remained on the interpreter path at roughly parity. Each
case used eight alternating measured pairs following a warmup pair. This
screen has no independently rebuilt A/A role. It covers one host and a
small opcode subset, with function specialization and native compilation
of mixed numeric workloads still open.

A local PTY probe sent SIGINT 4 ms after submitting a long loop. All ten
trials per role recovered and processed subsequent input. The observed
signal-to-diagnostic medians were 0.089 ms with native polling and 0.060 ms
with native execution disabled. This samples one interruption point. It
does not establish a worst-case latency bound. The checked million-iteration
loop still took 7.77 ms versus 165.92 ms in eight matched measured pairs.

The dedicated Arm64 workflow checks that the candidate really includes the
backend and the two independently built controls omit it. It runs the CLI,
VM, signal and math suites on each image. A loop beyond the backedge budget
checks that interpreter replay commits the exact result once. The full and
holdout corpora are then timed in paired rounds, with raw observations and
build provenance retained as an artifact.

Issue #666 separately evaluates a [portable integer component](bc-portable-integer-regions.md)
for the same bounded scalar domain. Its initial checkpoint is linked only into
unit tests, without VM integration or performance evidence. It does not change
this Apple Arm64 emitter or inherit its qualification.
