# Experimental native scalar regions

`ENABLE_BC_NATIVE_SCALAR=ON` enables bounded native scalar execution on Apple
Arm64 and x86-64 POSIX/Windows builds. The ordinary VM remains the default.
Unsupported targets, 32-bit builds and Apple universal builds use the
interpreter even if the option is requested. The x86-64 extension is an
unqualified experiment; its presence does not establish a performance gain.

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

The shared eligibility proof applies to both emitters. The x86-64 backend
uses volatile registers under the System V and Windows ABIs and preserves
all live values across its nonthrowing interrupt callback. Its stack frame
maintains sixteen-byte call alignment and reserves the Windows callback's
32-byte home area. Windows registers runtime function/unwind information
before entry and removes it before releasing the page. A failure to register,
protect or flush code falls back to the interpreter. No exception may propagate
through a generated frame. POSIX generated frames have no registered DWARF
unwind information; profiling tools may stop at them.

Separate profiling builds record VM `native_scalar_attempts` and
`native_scalar_completions`. A completion counts a successful kernel result
before ordinary Number staging, which can still fail or be interrupted.
These counters do not count native instructions. Emitter buffers, executable
pages and operating-system unwind bookkeeping are outside the selected
container allocator counts; ordinary cold RSS remains a separate observation.

Direct native tests exercise arithmetic, branches, caller restoration across
polls, unsigned overflow fallback, fuel exhaustion and interruption. Checked
and verified VM results are compared independently. Windows tests also look up
the callback's native return address in the runtime function table and capture
a stack trace across the generated frame. Native x86-64 execution under Rosetta
is local correctness evidence, not validation of Linux or Windows ABIs.

References: [Windows x64 calling convention](https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention),
[runtime unwind tables](https://learn.microsoft.com/en-us/cpp/build/exception-handling-x64),
[executable-page protection](https://learn.microsoft.com/en-us/windows/win32/api/memoryapi/nf-memoryapi-virtualprotect)
and [Intel instruction reference](https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html).
The emitted x86 instruction encodings are also checked against LLVM's assembler
during development; execution oracles establish their combined semantics.

## Historical Apple Arm64 measurements

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

The earlier dedicated Arm64 workflow checks that its candidate really includes the
backend and the two independently built controls omit it. It runs the CLI,
VM, signal and math suites on each image. A loop beyond the backedge budget
checks that interpreter replay commits the exact result once. The full and
holdout corpora are then timed in paired rounds, with raw observations and
build provenance retained as an artifact.
