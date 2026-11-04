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
The prototype does not poll for interrupts inside the generated region, so
latency is bounded by the backedge budget and instruction cap. No native
code is cached across function or variable changes.

In a local matched Release ON/OFF screen, the checked million-iteration
scalar loop took 8.0 ms with the option versus 165.3 ms with it disabled.
Calls and arrays remained on the interpreter path at roughly parity. Each
case used eight alternating measured pairs following a warmup pair. This
screen has no independently rebuilt A/A role. It covers one host and a
small opcode subset, with function specialization and native compilation
of mixed numeric workloads still open.
