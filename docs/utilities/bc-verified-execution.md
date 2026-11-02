# Verified execution and the checked oracle

Every externally supplied instruction pack passes the existing operand schema,
offset, result-kind, control-flow availability and parameter-stack verifier
before execution. A borrowed `VerifiedInstructions` view records that transition;
only the verifier or a persistent verified function owner can construct it.
The executor accepts this view rather than an unchecked instruction vector.
The source retains the existing borrowed lifetime contract.

Function bodies are verified before registration and stored in `FunctionCode`,
which exposes a read-only verified view. Redefinition replaces the owner and
its literal caches. The verified view changes no numeric decoding, runtime
ibase behavior or function argument semantics.

`VM::ExecutionChecks::full` retains the original per-instruction result diagnostic
switch and remains the public C++ constructor's default. The executable selects
`verified`, which omits that second switch after entry verification. The mode
choice occurs outside the instruction loop. Runtime semantic checks, including
division by zero, invalid bases, undefined functions and interrupts, remain.
Both modes retain operand/result variants and their access checks. Result
slots now represent emptiness as a variant alternative, replacing the outer
optional engagement flag. This keeps one discriminant per slot, including
instructions with no result. It uses ordinary portable C++ without an ABI or
CPU assumption; exact byte savings depend on the compiler and standard library.
The storage allocator still reports actual bytes, separately from process RSS.

Activation release destroys every owning alternative before retaining buffer
capacity. Repeated mixed number/array/string packs exercise that reset in both
modes, alongside recursive snapshots and the existing lifetime regression.
Empty-result diagnostics remain checked before visiting a slot. Operand
compaction and fused instructions remain separate experiments.

Direct execution tests compare checked and verified modes with independent
literal caches and VM state. Generated grammar/bytecode/native oracles and
branch/function/register tests exercise both modes. Malformed offset, type,
control-flow and marker fixtures require identical diagnostics and rejection
before an observable prefix can execute. Separate profiling counts prove that
the second switch executes in the checked oracle and is absent in verified mode.

These are incremental #207 stages. Numeric slot specialization, operand compaction,
liveness/fusion and dispatch alternatives remain separate measured decisions.
Keeping two executor instantiations may increase code size; paired benchmarks
must include small scripts and startup as well as hot loops.
