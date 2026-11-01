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
Both modes still use the existing operand and result variants and their access
checks; this stage does not compact them or introduce fused instructions.

Direct execution tests compare checked and verified modes with independent
literal caches and VM state. Generated grammar/bytecode/native oracles and
branch/function/register tests exercise both modes. Malformed offset, type,
control-flow and marker fixtures require identical diagnostics and rejection
before an observable prefix can execute. Separate profiling counts prove that
the second switch executes in the checked oracle and is absent in verified mode.

This is the first #207 stage. Numeric slot specialization, operand compaction,
liveness/fusion and dispatch alternatives remain separate measured decisions.
Keeping two executor instantiations may increase code size; paired benchmarks
must include small scripts and startup as well as hot loops.
