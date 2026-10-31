# bc VM and diagnostic tests

The isolated `gdsup/signal-first-action` test checks first-request preservation
and previous-action output in the signal API, before VM-specific restoration is
involved. Windows console-dispatch initialization must precede storage of the
requested action. Native POSIX sigaction remains the reference path; unsupported
facilities report an explicit CTest skip.

`test-token-format.cc` asserts token type names, source spellings, payload/debug
output and all operator classifications. Each positive operator case is paired
with the other token kinds, providing short-circuit independence witnesses for
the classification predicates. Runtime `vformat` calls check the empty/default
specification and rejection of a nonempty specification for tokens and all 26
letters with either standard-library formatting or supplemental fmt.

`test-instruction.cc` checks every instruction's diagnostic spelling with valid
operand types, stream destinations, complete instruction listings, mutable
operands and missing-operand exceptions. Variable/array masks use independent
namespaces: equality changes each mask separately, and empty, sparse, repeated,
last-letter and all-letter fixtures assert iteration order and diagnostic text.
These checks run with ordinary test output; enabling Catch's per-success logging
is unnecessary for production diagnostic coverage.

A temporary classification mutation replacing the final relational operator
with EOF is rejected by the token assertions. This is a fault-detection check,
not a production mode or a committed source variant.

These tests cover diagnostics and public value contracts. They do not establish
execution of every VM opcode, validate malformed bytecode or replace the direct
VM, subprocess runtime-error, function-frame, interrupt and verifier tests tracked
by #215 and #187. Coverage reports must retain those open obligations and the
separate extension configurations described in [bc-testing.md](bc-testing.md).

`test-format.cc` also calls each public token/letter formatter parser with an
empty parse context. The runtime formatting engine normally supplies a closing
brace even for `{}`, so testing `vformat` alone misses this accepted boundary.
The direct parser assertion complements the existing accepted/rejected format
specification cases without changing formatter implementation.

Function argument extraction checks the stored alternative before reading a
scalar number or whole-array value. A scalar/array mismatch is a source-level
runtime error with an expected-type diagnostic, rather than an uncaught variant
exception. The CLI regressions exercise both directions in isolated children.
This check does not replace the broader bytecode verifier contract in #187.

`bc/vm-cli` runs isolated, bounded runtime checks in both extension modes. It
asserts numeric domain/conversion errors, ibase bounds, binary/hex output,
undefined functions, argument count/type errors, recursion and argument order,
locals/redefinition, default and `-g` special-variable behavior, sparse defaults
and runtime ibase changes for a reused function literal. Each process must have
the expected status/stdout and an appropriate empty/nonempty diagnostic stream.
These cases gate the literal-cache, frame and array optimizations; they do not
replace malformed-bytecode verification or the separate interrupt protocols.
