# Experimental operand-stack execution

`ENABLE_BC_STACK_EXECUTION=ON` enables a portable scalar stack executor.
The verifier still runs before lowering. The planner accepts only when
every read matches the live stack suffix and the stack is empty at all
control-flow entries and exits. Fixed scalar locations are encoded in
loads and stores. A binary instruction records its operand order so
subtraction, division and comparisons keep their original semantics.

The interpreter uses the existing Number arithmetic and VM scalar and
parameter APIs. Function bodies cache an accepted or rejected stack plan
with the verified definition, so redefinition discards the plan. Local
variables, recursion and error cleanup use the ordinary call machinery.
Arrays, unsupported instructions, full-check execution, and prepared
divisors use the register executor.

The CLI loop item lowered from 22 source instructions to 17 four-byte
instructions, with two live stack values. The caller loop lowered from
25 to 20 instructions, also with two live values. Streaming's small CLI
items and the sparse-array loop did not use the stack path. A local
matched screen after enabling scalar function bodies measured calls at
0.965 and 0.957 times baseline on full and holdout, and recursion at
0.909 on holdout. An earlier screen without function-body lowering put
the scalar loops near parity. These are local checked observations with
no independent A/A rebuild. The option defaults off.
