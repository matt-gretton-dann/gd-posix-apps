# Experimental operand-stack execution

`ENABLE_BC_STACK_EXECUTION=ON` enables a portable scalar stack executor.
The verifier still runs before lowering. The planner accepts only when
every read matches the live stack suffix and the stack is empty at all
control-flow entries and exits. Fixed scalar locations are encoded in
loads and stores. A binary instruction records its operand order so
subtraction, division and comparisons keep their original semantics.

The interpreter uses the existing Number arithmetic and VM scalar and
parameter APIs. Function bodies cache an accepted or rejected stack plan
in a separate table keyed by function name. Redefinition invalidates the
entry. The plan table and operand pool follow the existing VM fields
to preserve the register executor’s field offsets. Local
variables, recursion and error cleanup use the ordinary call machinery.
Arrays, unsupported instructions, full-check execution, and prepared
divisors use the register executor. Register calls retain their usual
scalar-parameter prologue where supported. A stack call executes the
original parameter instructions itself. If the experimental Apple Arm64
native scalar backend is also enabled, it gets first refusal on CLI
items and can fall back to the stack or register executor.

The CLI loop item lowered from 22 source instructions to 17 four-byte
instructions, with two live stack values. The caller loop lowered from
25 to 20 instructions, also with two live values. Streaming's small CLI
items and the sparse-array loop did not use the stack path. A local
matched screen after enabling scalar function bodies measured calls at
0.965 and 0.957 times baseline on full and holdout, and recursion at
0.909 on holdout. An earlier screen without function-body lowering put
the scalar loops near parity. These are local checked observations with
no independent A/A rebuild. Native builds default to the stack executor for eligible scalar code.
Set `ENABLE_BC_STACK_EXECUTION=OFF` to use register dispatch throughout.
Coverage, profiling, cross builds and prepared-divisor configurations
default to register dispatch.

The first [same-image Linux comparison](https://github.com/matt-gretton-dann/gd-posix-apps/actions/runs/41828805835)
reproduced the stack loop and high-comparison losses on EPYC 7763.
Sparse arrays remained near parity in both modes. An
[in-place operand follow-up](https://github.com/matt-gretton-dann/gd-posix-apps/actions/runs/41829468608)
on EPYC 9V74 checked 480 observations. Stack/register ratios were
0.897 for calls in both presets, 0.925/0.911 for recursion, and
0.966/0.963 for the loop. The full-loop and high-comparison intervals
were wider than their holdout counterparts. Both modes used the same
timing executable within each run. The hosts differed between runs, so
the change across runs does not isolate the effect of the code edit.

The next lowering combines an adjacent scalar comparison and conditional
branch when the branch has no separate entry. It consumes the same two
operands without constructing a numeric Boolean or dispatching a second
instruction. Non-fused comparisons still produce a numeric result.

The operand pool experiment leases a vector for each active stack depth.
A lease clears its values on return, interruption or exception, while
retaining capacity for the next execution at that depth. Deque growth
preserves active vectors across recursive calls. Retained capacity
therefore follows the largest operand count used at each visited depth
over the VM's lifetime.
