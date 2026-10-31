# bc VM and diagnostic tests

The isolated `gdsup/signal-first-action` test checks first-request preservation
and previous-action output in the signal API, before VM-specific restoration is
involved. Windows console-dispatch initialization must precede storage of the
requested action. Native POSIX sigaction remains the reference path; unsupported
facilities report an explicit CTest skip.

## Interrupt protocols

The VM temporarily installs its SIGINT handler while executing a stream, then
restores the caller's complete signal action before reporting interruption.
Scoped restoration also runs when execution propagates an exception, such as
an output-stream failure. If installation is unsupported, the caller's existing
signal behavior remains the fallback. The shared interruption flag is an
always-lock-free `std::atomic_flag`, usable by POSIX signal handlers and by the
separate Windows console-handler thread.

`bc/signal-normal` and `bc/signal-exception` are isolated native processes which
assert handler, flags and mask restoration; the exception case also checks that
the exception propagates. A setup assertion distinguishes a signal API which
loses its first requested action from a VM restoration defect. Unsupported
signal facilities report explicit CTest skips.

`bc/signal-delivery` uses a POSIX PTY. A printed readiness marker inside the same
execution item as an infinite loop proves handler installation before SIGINT.
It interrupts two successive items, checks both diagnostics, then observes the
result of `2+3` while stdin remains open before sending quit. Reads and child
lifetimes are bounded, and children are killed/reaped on failures. The POSIX PTY
facility is explicitly skipped where unavailable; native restoration tests
remain independent of actual signal delivery.

## Value and diagnostic checks

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

## Operand-offset boundaries

`bc/verifier-offsets` runs valid-schema instruction fixtures in isolated children.
Invalid operand-1/operand-2 result offsets, minimum signed offsets and array-index
references must be rejected before an observable prefix executes. A data offset
must name a result inside the stream. An unconditional branch target, or the
second operand of `branch_zero`, may also name the end of the stream: interactive
parser items use that boundary for fall-through. Positive end/last-result cases
protect this distinction.

Offset underflow checks avoid negating the minimum signed value. This range
validation is only the boundary stage of #187; typed-result/control-flow joins
and parameter-marker verification remain required before code can be treated as
fully verified or its runtime checks specialized.

`bc/vm-cli` runs isolated, bounded runtime checks in both extension modes. It
asserts numeric domain/conversion errors, ibase bounds, binary/hex output,
undefined functions, argument count/type errors, recursion and argument order,
locals/redefinition, default and `-g` special-variable behavior, sparse defaults
and runtime ibase changes for a reused function literal. Each process must have
the expected status/stdout and an appropriate empty/nonempty diagnostic stream.
These cases gate the literal-cache, frame and array optimizations; they do not
replace malformed-bytecode verification or the separate interrupt protocols.

## Result-type preflight

After checking every offset bound, the verifier classifies each instruction's
result and checks operand references before executing any part of the stream.
Arithmetic, comparisons, conditional branches, indexes and return values require
numbers; load/store require compatible scalar or array locations and values.
Length accepts a number or an array location, and parameters accept numbers or
whole-array values. Print deliberately accepts every result alternative for
instruction diagnostics, but cannot refer to a result-less instruction.

`bc/verifier-offsets` includes isolated malformed-result cases with an observable
prefix which must never execute, as well as a valid forward-layout producer and
location printing. Type classification is independent of layout and does not
assume that every valid result reference is backward. Result types alone do not
establish availability; the control-flow preflight below checks function regions
and dominance, then verifies parameter-marker states along execution paths.
Instruction constructors retain their existing operand-schema preconditions.

## Function regions and result availability

The control-flow preflight matches each nonnested function definition with its
end metadata before any output or registration executes. Branches and result
references stay in their execution region. EOF is outer-stream termination and
must be the last instruction; function-end metadata is a body fall-through
sentinel, never an executable instruction. Copied function bodies are checked
with their function context retained.

Every reachable result consumer requires a producer which strictly dominates
it: every path from that region's entry executes the producer first. Forward
layout is accepted when this holds, including a branch through a later producer
and back to an earlier consumer. Results from an assumed previous loop iteration
do not justify a first read. Unreachable consumers still have their offset,
type and region contracts checked, but cannot execute an unavailable read.

`bc/verifier-offsets` rejects skipped producers, invalid joins, self references,
function-region crossings, malformed definition boundaries and misplaced EOF
before its observable prefix. Positive forward-layout and branch-join programs
assert their output. A seeded bytecode property suite generates small cyclic
control-flow graphs and compares verifier acceptance with an independent oracle:
removing a dominating producer disconnects every path to its consumer. The
oracle uses reachability rather than the implementation's dominator algorithm.
Generation is deterministic (seed `197187`) and does not execute arbitrary loops.
A temporary bypass of availability checking is rejected by 602 of the 1,280
property assertions; restoring the check returns all cases to passing.

The implementation uses iterative graph/tree traversals and linear auxiliary
storage. The following marker pass checks the remaining activation protocol;
these verifier checks alone do not establish the complete #215 coverage goal.

## Parameter-marker protocol

The outer stream starts with no argument packs. A function starts with one
incoming caller pack, consumed exactly once by its parameter prologue. Parameter
extraction is valid only before that consumption, with no nested owned pack.
Each `push_param_mark` creates an owned argument pack; parameters require such a
pack and `call` consumes it. A return, fall-through, EOF or quit cannot leave an
owned pack outstanding or an incoming pack unconsumed.

Every control-flow join requires identical marker states. This rejects growing
or shrinking packs around loops and paths which bypass or repeat prologue
consumption. Cycles before incoming-pack consumption are rejected; ordinary
function loops after the prologue remain valid. Checks are path-sensitive:
one mark may feed either of two mutually exclusive call instructions, so raw
global instruction counts are not a substitute for execution balance.

The isolated verifier suite checks missing marks, underflow, invalid joins,
uncalled malformed definitions, repeated/bypassed prologues, pre-prologue cycles
and owned-pack accumulation. Its positive branch fixture asserts two alternative
call sites sharing one mark. The direct array test now uses an actual declared
function and activation: it asserts the returned array element, restoration of
the local array and preservation of the caller's array. Argument count/type
mismatches remain checked source-level runtime errors, independent of this
structural protocol. Constructor operand-schema preconditions remain unchanged.

## Number formatter boundaries

The runtime number-formatter table covers Traits8/16/32 with exact normal and
debug strings for positive and negative values. It checks invalid specifications
through `vformat` and directly exercises empty, invalid and unterminated public
parse contexts. Ordinary `vformat("{}", ...)` supplies a closing brace, so it
does not replace the empty-context assertion. Both standard formatting and the
supplemental fmt backend run the same expectations.

A temporary mutant returning ordinary text for debug formatting is rejected at
each trait's debug-string assertion. The committed implementation is unchanged;
these are diagnostic contract tests, not a production formatting mode.

Numeric runtime tests assert diagnostic routes as well as status and output.
Negative assignment fixtures separate `=` from unary `-`; POSIX leaves the
historically ambiguous `=-` adjacency undefined, so a lexer rejection of that
form must not stand in for a numeric conversion error. Fractional-power cases
require the existing diagnostic to show the actual positive/negative exponent,
not the unrelated base operand. Fractional powers remain rejected.

## Verifier decision pairs and direct array length

The coverage follow-up calls the control-flow verifier with its documented
schema/type/offset prerequisites satisfied. Isolated function-body cases reject
nested definitions, EOF and extracting/consuming an incoming parameter pack
while an owned pack is active. A valid acyclic diamond before prologue
consumption checks an indegree decrement that does not yet reach zero.
Missing function ends and conditional branches across execution regions have
specific failure assertions and a valid conditional-branch counterpart.

An operand-2 fixture keeps the first producer dominating while a branch skips
the second; changing only that branch restores validity. The subprocess suite
also rejects a numeric store destination before its observable prefix can run.
A direct VM array-length test checks untouched and sparse arrays with either
frontend extension mode, including OFF where its source spelling is rejected.
These tests distinguish the instruction API from the frontend language gate.

Debug ON and Release OFF each pass 26 targeted assertions in five cases and
all six bounded offset-test methods. Temporarily disabling operand-2 dominance
makes its new fixture fail; restoring the implementation passes again. No
production mutation is committed.

## Exact VM execution and generated expressions

`test-vm-execution.cc` directly executes all ten binary arithmetic/comparison
opcodes over signed operands -4 through 4, using exact native integer results.
Zero divisors and negative exponents remain in the separate runtime-error and
scaled arithmetic suites. Unary cases assert negate, positive/negative abs,
perfect-square sqrt, scale and length. Conditional and unconditional branches
must select exactly one string; register stores must survive a second
instruction pack, including output-radix conversion.

The property test uses seed `197215` to generate 200 bounded expression trees
with signed leaves, addition, subtraction and multiplication. Each tree runs
as independently assembled bytecode and as parsed source. Both must match the
native integer oracle, with source, seed and sample reported on failure. Depth
three and leaves -9 through 9 keep every intermediate well within `int64_t`;
this does not use the bc arithmetic implementation to compute expectations.
Both executions use the currently validated VM. A future optimized execution
mode (#207) must run these same oracles; no optimized mode exists yet.

A temporary production mutation dispatching addition to subtraction fails 296
assertions in the direct and generated cases. Restoring the source passes all
3,704 assertions across five cases. The mutation is not committed. These tests
supplement the function/array, verifier, bounded CLI and signal suites; they do
not by themselves prove complete VM branch or MC/DC coverage (#215).

The diagnostic-print test additionally executes every supported result
alternative on both stdout and stderr: strings, numbers, variables, arrays,
array elements, loaded array values and the three special register locations.
The public verifier explicitly allows these diagnostic operations. Assert the
complete selected stream and an empty unselected stream; do not manufacture an
invalid result merely to reach a visitor. With this case, the execution group
passes 3,710 assertions across six cases in both configurations.
