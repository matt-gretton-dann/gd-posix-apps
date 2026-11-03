# Compact verified executable operands (#207, #197)

The candidate adds a separate execution stream after complete bytecode
verification. Each record holds an opcode, destination register and two
opcode-specific operands. Data offsets become absolute result-register indices;
branch offsets become absolute program counters. Variables, arrays and function
names retain their Letter values rather than re-decoding a wide operand variant
on each dispatch. Letter encoding does not assume contiguous character codes.

On the measured AppleClang 21 arm64 build, a source Instruction occupies 112
bytes, an executable instruction 16 bytes, and an executable operand 4 bytes.
The source instructions remain allocated: they own strings, mutable per-base
literal caches, call locations and definition masks/locations. This separates
hot dispatch records from side data; it does not reduce total instruction
storage to 16 bytes or establish an elapsed-time/RSS improvement.

## Ownership and control flow

A prepared outer stream owns its lowering and borrows the source instructions
only during execute(). A registered function owns both its source and lowering;
views are created on demand, so copying or moving a function cannot leave stored
pointers into a previous owner. No cache or source location points into a
transient caller's instruction vector. Each function uses its own index space.
Outer definition bodies are skipped, then verified and lowered independently at
registration. Compact definition metadata names the instruction after the body.

The compact loop uses absolute targets directly, including zero and the valid
one-past-end fallthrough. It advances the program counter only after ordinary
fallthrough, and polls for interrupts on each dispatch. It preserves argument
snapshots, special-register handling, numeric truncation and semantic errors.
The Number/array ownership kernels and load/store/print/length visitors are
shared with the reference interpreter. Last-use arithmetic transfers retain
the existing conservative ownership proof.

ExecutableOperand is a small union: the verified opcode determines whether its
active member is a register/control integer or a Letter. Lowering constructs
that member explicitly; dispatch reads only the corresponding member. Strings,
locations and masks remain in the source sidecar. This representation does not
accept public unchecked executable records: VM entry still verifies source
bytecode before constructing private prepared/function owners.

## Reference paths and scope

`ENABLE_BC_COMPACT_EXECUTION=OFF` keeps the existing verified interpreter.
`VM::ExecutionChecks::full` independently retains wide decoding and result
validation, even in a compact-enabled build. Empty or unrepresentable lowering
uses the generic interpreter. Tiny transient streams skip preparation; short
persistent functions are lowered because repeated calls can amortize the cost.
The candidate defaults ON so native CI actually measures it.

This experiment starts from accepted main 609d1e08, independently of rejected
register-reuse PR #436. It retains the accepted producer-only register layout.
It does not implement live-register reuse, typed numeric register pools,
superinstructions, threaded dispatch, cross-block optimization or native/JIT
compilation. Those parts of #207 remain separate work.

## Validation and acceptance

Focused lowering tests distinguish data-register references from control targets,
check immediate letters/streams, preserve short persistent functions, and keep
outer definition bodies separate. Independent checked/verified VM states and
literal caches exercise exact forward references, a finite loop branching to
instruction zero, function-owned literals and runtime ibase changes. Existing
numeric/property, alias, argument, recursion, redefinition, malformed-bytecode,
CLI, interrupt and lifetime tests remain required.

An initial local prototype incorrectly advanced the PC after taking an absolute
branch. Differential execution found the array-loop failure; the corrected loop
and explicit zero/forward-target tests precede this candidate's acceptance.
That prototype was never committed, pushed or merged.

Sanitizers and exact-output checks establish correctness only within their tested
scope. Charge lowering time and the additional code/metadata storage, then retain
full and held-out native Linux/Windows/macOS results, code size, allocations and
RSS. Linux/Windows allow up to 5% per-change slowdown under revised #197; macOS
requirements remain unchanged. Green workflow status alone is not performance
acceptance, and inconclusive controls cannot waive a confirmed loss. No overall
#207 or #197 completion is claimed.

## Local validation record

AppleClang 21, arm64, Debug with ASan/UBSan, fixed multiplication split point 126:

- Compact ON / extensions ON / divisor reuse OFF: 321 cases and 1,669,022
  assertions, plus 92 selected bc CTest entries: 47 executed passes, 45 declared
  skips, no failures.
- Compact ON / extensions OFF / divisor reuse ON: 321 cases and 1,669,007
  assertions; consumer-mode, VM lifetime/allocation, CLI and VM CLI CTests pass.
- Compact OFF / extensions ON / divisor reuse OFF: 321 cases and 1,669,022
  assertions, plus the same 92 selected entries (47 passes, 45 skips).

LeakSanitizer is unavailable here (`detect_leaks=0`); the isolated ordinary
allocation-balance test provides separate ownership evidence. These are distinct
configurations, not an exhaustive mode matrix or a pooled coverage result. The
coverage collector now records compact execution ON/OFF explicitly; an absent
cache option remains unknown for historical sources. Native three-platform
performance acceptance and fresh source-pinned LLVM coverage remain pending.
