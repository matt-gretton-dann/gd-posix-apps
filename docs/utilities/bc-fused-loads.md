# Fused location/load dispatch (#207, #197)

This follow-up to compact executable operands evaluates a pair operation for a
location immediately followed by its load. It handles scalar variables, whole
arrays, ibase, obase and scale. `ENABLE_BC_FUSED_LOADS=ON` selects pair lowering;
OFF retains ordinary compact dispatch in the same representation. Disabling
compact execution retains the wide interpreter, and full checked execution
always supplies an independent reference.

## Exact source results and entry points

The pair writes both original registers, in order: the location first, then its
loaded snapshot. It retains the normal load record at the second source PC.
Thus later stores may still name the location, and a backedge may enter only
the load using a previously established location. No result-use count or ban on
branches into the second instruction is assumed. Only a load with offset -1
qualifies; nonadjacent loads keep their actual producer and ordinary dispatch.

The pair uses the location opcode's otherwise unused second word for the load's
destination. Dedicated pair opcodes share the same 32-bit dispatch field as
ordinary executable operations, with 32-bit result and operand words. Source
and executable opcode spaces have exhaustive conversion switches, so source
schema additions must update both formats. source_opcode() recovers the original
source operation for planning and diagnostics; the hot loop dispatches executable
opcodes directly. No bit-field packing or character-code arithmetic is required.
Locations and loaded values retain distinct producer registers.

The interpreter executes both halves before advancing PC by two. It keeps the
load suffix executable independently. A dispatch polls for interruption before
the pair; two bounded location/load operations may occur between polls.
Logical-instruction profiling counts both source operations. Numeric copies,
array snapshot ownership and special-register reads use the same VM operations
as the unfused visitors. Ordinary runtime state is read when execution reaches
the pair, never frozen during lowering.

## Tests and measurement boundaries

Focused tests check all five pair kinds, both result indices, preserved suffix
metadata, the OFF lowering path and nonadjacent references. A differential
execution oracle first runs a fused pair, stores through its retained location,
then branches directly into its load suffix; the exact output is 0 then 1.
Separate mixed-value execution checks scalar/special/array alternatives.
Existing checked/verified numeric, alias, argument, recursion, redefinition,
CLI, signal and allocation/lifetime tests remain required.

The first compact layer's performance results belong to its original source,
not this follow-up. Compare the new pair source against accepted main and also
compare fused ON/OFF with the same record format to expose dispatch overhead.
Record code size, preparation work and memory as well as full/held-out runtime.
Source bytecode remains allocated; pair fusion does not imply total memory
savings. Coverage metadata records the fusion switch explicitly. Native
acceptance remains pending, with the revised #197 Linux/Windows 5% allowance and
unchanged macOS requirement. No overall #207 or #197 completion is claimed.

## Initial flag-dispatch checkpoint

The following validation belongs to d8f5312f, whose first representation used a
16-bit opcode plus a 16-bit fusion kind and checked that kind before each opcode
dispatch. Local paired timing rejected that source: loops slowed about 9.1%,
calls 4.4% and arrays 5.4% against accepted compact execution. Exact programs,
oracles and all slower samples remain recorded in PR #438. Its native run was
left running to preserve the original evidence.

The single-table revision replaces the extra flag test with dedicated pair
opcodes in one dispatch table. Its validation and measurements are independent;
no speedup is inferred from that structural change.

AppleClang 21 arm64, fixed multiplication split point 126:

- ASan/UBSan, fused and compact ON, extensions ON, divisor reuse OFF:
  325 cases / 1,669,093 assertions.
- ASan/UBSan, fused and compact ON, extensions OFF, divisor reuse ON:
  325 cases / 1,669,078 assertions.
- ASan/UBSan, fusion OFF with compact ON, and compact OFF with fusion ON,
  each with extensions ON/reuse OFF: 325 cases / 1,669,093 assertions.
- Consumer-mode, allocation/lifetime, CLI, VM CLI and coverage-metadata CTests
  pass in each of these configurations.
- Final fused default configuration: 92 selected bc entries, 47 completed
  successes and 45 declared skips, zero failures, including signal delivery.
- Separate Debug profiling build, fusion/compact/extensions/reuse ON:
  335 cases / 1,669,155 assertions. The logical-instruction counter reports
  exactly 12 source instructions in both checked and fused suffix-entry tests.

The layout probe still reports a 16-byte executable record, comprising a
2-byte opcode and 2-byte fusion kind plus the existing result/operand words.
This is a host layout measurement, not an elapsed-time improvement. Profiling
builds are separate from timing builds. LeakSanitizer is unavailable on this
host; ASan used detect_leaks=0, with separate ordinary-allocation balance checks.
Native source-pinned performance and coverage acceptance remain pending.

## Single-table revision validation

The initial d8f5312f loop slowdown reproduced locally: candidate/base median
1.090901 initially and 1.092040 in a fresh nine-pair confirmation, whose nominal
95% interval was [1.089166, 1.097373]. Native CI separately retained 19,800 checked
observations across three hosts with zero confirmed comparison losses. Those
favorable hosts do not waive the reproduced local macOS regression.

The revised dispatch passes the same four ASan/UBSan configurations and exact
case/assertion counts listed above, all five focused CTests in each mode, and
the broad default suite (92 entries: 47 successes, 45 declared skips). Its
separate profiling build passes all 335 cases / 1,669,155 assertions, including
logical source-instruction counts and independent suffix entry.

The revised layout probe reports source Instruction 112 bytes, executable record
16 bytes, operands 4 bytes each and a single 4-byte executable opcode. Exhaustive
source/reverse conversions are preparation-time operations; the hot loop uses
one executable-opcode switch. Fresh immutable timing, native and coverage
evidence is required before accepting this source.
