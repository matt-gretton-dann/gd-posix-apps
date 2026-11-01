# Exact current-main attribution — hold remains

A pre-bounded 27-pair batch compares accepted main 0868b4a, historical a5c1cb1,
scalar conversion bd002d4 and pinned Howard in the same serial schedule. All
224 invocations per preset pass exact output/status/stderr/deadline checks.
The historical parent remains near current main, so the intervening scan
profiling change does not explain the conversion candidate's regressions.
Held-out division is 1.165 [1.139,1.186] and modulo 1.132 [1.119,1.149] relative
to current main. These confirm the earlier slower observations; no additional
favorable reruns will override them without an actual candidate change.

Final-binary LLVM objdump disassembly extracts BasicNumber::divide. Both
functions have 708 lines, 64 stack accesses and 44 calls. After translating
internal branch addresses to function-relative offsets and external addresses
to markers, the only differences are a relocated string address/symbol. The
numeric kernel instruction sequence is unchanged. Function entry addresses
are 0x10002ab74 and 0x10002aed4, respectively. This weakens an inlining-change
hypothesis; code placement or allocator effects remain unproven explanations.

Disassembly command: llvm-objdump --disassemble --demangle --no-show-raw-insn
against each manifest's exact final Release binary. Raw and normalized function
extracts are retained. Normalization replaces instruction addresses and only
9-or-more-digit hexadecimal addresses; ordinary immediate constants remain.
A separate bounded function-entry alignment experiment is next, without any
claim about a hardware cause or a guarantee on another CPU.
