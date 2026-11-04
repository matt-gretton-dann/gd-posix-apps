# Frontend item boundaries

Refs #210 and #197. The driver executes each returned parser item
before it requests the next one. The current parser's `interactive`
flag controls this item boundary and does not detect a terminal.
Consecutive function definitions may already be collected before a
statement. The terminal-versus-batch language policy is tracked in #13.

## Two-item batching experiment

A temporary Release build of `4934a4e0` used the noninteractive parser
for named files and capped each parse at two input items. Test inputs
were regular files. Even this bound changed the visible behavior.

| File contents, one item per line | Current driver | Two-item prototype |
| --- | --- | --- |
| `2+3`, then `quit` | Prints 5, exits successfully | Exits successfully with no output |
| `2+3`, then `1 2` | Prints 5, reports the syntax error | Reports the syntax error with no output |
| `1/0`, then `1 2` | Reports division by zero | Reports the later syntax error |

Read-time quit discards the current parser item. A fatal parse error
also clears that item before emitting its diagnostic. Combining items
therefore discards or preempts earlier work. The prototype was dropped.
The named-file tests pin both diagnostic-order cases, and the existing
named-file quit test covers the output boundary before quit. A future
batch design would need to preserve per-item execution and error
ordering before it could claim a throughput improvement.

## Stream synchronization experiment

A separate build of the same revision called
`std::ios::sync_with_stdio(false)` before I/O. With stdin and stdout
attached to a PTY, both binaries emitted `5` after entering `2+3`
while the terminal stayed open. This is an Apple libc++ observation.
It leaves the terminal policy under #13.

The same host checked 192 outputs across startup, decimal, binary
and octal output, streaming stdin, and named-file statements. Each
case used one warmup and 15 interleaved measured launches per build.
Median ratios ranged from 0.992 to 1.028. Every nominal paired
interval included parity. This focused experiment gives no basis
to change stream synchronization. Existing decimal and nondecimal
bulk writes remain in place.
