# Reusable scalar binding frames — #207 experiment

`ENABLE_BC_BINDING_WINDOWS=ON` selects reusable scalar cells for verified
function activations. It is enabled on the qualification topic to exercise the
implementation in normal CI; performance acceptance remains pending.
`OFF` compiles the original scalar storage path. Full checking retains the
saved-value lifecycle. No architecture-specific instructions are required.

Each letter resolves through the current scalar binding table. A frame records
the previous pointers and supplies fresh zero-valued cells for its declared
parameters and autos. Free variables still resolve to the nearest dynamic
binding. Array snapshots use the existing saved-local vector. Return values
own their data before local cells reset.

Frame owners live in a deque, whose growth preserves references to earlier
activations. Each cell vector can grow only while its frame is inactive.
Allocation finishes before bindings publish. The installed binding prefix is
tracked for exception restoration, then cells reset to zero and previous
pointers clear on release. Recursion never reuses an active frame's cells.
VM moves preserve the state owner, rather than relocating these cells.

This replaces saved-Number traffic with pointer traffic. The global table adds
indirection to scalar access; pooled vectors and deque bookkeeping increase
cold allocation and retained metadata. Each depth retains its largest observed
scalar count until VM destruction. Host recursion remains unchanged. These
costs require measurement and are not presumed improvements.

## Correctness checks

Release with the feature enabled, Release with it disabled, and typed Debug
with ASan/UBSan each pass 7,952 assertions across 41 planner/execution cases,
plus the isolated lifetime/allocation-failure suite. New differential tests
cover recursive dynamic lookup through a callee with no local bindings,
frame-pool growth, alternating one- and 26-scalar masks, zero initialization,
global restoration and independently owned heap returns.

Enabled Release and the sanitizer build each pass 25 VM CLI and 24 math CLI
cases, with one optional external-interpreter case skipped. Sanitizer runs
use `ASAN_OPTIONS=detect_leaks=0`; the independent lifetime allocator counters
remain active. This is not a leak-sanitizer result. Hosted checks remain
pending; local profile configurations are validated below.

## Local performance observations

[Raw local programs, samples and identities](local/) preserve the first
candidate, including losing cases. All observed binaries match their recorded
hashes. The A/A role reuses the baseline executable; it measures timing noise,
not independent build variance. These are local AppleClang observations with
portable multiplication thresholds and generic result storage.

The five-workload full-dimension screen uses 17 paired samples and two warm-ups.
Candidate/base medians are 0.993 for the scalar loop, 1.049 for calls, 1.002 for
arrays, 0.995 for Fibonacci and 1.027 for recursion. The short-call interval is
[1.0466,1.0553], so it does not establish the Linux/Windows 5% allowance and is
adverse to unchanged macOS acceptance. The source-level unit arctangent screen
is 0.932, near-one is 0.996 and reciprocal is 0.990, using ordinary `-l`.

A separate 17-pair sweep uses `f(x)=x+1` with 1, 2, 3, 4, 6, 8, 12, 16, 20 and
26 scalar bindings. Its median falls from 0.990 at one scalar to 0.816 at 26.
This supports the intended wide-local mechanism, while the existing
`x*x+1` call workload still loses. Binding count alone therefore does not prove
that an arbitrary function benefits. No cutoff is selected to hide that loss.

The native full/holdout matrix, independent rebuilt A/A and fixed sparse
latency/throughput cohorts are recorded in PR #590. Local resource diagnostics
below supplement those reports. Cold timing and cross-platform resource costs
still need qualification. Keep this experiment held until its adverse classes
are resolved. The retained-buffer candidate #588 is a separate comparison.

## Local resource diagnostics

[Raw resource observations](resources/) preserve checked programs, seven native
RSS observations per role/workload, selected-allocation counters, scripts,
compiler commands, build logs and hashes. These are diagnostics, not paired
elapsed-time acceptance. The RSS binaries match the original local screen's
hashes. Separate Release profile builds use the same source
`5019aea78164102541fa14b2d1716aed61c7177c`, portable arithmetic thresholds,
and binding windows ON/OFF; they do not replace either timing binary.
Both profile configurations pass all 24 profiling CLI tests, including frame
capacity reuse and ownership accounting; their test logs are retained here.

| Case | Fallback frame peak bytes | Window frame peak bytes | Fallback/window allocations |
| --- | ---: | ---: | ---: |
| Scalar loop | 4,088 | 4,088 | 2 / 2 |
| One cold call | 4,200 | 8,288 | 5 / 7 |
| 200,000 calls | 4,200 | 8,288 | 5 / 7 |
| 50-deep recursion repeated 1,000 times | 8,760 | 10,640 | 11 / 56 |
| One 50-deep recursion with 26 scalar bindings | 151,608 | 71,888 | 16 / 57 |

The window adds a deque block and per-depth slot allocations. Narrow cold calls
therefore retain more frame storage, whereas wide recursion needs less peak
storage and moves fewer values (6,589 to 1,890 in this example). Allocation
counts and cumulative traffic are distinct: wide recursion requests 200,712
bytes cumulatively in the fallback and 71,888 in the window. Both roles finish
every checked program with zero live tracked bytes. These counters cover
selected allocator payloads, not all process allocations or allocator metadata.

Native macOS RSS medians are about 2.0 MB for loops/calls, 24.3 MB for arrays,
2.3 MB for Fibonacci and 2.2 MB for recursion. Differences are within 32 KB
between roles in this small screen; this is no universal memory bound. The
uninstrumented executable grows from 606,816 to 606,976 bytes. Hosted resource
evidence and mixed-depth/width retention behavior remain acceptance work.
