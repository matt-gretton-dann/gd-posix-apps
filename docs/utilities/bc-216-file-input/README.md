# File-input workload and transport

Refs #216, #210 and #197. Append one separate file-streaming class to the existing
45 workload dictionaries. The committed proof compares complete dictionaries
(program, oracle, parameters and group) in full, held-out and quick presets and
retains every previous program/oracle hash. The new class increments a scalar
across 20000/17500/200 complete file items and checks the final exact integer.

The harness stages exact LF bytes before timing, validates them before every
trial, appends the file path as one argv entry and supplies EOF on stdin. The
manifest records file mode. File validation warms artifact bytes: these are
cached file/frontend/VM process timings, not cold-filesystem or disk throughput.
Do not relabel earlier stdin data as file measurements.

Tests check quoting/Unicode transport with a Python fixture, closed stdin,
missing/changed artifacts, unsupported modes, exact integer oracles and native
quick execution including the file. This does not extend the interpreter's
existing filename-encoding contract. The representative native policy includes
the new group. PR #348 passes all fourteen checks; [native acceptance](accepted-native/README.md)
retains 5250 checked invocations, staged-byte audits and noisy intervals.
This infrastructure makes no scanner or performance-improvement claim.
