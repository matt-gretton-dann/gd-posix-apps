# Windows CPU diagnostic for the recurrence candidate

The original Windows full arithmetic run for #729 confirmed a named-file
regression. Its instrumented counts and the named-file virtual entry
points match between base and candidate. This diagnostic tests where
processor samples land while replaying checked programs from that archive.

Use the original uninstrumented x64 binaries from artifact 13891505514.
Verify the provider ZIP, both binary hashes, and both program and oracle
hashes. The named-file workload, the same input sent through stdin, and
the original streaming workload are three distinct cases. Every role uses
the same deployed executable path. Two blocks alternate role order. Each
stage has 64 checked launches.

Windows Performance Recorder samples CPU activity. The pinned TraceEvent
decoder filters to the checked child process IDs and reports sampled
instructions by module and by executable-relative address for `bc.exe`.
Only the filtered counts and identities are published. The system-wide
trace is temporary on the runner. A missing image or lost events fails the
diagnostic. Report unmapped samples separately.

These samples provide attribution on one host. They do not measure elapsed
time, resolve every inlined function, or qualify a runtime change. The
same-input stdin case is new diagnostic evidence. The two original
qualification decisions remain attached to their original archives.

Use `collect.py ARCHIVE unused unused --verify-only` to check the frozen
input identities offline. The native collection requires a Windows runner
with Windows Performance Recorder and .NET 8.
