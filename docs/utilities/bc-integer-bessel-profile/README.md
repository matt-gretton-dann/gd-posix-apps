# Integer tier Bessel attribution

This fixed diagnostic cohort addresses #633, following #624/#627 under #197.
It does not collect new paired elapsed samples, recalibrate counts, qualify a
candidate, or erase an adverse result. The frozen source remains ineligible for
adoption: Linux AMD EPYC 7763 near-root Bessel was 1.05466 times OFF, with primary
interval [1.05079, 1.06128] and fixed confirmation [1.05362, 1.06229], A/A pass.

`provenance.json` identifies verified artifact 13464962613 from run 42124822343
at source `2aba8ec2cc1ed23a3806079d2f6dc1caa6cfc27d`. The two program/oracle pairs
are byte-for-byte copies of its regular `j(0,1)` and near-root scale-20 repeated
cases, each with 16,384 iterations. The complete original manifest and summary
are preserved; unselected workload files are intentionally omitted. Replay
always selects the two declared names. Historical medians only choose the
existing profiler's approximately five-second repeated-input count, bounded by
10,000 repetitions and 64 MiB of expanded source. They are not new observations.

The workflow builds OFF/ON Release binaries and separate software-profile
binaries using the frozen source's checked builders, portable threshold 126 and
default native compiler. All four builds run on the same fresh Linux VM. Record
the actual CPU/compiler: this machine need not match the failed cohort's host.
No ISA flags, runtime changes, library recognition or kernel settings are added.
After each build, its fresh clone uses a sparse working tree to avoid retaining
duplicate tracked documentation. Source, Git objects, compiler outputs and
unique diagnostic evidence remain available; no user checkout is altered.

The existing CPU adapter samples `cpu-clock:u`; its privileged recorder launches
the interpreter as the regular runner. These are leaf samples, without a
callgraph or hardware branch/cache-miss attribution. Native traces must contain
interpreter samples. Both native and software-profile children
check complete outputs against the frozen numeric/state oracles. Instrumented
elapsed values are discarded. Every successful software profile must end with
zero tracked live storage; the OFF control must have zero integer events. ON
events may be zero: whether these programs actually enter the tier is a question
to answer, not an assumption built into the collector.
Each software-profile child executes one frozen program; native sampling may
repeat that program to obtain approximately five seconds of samples. Retained
repeat counts must be used when interpreting the different observations.

Retained evidence includes native traces/data, CPU/RSS availability, software
counters, actual binaries and runtime objects/libraries, disassembly and symbol
sizes, ordinary library source, build commands/configuration, source/binary/input
hashes and host identity. Unsupported native facilities remain explicit failures
or unavailable fields, not attribution proof. The collection has a shared
one-hour budget, shorter child deadlines and a 75-minute hosted job bound;
complete and partial diagnostic artifacts are retained. Existing portable
profiler adapters and the interpreter fallback remain unchanged.

Local checks run `test-collect.py --scripts <repo>/src/bc/scripts` and the existing
`src/bc/test-profiling-tools.py` suite. These use frozen-file validation and
controlled fake children/counters; they do not execute a local benchmark or
claim native Linux sampling. Interpret native results before selecting another
runtime/compiler-boundary experiment. #631's signed-zero integration is a
different source and receives no performance acceptance from this collection.
