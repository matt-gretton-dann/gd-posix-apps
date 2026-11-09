# Frozen ordinary arithmetic CPU attribution

Refs #200, #207 and #197; uses the separately frozen #700 qualification data.
This diagnostic topic changes no runtime, default, compiler option or acceptance
policy. It collects no paired elapsed samples or duration calibration and does
not qualify an optimization. Its purpose is to identify larger remaining
allocator, VM, frontend or arithmetic costs before choosing another experiment.

`provenance.json` pins original Linux arithmetic artifact 13651188167 from
run 42563261508, attempt 1. The provider ZIP digest is checked independently,
followed by CRCs, safe member paths, bounded expanded size and 27 selected file
hashes. The two measured ordinary x86-64 ELF files are reused byte-for-byte:
accepted base `b0247467` and candidate `4205accd`. No runtime is rebuilt. The
original compiler/configuration/source metadata remains original build evidence;
relocated executable paths are recorded separately. Instrumented images, changed
sources, extra argv inputs and an unavailable runtime library are rejected.

Ten fixed corpus cases cover full loop, recursion, definitions, file statements,
square, square root and cold division, plus held-out recursion, definitions and
cold division. Programs, expected outputs, input transport and calibrated counts
are unchanged. Entire original manifests and timing summaries are retained.
Those historical medians only select the existing profiler's approximately
five-second repetition estimate, capped at 10,000 repetitions and 64 MiB of
source. They are not new timing observations. Full/held labels describe the
original study. These cases become exposed diagnostic evidence for future
design, so they must not be described as untouched future holdouts. A subsequent
architecture study must preserve the original classes and predeclare fresh
unexposed holdouts; this changes none of the active cohort's inputs or rules.

The existing `profile-bc.py` adapter checks all output and samples `cpu-clock:u`
on native Linux. Only the recorder is privileged; the interpreter runs as the
ordinary runner user. Kernel settings and platform protections are unchanged.
These are leaf samples, without callgraphs or hardware branch/cache attribution.
Both role outcomes and the fixed seeded role order are retained. Definitions and
parsing repeat with each complete program; startup and argv library inputs occur
once in that process. The separate cold RSS observation is not an elapsed pair.

The profiling VM may differ from the original timing VM. Record its CPU/OS,
actual linked-library paths and hashes, original binary identities, every
command, repetition count, native trace and partial failure. Neither sample
fractions nor selected software counters establish source causation, performance
acceptance or dynamic equality with instrumented execution. Original allocation
and timing evidence remains in the preserved input ZIP. Review costs alongside
the original observations rather than multiplying isolated ratios.

The native controller has one shared 1,800-second budget and smaller child
deadlines; the hosted job has a 45-minute limit. Attempt two stops before
sampling. Original and partial outcomes are preserved, without favorable retries
or cancellation. Unsupported native facilities are explicit failures or
availability fields, not successful attribution. The generic interpreter and
other platforms gain no dependency on perf or this workflow.

Local validation runs only frozen preparation, controlled fake children and the
existing profiling-tool tests:

```sh
python3 docs/utilities/bc-frozen-arithmetic-profile/test-collect.py --scripts src/bc/scripts
python3 src/bc/test-profiling-tools.py src/bc/scripts/profile-bc.py
```

`collect.py --prepare-only` also verifies and relocates the real archive without
executing either binary. Native CPU collection occurs only in the dedicated
workflow. Keep the full qualification cohort untouched. All work counts toward
the ten-day TOTAL #197 goal cap; keep at least 20 GiB free locally. This topic
does not close the remaining competitive or architecture requirements.
