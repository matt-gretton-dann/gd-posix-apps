# Rational arctangent diagnostics

These are local Apple Clang profile counters, not elapsed benchmarks. The
baseline is accepted main `d1626677456c3b9a5a2a5bd86277f429df86bfbd`; the candidate
is identified by the maintained library source hash in the report. Both builds
use Release, profiling ON, generated-library image ON, extensions ON, automatic
IPO and the stable multiplication split point 126. No runtime ISA option changes.

`local-profile-diagnostics.json` identifies each program, binary and raw counter
file by hash. The exact programs are under `programs/`; raw counter files are
retained alongside the report. Run each binary with `-l`, the program on stdin
and `GD_BC_PROFILE_OUTPUT` set to a fresh JSON path to reproduce the counters.
Compare values with the committed `math-benchmark-oracles.json` enclosures for
`a-one` and `held-a-nine-eighths` at the recorded scales. Both oracle endpoints
must certify the one-ULP decision. All twelve outputs passed; every tracked
allocation category returns to zero live bytes.

The low-scale repeated case executes fewer instructions, divisions and
allocations. The larger function definition increases tracked peak storage by
27,552 bytes in these programs, including the unchanged unit-value path.
Higher-scale fallback retains its arithmetic but pays the additional selector
instructions. Full native arithmetic/math qualification and cold-loading costs
remain acceptance requirements; these counters do not predict elapsed speedup.
