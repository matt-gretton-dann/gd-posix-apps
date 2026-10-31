# Avoiding the unused terminal square — #211

Binary coefficient exponentiation now advances the exponent before deciding
whether another squared base has a consumer. Every remaining bit sees the same
successive square; the final unused square is omitted. The same general loop
serves reciprocal powers. Public sign, scale and truncation rules are unchanged.
The change uses generic C++ for every limb trait, with no special VM opcode.

## Reproducer and semantic checks

The issue originally used positive exponent one as its example, but the current
BasicNumber already returns early for that case. The new tests protect that
zero-product shortcut, including a 1,000-digit base with exact value/length checks.
The remaining general-loop defect is demonstrated by a separate failing test
commit before the fix: powers 2/3/16/31 perform 3/4/6/10 products, one more than
the necessary 2/3/5/9. After the fix all four operation counts match exactly.
Exponent zero remains zero products. These counts use small operands so no
recursive multiplication confounds the binary-exponentiation operation count.

Generic 8/16/32-bit exact cases cover exponents 0/1/2, powers of two, dense bits,
negative/fractional bases and negative exponents with nonzero target scale.
All 128 bc cases pass 580,355 assertions, five profiling checks and six CLI/oracle/
component groups. After integrating merged aligned arithmetic (#310), all 131
cases pass 580,388 assertions, plus the same five profiling and six CLI groups.
After integrating merged comparison (#312), the current tree passes 134 cases
(580,427 assertions), five profiling checks and eight CLI/helper groups,
including the 36-case quick corpus. The timing tables retain their original
isolated revisions. Test-first and fix commits remain separate. EOF diagnostic
allocations balance.

## Matched isolated experiment

Immutable parent: `2ddf749e8b313f620ad248d680e801c409bbd3f5`.
Immutable candidate: `5fd0ca567bcd4fabe9b901aba73ed5dc8fb73495`.
These isolate the terminal-square change before later integration of #310.
The as-run generator is the #310 integration tree
`47595bc9c1c60cbd864a7abc4e86c0d4a68b8713`; its recorded hashes identify the
unchanged existing power/control programs. The current maintained corpus has
power workloads even though the original planning snapshot did not.

Both fixed126 recipes record identical Apple Clang 21 compiler identities, libc++
header 220106, extensions ON, Release `-O3 -DNDEBUG`, thin LTO and C++20. Actual
commands and binary hashes are retained. Native Howard 7.0.3 at c8dabac is the
competitive reference. No instrumented executable enters the timing experiment.
The host is the recorded Apple M1/macOS 27.2 shared workstation, with no CPU/
frequency/thermal isolation. No local builds or profiling ran during timing.

Each experiment uses one checked warm-up and nine paired measured rounds per
case, seeded interleaving and 2,000 paired bootstrap resamples. All 120 full and
120 held-out invocations pass exact output/status/stderr/deadline gates. Full
seed 197200 and held-out seed 197201 use separately dimensioned shapes reserved
from implementation choices. Loop and small-multiplication controls remain
visible. This bounded four-class experiment is supported by the full arithmetic
correctness suite; it is not a timing sweep of every class.

The two full power workloads take 36.7% and 36.2% of parent time; held-out shapes
take 35.8% and 35.6%. Loop and the 100-digit multiplication winner remain essentially
unchanged within noise. All cases and intervals are below, with no aggregate.
These observations do not establish native Linux/Windows speedups. #216 owns
advisory native paired trends; competitive misses remain visible.

## Full

| Workload | Candidate / parent [95%] | Candidate / Howard [95%] |
| :--- | :--- | :--- |
| `loop_1000000` | 1.003 [0.994, 1.058] | 2.067 [2.026, 2.196] |
| `multiply_100d_x100000` | 0.997 [0.995, 0.999] | 0.918 [0.913, 0.923] |
| `power_100d_e17_x1000` | 0.367 [0.366, 0.370] | 0.846 [0.835, 0.855] |
| `power_20d_e32_x2000` | 0.362 [0.358, 0.372] | 0.790 [0.759, 0.802] |

## Holdout

| Workload | Candidate / parent [95%] | Candidate / Howard [95%] |
| :--- | :--- | :--- |
| `loop_875000` | 0.993 [0.969, 1.034] | 2.043 [2.017, 2.083] |
| `multiply_112d_x87500` | 1.002 [0.995, 1.015] | 0.872 [0.869, 0.874] |
| `power_112d_e17_x875` | 0.358 [0.356, 0.362] | 0.874 [0.864, 0.886] |
| `power_22d_e32_x1750` | 0.356 [0.350, 0.364] | 0.840 [0.805, 0.855] |
