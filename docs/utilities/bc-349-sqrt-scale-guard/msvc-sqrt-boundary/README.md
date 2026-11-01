# MSVC sqrt call boundary — rejected compiler experiment

Refs #349, #212 and #197. Exact candidate
`caedb62e5cfa9e53039ce551b7e252f5e44c1135`; native base
`23c2c4d8ec61ec04fdb3344ea6ec66d06827b6d9`. The only added runtime
compiler policy is an MSVC noinline boundary on sqrt; other compilers retain
the generic algorithm. Do not adopt this source: the intended isolation does
not satisfy the native comparison regression policy.

All fourteen ordinary checks pass. Local generic Release passes 214 cases/
692004 assertions and eight CLI/helper groups; OFF ASan/UBSan passes 171 numeric/
VM cases/507758 assertions and seven independent oracle groups. All 2373
previous evidence hashes and six checkout simulations pass.

Digest/source-verified artifacts retain 1750 Linux, 1750 macOS and 1780 Windows
checked invocations (5280 total). Windows equal comparison is confirmed at
1.118 [1.10556,1.14435] after bounded confirmation. Windows loop and dense-array
medians also remain slower/inconclusive. Linux has no confirmed regression and
macOS intervals are wide/noisy. Complete raw observations and controls remain
in the unmodified ZIPs; the initial c08b259 dataset stays separate.

The compiler mechanism is still unproven. Drop the sqrt-only hint and evaluate
a comparison-wrapper call boundary as a separate source candidate. This is not
a rerun of unchanged code until favorable, and no instruction-level cause is
claimed from timing alone.

Windows comparison A/A control:

| full | aa | `compare_equal_1000d_x100000` | 1.001 [0.9911135244889083, 1.0038804209983367] | pass |
