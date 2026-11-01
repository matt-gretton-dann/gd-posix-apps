# Comparison-boundary candidate: correctness prerequisite

Refs #349, #212, #200 and #197. Exact source
`d252da1f8d3e4e5d37dede1724851c9b004c8147`, base
`23c2c4d8ec61ec04fdb3344ea6ec66d06827b6d9`. MSVC isolates the
Number comparison wrapper; other compilers retain their generic policy.
All fourteen checks pass. Digest/source-verified native artifacts contain
1750 Linux, 1750 macOS and 1810 Windows checked invocations (5310 total).
No class satisfies the established two-pass confirmed-regression rule.

This does **not** establish performance equivalence or a comparison speedup.
Windows low comparison is initially 1.069 [1.05738,1.09271], then
1.068 [1.03854,1.09934] in the bounded confirmation. Equal comparison is
initially 1.065 [1.06146,1.06616], then 1.061 [1.04432,1.07655]. Both
are inconclusive under that rule, with slower medians in both passes. A/A
controls pass. Comparison performance therefore remains a #200 limitation;
the compiler mechanism and benefit of this boundary remain unproven.
The two earlier confirmed-regression datasets remain separate and unchanged.
Do not rerun unchanged source until favorable or relabel these results as wins.

Linux comparison and representative interpreter classes pass; macOS equality
and sqrt have noisy intervals. Windows sqrt is 1.004 [0.99840,1.01005].
The immediate purpose of this prerequisite is exact sqrt scale correctness,
not a performance claim. Future work must retain this comparison evidence.

Local generic validation: Release 214 cases/692004 assertions and eight
CLI/helper groups; OFF ASan/UBSan 171 numeric/VM cases/507758 assertions and
seven independent oracle groups. The previous 2386 payload hashes and seven
checkout simulations pass. Logs and unmodified ZIPs retain full details.
