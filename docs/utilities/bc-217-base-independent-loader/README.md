# Base-independent ordinary library initialization

Refs #217 and #197. Regression commit `a9fcf468` precedes fix `468a57fa`.
Loading the original source after a caller selects base 2, 8 or 16 initializes
scale to 4, 16 or 32. Six assertions fail across GD and pinned Howard. Normal
`-l` preload begins at base 10; these cases exercise ordinary source-file loading.

The source now uses `2*A` to initialize decimal 20 while preserving both bases.
All four Release library/source/decision/imported groups pass. The identical
source passes 476 certified cases and the loader checks in pinned Howard;
targeted OFF ASan/UBSan loader/basic cases and mechanical source identity pass.
The interpreted report retains 121/132 normal and 130/132 modeled outcomes,
with two infeasible exponential outcomes; its source hash/line positions change.

The earlier library-validation archive retains its measured revisions and
original initializer. This separate correction does not relabel those profiles
or establish a mathematical performance result.
