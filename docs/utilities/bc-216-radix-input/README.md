# Large nondecimal input workloads

Refs #216, #206 and #197. Append binary and hexadecimal input groups after
all 42 previous cases; `corpus-preservation.json` records exact dictionary
comparison against accepted main 9669ef7, including programs and oracles.
Full workloads have 100000 source characters and three separate assignment
statements. Held-out workloads have 112500 characters; quick uses 10000 and
one assignment. Separate literal instructions prevent a warmed single-literal
cache from replacing conversion work. Both groups enter the native PR subset.

Python integer decoding independently supplies decimal output oracles. The
finite decimal conversion limit is one million digits, enough for the 135464
decimal digits of the largest hexadecimal fixture. No hexadecimal letters are
added to the output validator: every workload emits ordinary decimal numbers.

Seventeen benchmark CLI groups, including all 44 native quick cases, and six
native orchestration groups pass. Full and held-out exact-value smokes for both
bases are recorded with their executable hash. Their durations are correctness
smokes only, not speed comparisons. Native three-platform comparisons are still
required before adoption of the runtime dispatch.
