# Subtractive Karatsuba square experiment — #703

This experiment follows #197/#200 and retains the accepted multiplication and
triangular-square kernels as the generic fallback. It is **OFF by default**;
`ENABLE_BC_SUBTRACTIVE_SQUARE=ON` explicitly selects it for supported traits.
Its 64-limb crossover is independent of the ordinary product crossover and is
fixed before performance collection. No performance improvement is claimed.

The exact ordinary Linux images sampled in #702 placed 98.2% of CPU periods for
`square_1000d_x10000` in `LimbSquare`. This is a leaf-sample observation from a
separate host, not an elapsed comparison or proof of removable cost. The original
4205accd arithmetic cohort misses the Howard target for medium squares on Linux
and macOS. Old holdouts are exposed and cannot become fresh holdouts again.

For `x=a+B^m*b`, the kernel computes `a²`, `b²` and `abs(a-b)²`, then reconstructs
`2ab=a²+b²-abs(a-b)²`. Each child has at most `ceil(n/2)` limbs; unlike sum-based
Karatsuba, the middle input cannot grow by a carry limb. A caller-owned scratch
buffer reserves `W(n)=3*ceil(n/2)+1+W(ceil(n/2))` above the leaf crossover. Low
and high calls reuse it sequentially. The difference and middle then occupy
separate prefixes and the middle child borrows its suffix. No recursive call
allocates. Capacity arithmetic is checked, and exact output spans include no
hidden guard limbs. Signed carry is represented by -1/0/1 without requiring a
signed widened integer type.

Existing sparse and repeated-limb shape dispatch runs before scratch allocation.
Only identical borrowed coefficient spans select the new square path; equal
values in unrelated buffers retain ordinary multiplication. Unsupported widened
traits retain the generic product. Sign, decimal scale, COW and shared snapshots
remain responsibilities of Number. The PUBLIC CMake definition gives all Number
consumers the same template dispatch. When the exact-transform experiment is
also enabled, a supported subtractive square takes precedence; this is not a
combined performance qualification of those experiments.

Tests compare independent decimal-character convolution across the three normal
limb widths, force tiny and boundary crossovers, and check dirty scratch, exact
output guards, empty/zero/odd/leading-zero coefficients, carry and borrow chains,
shared opposite signs and fractional scale. Profile-enabled tests check that a
112-limb square uses three leaf squares even when the product crossover is 126.
The separate hosted matrix covers ordinary, diagnostic and explicit OFF modes
on Linux, macOS and Windows, preserving compile commands, configurations, actual
images, source hashes and logs. Correctness is not performance acceptance.

Any later elapsed study must freeze its complete protocol before launch, retain
all existing arithmetic/math/resource/startup classes and add fresh square shapes
and dimensions. Keep rejected/inconclusive results and independent A/A controls;
do not pool hosts, retry for favorable results or waive startup requirements.
The ten-day TOTAL recorded goal-time cap includes all prior implementation,
queueing and review. At the cap, preserve incomplete results and leave unmet
requirements open.

The qualification continuation reuses the matched resource collectors and their
tests from immutable source `4205accd05ff938e9860b349f27117c8323297ab` (including
the #700 resource collection changes in `f4c496df`). Only collector/test files
are carried over: no prefix-buffer, typed-reset, word-runtime or math-library
implementation is imported. Every new study must retain its own source, images,
compile selection, software counters, native RSS and startup data. Historical
performance results remain bound to their original sources and are not pooled
or transferred. Ordinary/profile compile eligibility is recorded separately;
matching selected counters cannot establish identical dynamic execution paths.

`--require-candidate-subtractive-square` makes both native collectors reject
unverified roles before elapsed sampling. The immutable accepted base must lack
the experiment and retain its CMake/Number source identities; the candidate must
have verified ON compilation and retain all five relevant source files. Cache
booleans alone cannot activate the experiment: three Number consumers must agree
in the compilation database, or both native Visual Studio Release projects must
provide the same unambiguous definition without per-file overrides. Actual
cache, source and project bytes are archived in `subtractive-square-policy`.
Resource roles must match their ordinary build's selection and source hashes.
The original three square sizes also receive matched resource checks, with a
positive recursion-counter requirement for medium/large enabled cases. These
checks record compile selection and diagnostic execution, not ordinary/profile
dynamic-path identity or elapsed acceptance.
