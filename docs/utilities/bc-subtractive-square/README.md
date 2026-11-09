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
