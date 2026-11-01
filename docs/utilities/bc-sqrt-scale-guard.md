# Square-root guard-scale boundary

Refs #349, #212 and #197. Square root returns scale max(input scale, requested
scale). Optional extra working digits must fit that same scale field; adding
them unchecked wraps near its maximum. The eight-bit generic model exposes the
error with bounded storage: sqrt(4) requested at scale 255 used to return 2.00
at scale 2, and irrational results also lost digits.

Determine the initial magnitude from the original radicand before growing its
working coefficient. Querying length after growth can exceed the narrow helper's
result domain (already visible around requested scales 95–96) even when the input
is small. The helper's invariant remains intact; no assertion is weakened.

Keep the established extra limb-plus-one guard digits when representable.
Otherwise use the requested result precision. At working scale W, after
rescaling the radicand, the Newton loop is integer floor iteration on
n = original_coefficient * 10^(2W - original_scale). Its quotient and halved
sum are exact integer floors. For positive c the first step is at least
floor(sqrt(n)); above that root later steps strictly decrease. At the root a
step stays or increases, so the existing nondecrease rule returns the exact
floor root and prevents adjacent-integer cycling. Extra guard digits are not
a prerequisite for that proof.

Regressions check exact squares and independently generated isqrt(2 * 10^510)
prefixes at scales 95–96 and 252–255, explicit result scale, output and retained snapshots.
This preserves the generic fallback without adding an arbitrary scale limit.
Native maximum-scale allocations are not attempted; broader implementation-limit
policy remains #186. Precision growth and better initial estimates remain #212
experiments, separately measured after this correctness prerequisite.

## Native compiler-policy evaluation

The [initial native candidate](bc-349-sqrt-scale-guard/initial-native/README.md)
passes correctness and ordinary checks but is held: Windows confirms an equal-
comparison slowdown, even though that workload does not execute sqrt. An inlining
or layout interaction is a hypothesis, not an established instruction-level cause.
The [sqrt-only boundary experiment](bc-349-sqrt-scale-guard/msvc-sqrt-boundary/README.md)
also fails the Windows comparison policy and is dropped. The next source instead
isolates the affected Number comparison wrapper from surrounding MSVC inlining;
the limb comparator already has its own boundary. Other compilers keep the same
generic policy. Fresh exact-source native evaluation is required before adoption,
and both earlier losing datasets remain unchanged.
