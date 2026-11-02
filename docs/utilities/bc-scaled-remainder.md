# Direct scaled remainder experiment

Refs #416, #197, #200 and #216. This is a correctness-checked candidate;
native full/held-out performance evidence is required before adoption.

For magnitudes A and B, operand scales sa and sb, and quotient scale t,
POSIX bc defines x%y as x minus the product of y and the quotient truncated
to t places. Let R=max(sa,t+sb), U=A*10^(R-sa), and V=B*10^(R-sb-t).
The quotient coefficient is floor(U/V), so the remainder coefficient is
U-floor(U/V)*V = U mod V at scale R. Both decimal exponents are nonnegative.
The remainder has the dividend sign; changing the divisor sign changes
both quotient and divisor signs and leaves their product unchanged.

The generic coefficient kernel shares Algorithm D normalization, quotient
estimation, correction and add-back between quotient and remainder results.
Compile-time result selection avoids a result-mode branch inside the limb loop.
Remainder mode allocates no quotient vector. After all quotient digits have
been processed, the low n dividend limbs contain the exact remainder times
the normalization multiplier d. Dividing those limbs by d is exact; its discarded
remainder must be zero. Scalar and two-limb cases return their native remainder.
Ordinary quotient callers retain their existing result and normalization rules.

The number layer materializes only U and V, compares magnitudes, and requests
coefficient remainder only when U>V. Equality produces zero; U<V already holds
the result. Keep the chosen scale even for zero and preserve shared operands.
Division-by-zero diagnostics are unchanged. The formula is used when t+sb fits
the trait scale field; the previous quotient-product path remains for sums
outside that field, avoiding a newly wrapped exponent in the direct path.
This does not enlarge or redefine public language limits.

Test-first semantic guards use independent uint64 arithmetic across signs,
scales and coefficient boundaries with NumberTraits8 as the primary small-radix
model, plus focused 16/32-bit checks. Frozen Python integer references reach
native multi-limb division; shared and identical divisor tests preserve aliases.
Existing quotient-estimate/add-back fixtures exercise both outputs. CLI tests
retain modulo-by-zero errors. A profiling fixture checks exact scaled-integer
outputs and observes no quotient-product multiplication; it detects one such
multiplication in each original-path fixture. Logical counters are separate
from elapsed-time and RSS evidence.

Initial pre-integration validation: 239 ASan/UBSan component cases with
1,483,272 assertions, nine CLI/driver/lifetime checks, 616 certified ordinary-
source math references and nineteen profiling checks pass. LeakSanitizer is
unavailable on the local host. These checks establish semantics and removed
work, not a native speedup. Integrate the current accepted root implementation
before final native evaluation. Prepared divisors, reusable caller-provided
quotient/remainder buffers and symbolic shifted coefficient views remain
separate #197 follow-ups.


## Integrated validation and native matrix

The candidate now includes accepted staged-root main `66cbf5d`. After integration,
257 ASan/UBSan cases with 1,505,459 assertions, nine CLI/driver/lifetime checks,
616 ordinary-source math references and twenty-four profiling checks pass.
The benchmark builder and orchestration suites pass 37 and ten checks.

Two new fractional modulo groups exercise numerator and divisor decimal
padding. Full cases use 600/230-digit seeded coefficients, operand scales
3/7 or 97/37, quotient scale 43 and 1000 repetitions. Holdout uses different
seeded coefficients, dimensions, scales and counts; quick mode scales down
both dimensions and repetitions. Exact Fraction tests derive x-trunc(x/y,t)*y
independently of the integer remainder oracle. A frozen snapshot verifies
all prior 56 corpus cases and their payloads remain unchanged in each preset;
all 58 quick cases execute with checked outputs. The full native branch route
will compare complete full/holdout classes, not just the new modulo groups.
Native timing remains pending and no runtime adoption is claimed.
