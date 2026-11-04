# Truncate before negative-exponent division (#439, #197)

For integer coefficient magnitudes A and B>0, operand scales sa/sb and requested
quotient scale t, the result coefficient is floor(A*10^(t+sb-sa)/B). When the
exponent is negative, k=sa-sb-t>0 and this becomes floor(A/(B*10^k)).

Integer division gives the exact identity:

    floor(A/(B*10^k)) = floor(floor(A/10^k)/B)

Write A=q*10^k+r with 0<=r<10^k, then q=u*B+v with 0<=v<B. The remaining
fraction is (v+r/10^k)/B<1, so both forms have quotient u. Apply the original
signs after magnitude division to preserve truncation toward zero. Set the
requested result scale even when the coefficient becomes zero.

The implementation uses rescaled_copy(0,k) to copy only surviving numerator
limbs, then normalizes present zero storage before coefficient comparison.
It retains the original denominator, preserving scalar division by values such
as seven. This avoids constructing a large denominator merely to discover that
the quotient is zero. Positive/zero exponent behavior is unchanged. No platform
intrinsics or special library/function recognition are required. The
negative-exponent conversion uses a separate call boundary where the compiler
supports an explicit no-inline attribute. Other compilers use the same
conversion and choose their own inlining.

## Regression and oracles

The test-first commit 3de4a6cf uses the public Number API and the existing
isolated allocation ceiling. Inputs are parsed before the ceiling. Dividing a
tiny coefficient with a large fractional scale by seven at scale zero must
return exact zero without allocating a padded denominator; retained numerator
and divisor snapshots must survive. Baseline a25b2894 builds the test,
then exits 1. No production fault hooks or broken internal invariants are used.
The fixed operation passes for 8-, 16- and 32-bit trait models.

Independent uint64 coefficient/scale oracles cover all signs, scales straddling
limb boundaries, zero/equality cases and multi-limb denominators. Output values
and target scale are checked separately from immutable operand snapshots.
Positive-exponent cases outside the bounded integer oracle's representable
range are explicitly skipped; overflowing mathematical denominators imply an
exact zero quotient for its bounded numerator. This does not claim exhaustive
coverage of all coefficient sizes.

Profiling output is still derived from unreduced exact powers. Request-dimension
expectations now reflect the actual truncated numerator and original divisor,
including zero coefficients. These are logical request dimensions, not timing,
RSS or process-wide malloc counts. Prepared-cache hits may retain their older
padded representation: their numeric result remains equivalent and their
separate performance is not silently attributed to this ordinary path. Modulo
is not independently rewritten by this change.

## Validation

The extensions-enabled Debug build passed 329 cases and 1,973,304 assertions.
The VM lifetime, CLI, independent number oracle and VM CLI CTests passed.
These builds used a fixed multiplication split point of 126. The native
performance and resource checks are separate from these correctness results.


The initial inline candidate exceeded the Linux limit on 100-digit
multiplication. The confirmed median ratio was 1.0571 with a 95% interval
of [1.0535, 1.0681]. The hot multiplication body matched after address
normalization, but its placement and the typed dispatch body changed.
The isolated conversion requires its own paired performance measurements.
