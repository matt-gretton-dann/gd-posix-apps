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
intrinsics or special library/function recognition are required.

## Native division boundary experiment

The next structural candidate keeps the complete 16- and 32-bit division driver
in `number-division.cc`, using the same private generic implementation as the
header-defined fallback for other traits and standalone consumers. Linking the
CMake libbc target propagates GD_BC_OUTLINED_DIVISION=1; the independent tuning
tool retains the generic implementation and does not acquire a library dependency.
Supported compilers keep that native
boundary out of line even with IPO. This tests whether expanding the new
coefficient work inside VM dispatch affects unrelated arithmetic optimization;
it does not change the quotient algorithm or tune instruction alignment.
The initial source's Windows multiplication losses remain rejected evidence.
This boundary is a hypothesis requiring fresh full/held-out native comparison,
including small division, startup and code-size costs; no recovery is assumed.
The linked-client prototype passes both ASan/UBSan language/reuse configurations
(324 cases each), its separate profiling suite (333 cases and profiling CLI),
and all 92 default bc CTest entries (47 pass, 45 declared skips). The independent
tuner builds in each configuration. Compile-command audits distinguish 44 normal
consumers (43 outlined linked clients and one generic tuner) from 45 profiling
consumers (one extra profiling entry), rather than pooling mode evidence.

## Regression and oracles

The test-first commit e37dedd0 uses the public Number API and the existing
isolated allocation ceiling. Inputs are parsed before the ceiling. Dividing a
tiny coefficient with a large fractional scale by seven at scale zero must
return exact zero without allocating a padded denominator; retained numerator
and divisor snapshots must survive. Accepted main ced17fb7 builds the test,
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

## Acceptance

The storage regression and 304,080 focused assertions pass under ASan/UBSan.
The full extensions-ON / prepared-reuse-OFF suite passes 324 cases / 1,973,102
assertions; extensions-OFF / prepared-reuse-ON passes 324 / 1,973,087. The five
consumer, lifetime, CLI, VM CLI and independent number-oracle CTests pass in
both configurations. The restored default configuration also passes all 92
bc CTest entries (47 executed successfully, 45 declared skips).

A separate profiling-enabled, coverage-disabled build passes 333 cases /
1,973,156 assertions and its profiling CLI tests, including exact output and
reduced work dimensions. These builds use a fixed multiplication split point
of 126. Eight separate compact/fallback × prepared-reuse × language coverage modes at
b54f0c3b pass their 416 selected entries and retain distinct raw denominators;
43 configured header consumers per mode have audited language definitions.
Adjusted feasibility coverage is not computed or claimed complete.

These are introduction-stage records. Current-head CI and native adoption
evidence is recorded in [PR #440](https://github.com/matt-gretton-dann/gd-posix-apps/pull/440).
The initial b54f0c3b build exposed a GCC replacement-delete diagnostic in the
isolated lifetime test and an accidentally edited hash-protected historical
README. Keep that failed checkpoint: its correction isolates GCC inlining only
in the test and restores the archived README bytes. Neither changes the
arithmetic. Historical evidence is linked from this document rather than edited.
LeakSanitizer is unavailable on this host; isolated ordinary-allocation balance
remains separate evidence. Preserve every source identity and slower/inconclusive
class; this candidate does not establish the overall Howard or #197 target.
