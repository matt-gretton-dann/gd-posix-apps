# Fractional square-root initial estimate

Refs #212 and #197. This experiment changes the initial estimate separately
from precision growth. Working precision, exact Newton steps, termination and
final truncation retain the #349 rules.

PR #355 is accepted at source `aff408d` after integration with the forward
decimal kernel and expanded corpus. All fourteen checks, 3600 matched local
timing checks, 300 separate RSS checks and 6000 native checked invocations pass;
the native audit finds no confirmed regression. Complete final recipes,
intervals, memory costs and raw ZIPs are in
[`bc-212-fractional-estimate/accepted`](bc-212-fractional-estimate/accepted/README.md).
The earlier Linux decimal regressions remain preserved under `initial` and
`expanded-native`; their mechanism is unproven. Precision growth and capacity
reuse remain outstanding work under issue #212.

For a nonzero coefficient with P decimal digits and input scale S, use
`10^floor((P-S)/2)` as the initial power. The former estimate clamps negative
exponents to zero, starting at one even for tiny fractions. For negative
`P-S`, compute the fractional shift as `(S-P)/2 + (S-P)%2`, using only
nonnegative unsigned operations. At working scale W the guess coefficient is
`10^(W-shift)`. Since W >= S >= shift, its exponent is representable. Positive
integer growth remains a separate multiplication, avoiding an unsigned sum of
working scale and magnitude exponent.

The true root is within a factor sqrt(10) of this initial power. The existing
first-step rule supports an estimate on either side of the root: integer-floor
Newton first reaches at least the floor root, then strictly decreases until
its nondecreasing termination step. No floating-point estimate, native-only
kernel, new scale cap or weaker assertion is involved.

All three traits have exact-square and independent sqrt-two-prefix fixtures
for fractional scales 2, 4, 18 and 64, including immutable snapshots. Existing
8-bit guard boundaries, bounded native-width allocation tests and integer-floor
oracles through scale 10000 remain active.

Separate diagnostic builds record Newton iterations, summed working scales
and Number division request coefficient dimensions before normalization.
Release timing and fresh-process RSS must be measured separately against the
actual integrated baseline; diagnostic elapsed times are not speed evidence.
Those checks passed for the integrated accepted source identified above.
Precision growth itself remains a separate #212 experiment.
