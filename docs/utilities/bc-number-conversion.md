# Numeric text conversion

Number stores an integer coefficient and a separate decimal scale. Native
coefficients use base 10^9 limbs; test traits use smaller decimal limbs. Decimal
input can populate those limbs directly without repeatedly multiplying the
whole growing coefficient by ten.

For input base ten, validate decimal characters and a sole optional radix point,
retain the original number of fractional places, and ignore leading coefficient
zeros when sizing storage. Resize once, then decode from the least significant
end in groups of the trait's decimal limb width. Fractional trailing zeros stay
in the coefficient and scale. Every exposed limb is initialized before use;
copies retain normal inline-value or shared heap COW semantics.

Single decimal digits use a direct scalar multiply-accumulate path at every
input base, preserving their base-independent value without chunk planning.
Single hexadecimal letters retain the generic decoder's same base-independent
rule. Longer nondecimal literals can combine several input digits in one native
multiply-accumulate. Short literals retain their scalar loop; traits without
sufficient widened headroom also use that generic fallback.
No input base is assumed at compilation or function definition time.

Independent tests construct references from small integer arithmetic and exact
division, avoiding the string parser under test. They cover all three traits,
chunk boundaries, leading/trailing zeros, fractional positions, empty/point-only
Number API forms, copies and single hexadecimal digits under every input base.
Existing CLI validation retains its own token/error contracts.

Apple arm64 Clang gives the large division function 64-byte entry alignment.
The bounded experiment addresses observed layout sensitivity after decoder
changes; it does not establish a hardware cause or cache-line property. Other
platforms keep their generic compiler alignment policy. [Complete measurements
and retained regressions](bc-206-decimal-input/README.md) distinguish the initial,
scalar and aligned stages, including inconclusive division/modulo intervals.

[Nondecimal grouping measurements](bc-206-radix-input/README.md) retain all
fixtures and intervals; native acceptance is still pending. Batched output
and conversion crossover decisions remain open. Performance results must distinguish component
conversion from end-to-end multiplication, which includes input and output.
The separate #320 fix handles fractional padding that bypassed line wrapping.

## Component measurement inputs

`bc-components conversion-in ITERATIONS SPLIT_POINT [TEXT_BASE]` accepts an
optional input base from 2 through 16; decimal remains the default. Nondecimal
single digits retain their base-independent values. Fractional conversion keeps
the existing decimal-place scale and truncation rule. Invalid benchmark fixtures
are rejected before measurement. The default decimal fixture syntax remains
strict signed decimal text.

For `conversion-out`, the same optional argument chooses the output base while
the fixture remains decimal. The JSON report records `text_base`; arithmetic and
parse components reject this extra argument. Internal conversion intervals still
include validation and consumer/reset work, with final output checking outside
timing. Control rows remain separate and are never subtracted.

## Nondecimal group bounds

Let B be the storage radix and M the smaller of the native limb maximum and
floor(widened maximum / B). Both a group's multiplier and its coefficient must
be at most M. If the incoming carry is at most M, every limb multiply-accumulate
is at most (B-1)M + M = BM, which fits the widened type. The carry remains bounded
by M and is drained into as many canonical limbs as needed.

The group planner bounds even the legacy decoder's full 0–16 range, including
its release-build behavior for out-of-base API forms. A k-digit group in input
base b has coefficient at most 16(b^k-1)/(b-1). Therefore b^k is bounded by
floor(M(b-1)/16)+1, computed with quotient/remainder arithmetic to avoid an
intermediate overflow. Every partial group also satisfies the bound. Normal
lexer-validated digits require less headroom, but this optimization preserves
the existing decoder rather than broadening the public input contract.

Radix points only affect the count of fractional source digits. The constructed
integer coefficient C is then converted to floor(C*10^scale/b^scale) using the
existing fractional rescaling path. Runtime input-base changes and persistent
literal-cache invalidation remain unchanged.

## Output block emission

Decimal coefficient formatting retains the existing stream/locale conversion of
limbs, zero padding, scale and sign policy. Whole-number characters are emitted
in blocks bounded by the current continuation-line capacity. If a caller sets a
stream field width, the first emitted character consumes it as before; batching
starts after that character. The physical continuation policy, including a
continuation immediately after an exactly filled whole-number line, is unchanged.

Wrapped nondecimal and fractional text uses string views of the owned formatting
buffer rather than allocating a substring per line. Views never escape the call.
Fractional leading zero padding retains its bounded zero buffer. The stream's
existing fill/alignment side effects and error handling remain in their original
paths. Capacity and time claims require measurements; batching does not imply
that nondecimal conversion has changed its arithmetic algorithm.

[Complete output measurements](bc-206-output/README.md) retain full/held-out
fixtures, every checked interval, controls and compiler/source provenance.
Native acceptance and output-base algorithm decisions remain separate work.
