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
fixtures and intervals; [native acceptance](bc-206-radix-input/accepted-native/README.md)
records the corrected GCC source and retained initial Linux losses. Output block emission and grouped nondecimal output are now accepted;
remaining conversion experiments keep #206 open. Performance results must distinguish component
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

In the classic global locale, decimal coefficient formatting uses a reusable
stack limb buffer, converts the first limb without padding, and writes each
interior limb at exactly the decimal storage width. One pre-reserved coefficient
string retains the full coefficient, so this is not constant-space formatting
or a cross-call workspace cache. Capacity multiplication is checked before
reservation. A nonclassic global locale retains the historical stream formatter,
including custom facets and punctuation independently of the destination locale.
Zero padding, scale and sign policy remain unchanged. Whole-number characters are emitted
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

## Whole-number output groups

For alphabetic output bases 2 through 16, a coefficient of at least four
storage limbs can divide by the largest power of the output base strictly
below the storage radix. Each remainder then expands through bounded native
arithmetic; all groups except the most significant are padded to their full
width. The character buffer is bounded by the native limb's bit count, and
only initialized bytes are appended. This reduces both full-coefficient
passes and the temporary remainder-vector length without changing formatting.

Short coefficients, insufficient group width and bases above 16 retain the
scalar division/space-separated formatting path. Fractional conversion is
unchanged. The four-limb choice is supported by measured full/held-out
regimes and the accepted native candidate; it is not a universal crossover claim. The diagnostic
`radix_output_divisions` event counts actual whole-coefficient division calls,
including scalar fallback. Profiling remains separate from elapsed timings.

[Complete grouped-output crossover evidence](bc-206-output-groups/README.md)
retains every full/held-out fixture, control, interval and actual source recipe.
[Accepted native evidence](bc-206-output-groups/accepted-native/README.md)
records PR #337 and preserves earlier held regressions.

[Fixed-buffer decimal limb formatting measurements](bc-206-decimal-format/README.md)
retain every local interval, fixture, control and immutable recipe.

## Diagnostic divide-and-conquer components

`bc-components conversion-in-dc ITERATIONS SPLIT_POINT [TEXT_BASE]` and
`conversion-out-dc` evaluate bounded positive-integer prototypes in the benchmark
executable. They do not change runtime Number conversion or the bc language.
Fixtures with a sign or radix point are rejected before timing. Input uses the
specified base; output fixtures remain decimal. Zero and the existing single
out-of-base digit rule remain supported. Text bases are 2–16.

Input recursively splits source digits and combines `left * base^right_width +
right`. Output divides by an exact radix power, then concatenates the quotient
with an exactly padded remainder. Its integer log estimate chooses only the
partition; exact arithmetic determines all result digits. Nonshrinking output
partitions and oversized native exponents are rejected. Decimal conversion
retains its existing linear path. Leaves use at most 64 input characters or
64 decimal coefficient digits; this is an experimental choice, not a measured
universal crossover.

Each conversion owns a local power cache, so generation, cache allocation and
cleanup are included in every interval. There is no hidden cross-call reuse.
Leaf output uses the public Number formatter and joins intermediate continuation
lines; that overhead is part of this prototype. Its performance cannot establish
a lower bound for other divide-and-conquer implementations. Both experimental
and ordinary modes share fixture validation. Controls remain separate and are
never subtracted. Profile-instrumented component reports still have no elapsed
seconds. Native source, regimes, peak-memory scope and any measured losses must
be recorded before choosing a runtime crossover.

[DC experiment results](bc-206-divide-conquer/README.md) retain all input wins,
small-input/output losses, cutoff sweeps, separate controls and process-memory
observations. [Actual runtime comparisons](bc-206-divide-conquer/runtime/README.md)
retain their distinct source identities. PR #338 accepts the 65536-significant-
character integer input policy after exact three-platform native checks, while
short/fractional/exponent-width-limited inputs retain the generic fallback.
The rejected output DC prototype does not replace grouped runtime output.
