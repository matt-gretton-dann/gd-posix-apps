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

Single hexadecimal digits and other established input contracts fall back to
the generic decoder when they are not ordinary decimal text. Other input bases
retain their exact generic multiply-accumulate and fractional conversion path.
No input base is assumed at compilation or function definition time.

Independent tests construct references from small integer arithmetic and exact
division, avoiding the string parser under test. They cover all three traits,
chunk boundaries, leading/trailing zeros, fractional positions, empty/point-only
Number API forms, copies and single hexadecimal digits under every input base.
Existing CLI validation retains its own token/error contracts.

This is the first #206 stage. Nondecimal grouping, batched output and conversion
crossover decisions remain open. Performance results must distinguish component
conversion from end-to-end multiplication, which includes input and output.
The separate #320 fix handles fractional padding that bypassed line wrapping.
