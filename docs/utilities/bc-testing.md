# Testing bc arithmetic

Arithmetic optimization is gated by correctness tests; agreement between two
implementations of multiplication alone is not an independent oracle.

`test-number.cc` uses the following complementary checks:

* NumberTraits8 exhausts all 100 by 100 one-limb pairs for addition,
  subtraction, multiplication, quotient and remainder. A second set gives each
  operand an extra low limb to force carries and borrows across a limb boundary.
* A shared NumberTraits8/16/32 suite checks signed, directed multi-limb inputs
  and reproducible dense inputs against exact native integer arithmetic.
  Inputs stay at or below 3,000,000,000, so all products fit in `int64_t`.
  Thresholds 1, 2, 3 and the trait base exercise Karatsuba, mixed and schoolbook
  dispatch. Shared copies and self-aliasing operations have separate assertions.
* Conversion and width tests retain small-trait coverage while checking the
  largest valid limb, widening bounds, error sentinels and nondecimal fractions
  in each trait. No enormous allocations are needed to test these boundaries.
* Larger randomized multiplication tests compare the same operand pair in
  schoolbook, Karatsuba and mixed modes, and check the third result explicitly.

CTest uses seed `197199`. To replay or investigate with a different seed:

```sh
build/src/bc/test-libbc '[number]' --rng-seed 197199
build/src/bc/test-libbc '[oracle]' --rng-seed 197199
```

Failures include operands and seed/sample information. The exhaustive cases do
not depend on a seed. Tests which alter the global multiplication threshold
restore its initial library default (1) on scope exit, including assertion
failures. Successes are not individually logged, keeping exhaustive CI runs
manageable.

The small trait is an algorithmic model, not a substitute for width-specific
overflow checks or native-platform validation. Current coverage also does not
prove conformance for every possible scale or operand length. Independent exact
rational CLI oracles and the power/modulo defects found while developing them
are tracked in [#199](https://github.com/matt-gretton-dann/gd-posix-apps/issues/199),
[#219](https://github.com/matt-gretton-dann/gd-posix-apps/issues/219) and
[#220](https://github.com/matt-gretton-dann/gd-posix-apps/issues/220).

Raw line, branch and MC/DC measurements and feasibility classification remain
separate work in [#214](https://github.com/matt-gretton-dann/gd-posix-apps/issues/214)
and [#215](https://github.com/matt-gretton-dann/gd-posix-apps/issues/215). No coverage
percentage is inferred from assertion counts.

## Imported zero-scale oracle discrepancy

The imported Howard `tests/misc` fixture has four saved answers which discard
precision when subtracting a tiny power truncated to zero. POSIX requires
addition/subtraction to preserve the maximum operand scale; GNU agrees with
that rule. The imported test still runs and reports an explicit expected
failure against its old answers. Reverting to those answers would be an
unexpected pass and fail the suite.

`bc/number-oracle` independently checks all four expressions, including escaped
newlines and base-9 output, using exact fractions, decimal truncation and integer
radix conversion. It requires status 0, empty stderr and the complete corrected
output. This replaces the incorrect oracle without adding an unsupported-test
skip or accepting arbitrary failures. See
[#233](https://github.com/matt-gretton-dann/gd-posix-apps/issues/233).
