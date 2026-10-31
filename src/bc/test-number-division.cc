/** \file test-number-division.cc
 * Exact native-integer oracles for directed multi-limb division.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include <array>
#include <cstdint>
#include <string>

#include "bc.hh"

TEMPLATE_TEST_CASE("Directed long division preserves quotient and remainder",
                   "[bc][number][oracle][long-division]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  auto number = [](int64_t value) {
    // BasicNumber parses unsigned numeric tokens; the parser applies signs.
    Number result(std::to_string(value < 0 ? -value : value), 10);
    if (value < 0) {
      result.negate();
    }
    return result;
  };
  struct Pair
  {
    int64_t dividend;
    int64_t divisor;
  };
  std::array<Pair, 10> const pairs{{
    {123456789, 97},                // Single-limb divisor in the base-100 model.
    {9999, 101},                    // Two-limb direct division in the base-100 model.
    {3128583549859727, 34970},      // Normalize by 17; correct the second-limb estimate.
    {6687218857121369, 63964738},   // Already normalized; decrement the estimate twice.
    {8623134770622992, 819692222},  // Normalize by 7; multiply/subtract requires add-back.
    // Estimate equals base with false second-limb test; also add-back and repeated correction.
    {1626423182298612, 81326081630},
    {1000000000000000, 999999999999},
    {9999999999999999, 1000000001},
    {1000000001, 9999999999999999},  // Dividend smaller than divisor.
    {999999999999, 999999999999},    // Exact cancellation.
  }};
  for (auto const pair : pairs) {
    for (int64_t lhs_sign : {-1, 1}) {
      for (int64_t rhs_sign : {-1, 1}) {
        int64_t const lhs = pair.dividend * lhs_sign;
        int64_t const rhs = pair.divisor * rhs_sign;
        INFO("lhs = " << lhs << ", rhs = " << rhs << ", limb base = " << Number::base_);
        Number const original = number(lhs);
        Number const divisor = number(rhs);
        Number quotient = original;
        quotient.divide(divisor, 0);
        REQUIRE(quotient == number(lhs / rhs));
        REQUIRE(quotient.scale() == 0);
        Number remainder = original;
        remainder.modulo(divisor, 0);
        REQUIRE(remainder == number(lhs % rhs));
        REQUIRE(remainder.scale() == 0);
        REQUIRE(original == number(lhs));
        REQUIRE(divisor == number(rhs));
        Number alias = original;
        alias.divide(alias, 0);
        REQUIRE(alias == Number("1", 10));
        alias = original;
        alias.modulo(alias, 0);
        REQUIRE(alias == Number("0", 10));
      }
    }
  }
}

TEMPLATE_TEST_CASE("Quotient estimate boundaries hold at every limb width",
                   "[bc][number][oracle][quotient-estimate]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  uint64_t const base = Number::base_;
  auto block = [](uint64_t value) {
    auto const width = static_cast<std::size_t>(Number::base_log10_);
    auto text = std::to_string(value);
    return std::string(width - text.size(), '0') + text;
  };
  auto const high = block(base / 2);
  auto const zero = block(0);
  auto const one = block(1);
  auto const maximum = block(base - 1);
  struct Case
  {
    std::string dividend;
    std::string divisor;
    std::string quotient;
    std::string remainder;
  };
  // Construct decimal limb blocks, not inputs/results computed by bc kernels.
  // A: U = V*B-1, V = (B/2)*B^2+B+1. q_hat == B while the second test is false.
  // B: U = (B/2)*B^3, V = (B/2)*B^2+(B-1)*B+1. Two estimate decrements.
  // C: U = V*(B-1)-1, V = (B/2)*B^2+1. The low limb forces borrow/add-back.
  std::array<Case, 3> const cases{{
    {high + one + zero + maximum, high + one + one, maximum, high + one + zero},
    {high + zero + zero + zero, high + maximum + one, block(base - 2),
     block(2) + block(base - 3) + block(2)},
    {block(base / 2 - 1) + high + zero + block(base - 2), high + zero + one, block(base - 2),
     high + zero + zero},
  }};
  for (auto const& test : cases) {
    for (bool const negative_lhs : {false, true}) {
      for (bool const negative_rhs : {false, true}) {
        INFO("base = " << base << ", U = " << test.dividend << ", V = " << test.divisor
                       << ", lhs negative = " << negative_lhs
                       << ", rhs negative = " << negative_rhs);
        Number original(test.dividend, 10);
        Number divisor(test.divisor, 10);
        Number expected_quotient(test.quotient, 10);
        Number expected_remainder(test.remainder, 10);
        Number saved_original(test.dividend, 10);
        Number saved_divisor(test.divisor, 10);
        if (negative_lhs) {
          original.negate();
          saved_original.negate();
          expected_remainder.negate();
        }
        if (negative_rhs) {
          divisor.negate();
          saved_divisor.negate();
        }
        if (negative_lhs != negative_rhs) {
          expected_quotient.negate();
        }
        Number quotient = original;
        quotient.divide(divisor, 0);
        Number remainder = original;
        remainder.modulo(divisor, 0);
        REQUIRE(quotient == expected_quotient);
        REQUIRE(remainder == expected_remainder);
        REQUIRE(quotient.scale() == 0);
        REQUIRE(remainder.scale() == 0);
        REQUIRE(original == saved_original);
        REQUIRE(divisor == saved_divisor);
      }
    }
  }
}

TEMPLATE_TEST_CASE("Knuth normalization can promote inline storage before quotient views",
                   "[bc][number][division][storage]", GD::Bc::NumberTraits8, GD::Bc::NumberTraits16,
                   GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  auto const width = Number::base_log10_;
  // With B = 10^width, (B^3 - 1) / (B + 1) has quotient B^2 - B
  // and remainder B - 1. Normalization multiplies by B/2, growing the
  // three-limb dividend before Algorithm D captures its stable buffers.
  std::string const numerator_text(3 * width, '9');
  std::string const divisor_text = "1" + std::string(width - 1, '0') + "1";
  Number const numerator(numerator_text, 10);
  Number const divisor(divisor_text, 10);
  auto quotient = numerator;
  quotient.divide(divisor, 0);
  CHECK(quotient == Number(std::string(width, '9') + std::string(width, '0'), 10));
  auto remainder = numerator;
  remainder.modulo(divisor, 0);
  CHECK(remainder == Number(std::string(width, '9'), 10));
  CHECK(numerator == Number(numerator_text, 10));
  CHECK(divisor == Number(divisor_text, 10));
}
