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
