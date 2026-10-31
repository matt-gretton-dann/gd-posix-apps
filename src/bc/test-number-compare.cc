/** \file test-number-compare.cc
 * Independent fixed-point comparison oracles at every generic limb width.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include <array>
#include <cstdint>
#include <string>

#include "bc.hh"

TEMPLATE_TEST_CASE("Signed comparisons agree with fixed-point integer ordering",
                   "[bc][number][oracle][comparison]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  // Interpret each native integer as thousandths. Additional decimal zeros
  // change representation/scale without changing the independent oracle value.
  std::array<int64_t, 13> const values{
    {-1000000000000LL, -1001, -1000, -999, -2, -1, 0, 1, 2, 999, 1000, 1001, 1000000000000LL}};
  auto number = [](int64_t value, unsigned zeros) {
    auto const magnitude = static_cast<uint64_t>(value < 0 ? -value : value);
    auto const fraction = std::to_string(1000 + magnitude % 1000).substr(1);
    Number result(std::to_string(magnitude / 1000) + "." + fraction + std::string(zeros, '0'), 10);
    if (value < 0) {
      result.negate();
    }
    return result;
  };
  for (auto const lhs : values) {
    for (auto const rhs : values) {
      for (unsigned const lhs_zeros : {0U, 2U}) {
        for (unsigned const rhs_zeros : {0U, 3U}) {
          INFO("lhs thousandths = " << lhs << ", rhs thousandths = " << rhs
                                    << ", extra zeros = " << lhs_zeros << ',' << rhs_zeros
                                    << ", base = " << Number::base_);
          Number const left = number(lhs, lhs_zeros);
          Number const right = number(rhs, rhs_zeros);
          CHECK((left == right) == (lhs == rhs));
          CHECK((left < right) == (lhs < rhs));
          CHECK((left > right) == (lhs > rhs));
        }
      }
    }
  }
  Number negative_zero("0.000", 10);
  negative_zero.negate();
  REQUIRE(negative_zero == Number("0", 10));
  REQUIRE_FALSE(negative_zero < Number("0.00", 10));
  REQUIRE_FALSE(negative_zero > Number("0.00", 10));
}
