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

TEMPLATE_TEST_CASE("Equal-scale comparison tolerates untrimmed digit storage",
                   "[bc][number][oracle][comparison]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  using Result = GD::Bc::Details::ComparisonResult;
  Digits const empty(0);
  Digits const one(1);
  Digits wide_zero(1);
  wide_zero.mul_pow10(4 * Digits::base_log10_);
  wide_zero.mac(0, 0);  // Keep the allocated, nonempty all-zero representation.
  CHECK(wide_zero.is_zero());
  CHECK(wide_zero.length() == 1);
  CHECK(wide_zero.compare(empty, 0) == Result::equality);
  CHECK(empty.compare(wide_zero, 0) == Result::equality);
  CHECK(wide_zero.compare(one, 0) == Result::less_than);
  CHECK(one.compare(wide_zero, 0) == Result::greater_than);
  wide_zero.mac(0, 1);  // One with several more significant zero limbs.
  CHECK_FALSE(wide_zero.is_zero());
  CHECK(wide_zero.length() == 1);
  CHECK(wide_zero.compare(one, 0) == Result::equality);
  CHECK(one.compare(wide_zero, 0) == Result::equality);

  Digits high(1);
  high.mul_pow10(4 * Digits::base_log10_);
  Digits low(2);
  low.mul_pow10(3 * Digits::base_log10_);
  CHECK(high.compare(low, 0) == Result::greater_than);
  CHECK(low.compare(high, 0) == Result::less_than);
  CHECK(high.compare(high, 0) == Result::equality);
}
