/** \file test-number-modulo.cc
 * Exact scaled remainder regressions.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <new>
#include <catch2/catch.hpp>

#include <string_view>

#include "bc.hh"

namespace {
template<typename Number>
auto number(std::string_view text) -> Number
{
  bool const negative = text.front() == '-';
  Number result(negative ? text.substr(1) : text, 10);
  if (negative) {
    result.negate();
  }
  return result;
}
}  // namespace

TEMPLATE_TEST_CASE("Modulo preserves the full quotient product", "[bc][number][modulo]",
                   GD::Bc::NumberTraits8, GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  auto [lhs, rhs, target_scale, expected] =
    GENERATE(table<std::string_view, std::string_view, unsigned, std::string_view>({
      {"5.3", "1.11", 2, "0.0053"},
      {"-5.3", "1.11", 2, "-0.0053"},
      {"5.3", "-1.11", 2, "0.0053"},
      {"-5.3", "-1.11", 2, "-0.0053"},
      {"1", "7", 0, "1"},
      {"1", "7", 3, "0.006"},
      {"1", "100.00", 0, "1.00"},
      {"0", "1.00", 3, "0.00000"},
      {"5.000", "1", 0, "0.000"},
      {"2", "1.00", 3, "0.00000"},
      {"1.0000", "100.0", 1, "1.0000"},
    }));
  INFO("lhs = " << lhs << " rhs = " << rhs << " scale = " << target_scale);
  auto actual = number<Number>(lhs);
  auto const want = number<Number>(expected);
  actual.modulo(number<Number>(rhs), static_cast<typename Number::NumType>(target_scale));
  REQUIRE(actual == want);
  REQUIRE(actual.scale() == want.scale());
}
