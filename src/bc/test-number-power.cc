/** \file test-number-power.cc
 * Exact precision regressions for generic exponentiation.
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

TEMPLATE_TEST_CASE("Power preserves operand precision", "[bc][number][power]",
                   GD::Bc::NumberTraits8, GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  auto [base, exponent, target_scale, expected] =
    GENERATE(table<std::string_view, std::string_view, unsigned, std::string_view>({
      {"0", "0", 0, "1"},
      {"2", "0", 0, "1"},
      {"2", "1", 0, "2"},
      {"2", "2", 0, "4"},
      {"2", "16", 0, "65536"},
      {"2", "31", 0, "2147483648"},
      {"-1.25", "3", 6, "-1.953125"},
      {"-1.25", "-1", 6, "-0.800000"},
      {"-1.25", "-3", 6, "-0.512000"},
      {"1.23", "2", 0, "1.51"},
      {"1.23", "3", 1, "1.86"},
      {"-1.23", "3", 0, "-1.86"},
      {"1.23", "2", 3, "1.512"},
      {"0.10", "3", 0, "0.00"},
      {"1.23", "0", 0, "1"},
      {"1.23", "1", 0, "1.23"},
      {"1.23", "-2", 4, "0.6609"},
      {"-2", "-3", 6, "-0.125000"},
    }));
  INFO("base = " << base << " exponent = " << exponent << " scale = " << target_scale);
  auto actual = number<Number>(base);
  auto const want = number<Number>(expected);
  actual.power(number<Number>(exponent), static_cast<typename Number::NumType>(target_scale));
  REQUIRE(actual == want);
  REQUIRE(actual.scale() == want.scale());
}
