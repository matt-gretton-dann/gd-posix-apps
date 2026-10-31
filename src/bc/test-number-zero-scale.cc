/** \file test-number-zero-scale.cc
 * POSIX addition/subtraction scale rules, including zero fast paths.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include <string>

#include "bc.hh"

namespace {
template<typename Number>
auto number(std::string text) -> Number
{
  bool const negative = text.front() == '-';
  Number result(negative ? text.substr(1) : text, 10);
  if (negative) {
    result.negate();
  }
  return result;
}
}  // namespace

TEMPLATE_TEST_CASE("Zero operands preserve maximum arithmetic scale", "[bc][number][zero-scale]",
                   GD::Bc::NumberTraits8, GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  for (unsigned zero_scale : {0U, 1U, 3U, 9U, 80U, 99U}) {
    auto const zero = Number("0." + std::string(zero_scale, '0'), 10);
    for (std::string const text : {"0", "0.00", "1", "-1", "1.2500", "-1.2500"}) {
      auto const operand = number<Number>(text);
      auto const expected_scale = std::max(zero.scale(), operand.scale());
      INFO("zero scale = " << zero_scale << " operand = " << text);
      auto actual = operand;
      actual.add(zero);
      REQUIRE(actual == operand);
      REQUIRE(actual.scale() == expected_scale);
      actual = zero;
      actual.add(operand);
      REQUIRE(actual == operand);
      REQUIRE(actual.scale() == expected_scale);
      actual = operand;
      actual.sub(zero);
      REQUIRE(actual == operand);
      REQUIRE(actual.scale() == expected_scale);
      actual = zero;
      actual.sub(operand);
      actual.negate();
      REQUIRE(actual == operand);
      REQUIRE(actual.scale() == expected_scale);
      // Copies used above must not mutate the shared original representation.
      REQUIRE(operand == number<Number>(text));
      REQUIRE(zero.is_zero());
      REQUIRE(zero.scale() == zero_scale);
    }
    auto alias = zero;
    alias.add(alias);
    REQUIRE(alias.is_zero());
    REQUIRE(alias.scale() == zero.scale());
    alias.sub(alias);
    REQUIRE(alias.is_zero());
    REQUIRE(alias.scale() == zero.scale());
  }
}
