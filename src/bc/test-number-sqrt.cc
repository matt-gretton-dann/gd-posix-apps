/** \file test-number-sqrt.cc
 * Exact termination boundaries for integer-coefficient Newton iteration.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include <string>

#include "bc.hh"

TEMPLATE_TEST_CASE("Square root terminates on adjacent Newton iterates",
                   "[bc][number][sqrt-termination]", GD::Bc::NumberTraits8, GD::Bc::NumberTraits16,
                   GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  unsigned const guard = Number::base_log10_ + 1;
  std::string const root_text = "1" + std::string(guard, '0');
  Number const root(root_text, 10);
  for (char offset : {'0', '1', '2', '3'}) {
    // x=10^(2*guard)+2 makes the working integer coefficient one below
    // a perfect square. The floor Newton sequence then has a two-cycle.
    std::string const text = "1" + std::string(2 * guard - 1, '0') + offset;
    INFO("input = " << text << " guard = " << guard);
    Number actual(text, 10);
    actual.sqrt(0);
    REQUIRE(actual == root);
    REQUIRE(actual.scale() == 0);
  }
  std::string const fractional = "1" + std::string(2 * guard, '0') + ".02";
  Number actual(fractional, 10);
  actual.sqrt(0);
  REQUIRE(actual == root);
  REQUIRE(actual.scale() == 2);

  // At guard+1 decimal places, the root lies just below root+10^-guard.
  Number precise("1" + std::string(2 * guard - 1, '0') + "2", 10);
  precise.sqrt(static_cast<typename Number::NumType>(guard + 1));
  Number const expected(root_text + "." + std::string(guard, '0') + "9", 10);
  REQUIRE(precise == expected);
  REQUIRE(precise.scale() == guard + 1);
}
