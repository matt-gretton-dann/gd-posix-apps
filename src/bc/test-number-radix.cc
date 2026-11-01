/** \file test-number-radix.cc
 * Independent digit-arithmetic oracles for runtime-base conversion.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <catch2/catch.hpp>

#include <array>
#include <random>
#include <string>

#include "bc.hh"

TEMPLATE_TEST_CASE("Runtime-base construction agrees with digit arithmetic",
                   "[bc][number][oracle][radix-parse]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using N = typename Number::NumType;
  constexpr char const* alphabet = "0123456789ABCDEF";
  std::mt19937 random(197206);
  for (unsigned base = 2; base <= 16; ++base) {
    Number const radix{N(base)};
    // Even the largest widened numerator remains inside the small trait's
    // existing decimal-length domain; these are not oversized limit tests.
    for (std::size_t count : {1U,  2U,  3U,  4U,  6U,  7U,  8U,  9U,  12U, 13U, 15U, 16U, 17U,
                              24U, 25U, 27U, 28U, 29U, 31U, 32U, 33U, 56U, 57U, 63U, 64U}) {
      for (bool maximal : {false, true}) {
        Number coefficient{N(0)};
        std::string digits;
        for (std::size_t i = 0; i < count; ++i) {
          unsigned const digit = maximal ? base - 1 : unsigned(random() % base);
          digits += alphabet[digit];
          coefficient.multiply(radix, N(0));
          coefficient.add(Number(N(digit)));
        }
        for (std::size_t places : {std::size_t{0}, std::size_t{1}, count / 2, count}) {
          Number denominator{N(1)};
          for (std::size_t i = 0; i < places; ++i)
            denominator.multiply(radix, N(0));
          auto expected = coefficient;
          expected.divide(denominator, N(places));
          std::string literal = "000" + digits;
          if (places != 0)
            literal.insert(literal.size() - places, 1, '.');
          INFO("base=" << base << ", digits=" << count << ", places=" << places
                       << ", maximal=" << maximal << ", literal=" << literal);
          Number const parsed(literal, N(base));
          CHECK(parsed == expected);
          CHECK(parsed.scale() == expected.scale());
          CHECK(parsed.length() == expected.length());
          auto mutated = parsed;
          mutated.add(Number(N(1)));
          CHECK(parsed == expected);
        }
      }
    }
  }
}
