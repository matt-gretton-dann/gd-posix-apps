/** \file test-number-radix.cc
 * Independent digit-arithmetic oracles for runtime-base conversion.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <catch2/catch.hpp>

#include <algorithm>
#include <array>
#include <random>
#include <sstream>
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

namespace {
// Divide a decimal character vector by a small output radix. This oracle uses
// no Number operations or production limb/output helpers.
auto radix_reference(std::string decimal, unsigned base) -> std::string
{
  constexpr char const* alphabet = "0123456789ABCDEF";
  std::string reversed;
  do {
    std::string quotient;
    unsigned remainder = 0;
    for (char digit : decimal) {
      unsigned const value = remainder * 10 + unsigned(digit - '0');
      unsigned const next = value / base;
      if (next != 0 || !quotient.empty())
        quotient += static_cast<char>('0' + next);
      remainder = value % base;
    }
    reversed += alphabet[remainder];
    decimal = std::move(quotient);
  } while (!decimal.empty());
  std::reverse(reversed.begin(), reversed.end());
  return reversed;
}

auto unwrap_radix(std::string text) -> std::string
{
  for (auto at = text.find("\\\n"); at != std::string::npos; at = text.find("\\\n"))
    text.erase(at, 2);
  if (!text.empty() && text.front() == '\n')
    text.erase(0, 1);
  return text;
}
}  // namespace

TEMPLATE_TEST_CASE("Runtime-base output agrees with independent decimal division",
                   "[bc][number][oracle][radix-output]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using N = typename Number::NumType;
  std::mt19937 random(197207);
  constexpr char const* alphabet = "0123456789ABCDEF";
  for (unsigned base = 2; base <= 16; ++base) {
    for (std::size_t count : {1U,  2U,  7U,  8U,  9U,  15U, 16U, 17U,  27U,  28U,  29U, 30U,
                              35U, 36U, 37U, 67U, 68U, 69U, 70U, 127U, 128U, 129U, 179U}) {
      std::string decimal = "7";
      for (std::size_t i = 1; i < count; ++i)
        decimal += static_cast<char>('0' + random() % 10);
      auto const whole = radix_reference(decimal, base);
      for (bool fraction : {false, true}) {
        std::string expected = whole;
        if (fraction) {
          expected += '.';
          unsigned remainder = 5;
          for (unsigned power = 1; power < 10; power *= base) {
            remainder *= base;
            expected += alphabet[remainder / 10];
            remainder %= 10;
          }
        }
        Number const original(decimal + (fraction ? ".5" : ""), N(10));
        for (bool negative : {false, true}) {
          auto value = original;
          if (negative)
            value.negate();
          unsigned const position = std::array<unsigned, 4>{0, 67, 68, 69}[count % 4];
          std::ostringstream output;
          value.output(output, N(base), position);
          INFO("base=" << base << ", decimal digits=" << count << ", fraction=" << fraction
                       << ", negative=" << negative << ", position=" << position);
          CHECK(unwrap_radix(output.str()) == (negative ? "-" : "") + expected);
          unsigned column = position;
          for (char c : output.str()) {
            if (c == '\n') {
              CHECK(column <= 70);
              column = 0;
            }
            else
              ++column;
          }
          CHECK(column <= 70);
          CHECK(original == Number(decimal + (fraction ? ".5" : ""), N(10)));
        }
      }
    }
  }
}
