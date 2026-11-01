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

#if GD_BC_PROFILE
TEMPLATE_TEST_CASE("Radix output groups reduce whole-coefficient divisions",
                   "[bc][number][radix-output-profile]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  std::string const decimal(100, '7');
  Number const value(decimal, 10);
  auto const expected = radix_reference(decimal, 16);
  auto count = []() {
    return GD::Bc::Profile::snapshot()
      .events[static_cast<std::size_t>(GD::Bc::Profile::Event::radix_output_divisions)];
  };
  auto const before = count();
  std::ostringstream output;
  value.output(output, 16, 0);
  auto const divides = count() - before;
  CHECK(unwrap_radix(output.str()) == expected);
  if constexpr (Number::base_ > 256)
    CHECK(divides < expected.size() / 2);
  else
    CHECK(divides == expected.size());  // Narrow-trait scalar fallback.
}
#endif

TEMPLATE_TEST_CASE("Large space-separated output retains the generic path",
                   "[bc][number][radix-output]", GD::Bc::NumberTraits8, GD::Bc::NumberTraits16,
                   GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using N = typename Number::NumType;
  for (auto const& fixture : std::array<std::pair<unsigned, char const*>, 2>{
         std::pair<unsigned, char const*>{20, "1073741824000000000000000000000000000000"},
         std::pair<unsigned, char const*>{
           99, "739700373388280422730015092316714942252676262352676444347001"}}) {
    Number const value(fixture.second, N(10));
    std::string expected = " 01";
    for (unsigned i = 0; i < 30; ++i)
      expected += " 00";
    std::ostringstream output;
    value.output(output, N(fixture.first), 0);
    CHECK(unwrap_radix(output.str()) == expected);
  }
}

TEMPLATE_TEST_CASE("Large radix integer construction retains exact values and snapshots",
                   "[bc][number][oracle][radix-input-large]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using N = typename Number::NumType;
  struct SplitScope
  {
    SplitScope()
    {
      Number::multiply_split_point(
        static_cast<N>(std::min(typename Number::WideType(126), Number::base_)));
    }
    ~SplitScope() { Number::multiply_split_point(1); }
  } split;
  // The narrow model exercises generic fallback inside its supported length
  // domain (length must be below the storage radix). The widest model also
  // reaches the measured production DC boundary.
  std::size_t const count = Number::base_ == 100 ? 64 : Number::base_ == 10000 ? 4096 : 65536;
  constexpr char const* alphabet = "0123456789ABCDEF";
  for (unsigned base : {2U, 16U}) {
    Number expected{N(base)};
    expected.power(Number(std::to_string(count), N(10)), N(0));
    expected.sub(Number(N(1)));
    std::string const digits(count, alphabet[base - 1]);
    Number const parsed(digits, N(base));
    INFO("base=" << base << ", count=" << count << ", storage radix=" << Number::base_);
    CHECK(parsed == expected);
    CHECK(parsed.scale() == 0);
    CHECK(parsed.length() == expected.length());
    auto changed = parsed;
    changed.add(Number(N(1)));
    CHECK(parsed == expected);
    CHECK(changed > parsed);
    Number const padded(std::string(70, '0') + digits, N(base));
    CHECK(padded == expected);
    CHECK(padded.length() == expected.length());
  }
  Number const zero(std::string(count, '0'), N(2));
  CHECK(zero.is_zero());
  CHECK(zero.length() == Number(N(0)).length());
  CHECK(zero.scale() == 0);
  Number const tiny(std::string(count, '0') + '1', N(2));
  CHECK(tiny == Number(N(1)));
  CHECK(tiny.length() == 1);
}
