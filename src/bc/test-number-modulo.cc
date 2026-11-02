/** \file test-number-modulo.cc
 * Exact scaled remainder regressions.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <new>

#include <catch2/catch.hpp>

#include <algorithm>
#include <array>
#include <cstdint>
#include <string>
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

auto power10(unsigned exponent) -> std::uint64_t
{
  std::uint64_t value = 1;
  while (exponent-- != 0) {
    value *= 10;
  }
  return value;
}

auto scaled_text(std::uint64_t coefficient, unsigned scale) -> std::string
{
  auto text = std::to_string(coefficient);
  if (scale != 0) {
    if (text.size() <= scale) {
      text.insert(0, scale + 1 - text.size(), '0');
    }
    text.insert(text.size() - scale, 1, '.');
  }
  return text;
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

TEMPLATE_TEST_CASE("Scaled modulo agrees with independent integer remainders",
                   "[bc][number][modulo][oracle]", GD::Bc::NumberTraits8, GD::Bc::NumberTraits16,
                   GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using Scale = typename Number::NumType;
  // Every intermediate fits uint64_t. The primary small-radix model exercises
  // long division; focused wider models check their scalar and limb boundaries.
  constexpr std::array<std::uint64_t, 15> dividends{
    0, 1, 9, 10, 99, 100, 101, 199, 255, 999, 1000, 12345, 99999, 1234567, 19999999};
  constexpr std::array<std::uint64_t, 14> divisors{1,  2,   3,   7,   9,   10,   11,
                                                   99, 100, 101, 199, 999, 1001, 65521};
  for (auto a : dividends) {
    for (auto b : divisors) {
      for (unsigned sa = 0; sa != 3; ++sa) {
        for (unsigned sb = 0; sb != 3; ++sb) {
          for (unsigned t = 0; t != 3; ++t) {
            unsigned const result_scale = std::max(sa, sb + t);
            auto const numerator = a * power10(result_scale - sa);
            auto const denominator = b * power10(result_scale - sb - t);
            auto const remainder = numerator % denominator;
            for (unsigned signs = 0; signs != 4; ++signs) {
              auto const lhs = number<Number>((signs & 1 ? "-" : "") + scaled_text(a, sa));
              auto const rhs = number<Number>((signs & 2 ? "-" : "") + scaled_text(b, sb));
              auto const expected =
                number<Number>((signs & 1 ? "-" : "") + scaled_text(remainder, result_scale));
              INFO("A=" << a << " B=" << b << " sa=" << sa << " sb=" << sb << " t=" << t
                        << " signs=" << signs);
              auto actual = lhs;
              actual.modulo(rhs, static_cast<Scale>(t));
              REQUIRE(actual == expected);
              REQUIRE(actual.scale() == result_scale);
              // Division must detach shared coefficients before normalizing.
              REQUIRE(lhs == number<Number>((signs & 1 ? "-" : "") + scaled_text(a, sa)));
              REQUIRE(rhs == number<Number>((signs & 2 ? "-" : "") + scaled_text(b, sb)));
            }
          }
        }
      }
    }
  }
}

TEMPLATE_TEST_CASE("Scaled modulo permits a shared or identical divisor",
                   "[bc][number][modulo][oracle]", GD::Bc::NumberTraits8, GD::Bc::NumberTraits16,
                   GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  auto const original = number<Number>("1234567890123456789012345678901234567890.1234");
  for (unsigned t = 0; t != 4; ++t) {
    auto actual = original;
    actual.modulo(actual, static_cast<typename Number::NumType>(t));
    REQUIRE(actual.is_zero());
    REQUIRE(actual.scale() == t + original.scale());
    REQUIRE(original == number<Number>("1234567890123456789012345678901234567890.1234"));
  }
}

TEMPLATE_TEST_CASE("Scaled modulo preserves native multi-limb remainders",
                   "[bc][number][modulo][oracle]", GD::Bc::NumberTraits8, GD::Bc::NumberTraits16,
                   GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  // Frozen Python-integer references use the coefficient formula in the
  // preceding test. These divisors also reach Algorithm D at native widths.
  auto [lhs, rhs, scale, expected] =
    GENERATE(table<std::string_view, std::string_view, unsigned, std::string_view>({
      {"123456789012345678901234567890123456789012345678901234567890.12",
       "10000000000000000000000000001.23", 2, "71740493827174049382719272.7214"},
      {"99999999999999999999999999999999999999999999999999999999",
       "500000000000000000000000000000000000001", 0, "499999999999999999999800000000000000000"},
      {"100000000000000000000000000000000000000000000000000000001",
       "999999999999999999999999999.99", 1, "1.010"},
      {"98765432109876543210987654321098765432109876543210.25", "12345678901234567890123456789", 2,
       "74529098772885009877288500.25"},
      {"1000000000000000000000000001.01", "9999999999999999999999999999.99", 1, "1.011"},
      {"1234567890123456789012345678901234567890", "1000000000000000000000000000000000000000", 3,
       "567890123456789012345678901234567890.000"},
    }));
  for (unsigned signs = 0; signs != 4; ++signs) {
    auto actual = number<Number>((signs & 1 ? "-" : "") + std::string(lhs));
    auto const divisor = number<Number>((signs & 2 ? "-" : "") + std::string(rhs));
    auto const want = number<Number>((signs & 1 ? "-" : "") + std::string(expected));
    actual.modulo(divisor, static_cast<typename Number::NumType>(scale));
    REQUIRE(actual == want);
    REQUIRE(actual.scale() == want.scale());
  }
}
