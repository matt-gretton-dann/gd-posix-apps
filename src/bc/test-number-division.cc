/** \file test-number-division.cc
 * Exact native-integer oracles for directed multi-limb division.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <new>

#include <catch2/catch.hpp>

#include <array>
#include <cstdint>
#include <sstream>
#include <string>

#include "bc.hh"

TEMPLATE_TEST_CASE("Directed long division preserves quotient and remainder",
                   "[bc][number][oracle][long-division]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  auto number = [](int64_t value) {
    // BasicNumber parses unsigned numeric tokens; the parser applies signs.
    Number result(std::to_string(value < 0 ? -value : value), 10);
    if (value < 0) {
      result.negate();
    }
    return result;
  };
  struct Pair
  {
    int64_t dividend;
    int64_t divisor;
  };
  std::array<Pair, 10> const pairs{{
    {123456789, 97},                // Single-limb divisor in the base-100 model.
    {9999, 101},                    // Two-limb direct division in the base-100 model.
    {3128583549859727, 34970},      // Normalize by 17; correct the second-limb estimate.
    {6687218857121369, 63964738},   // Already normalized; decrement the estimate twice.
    {8623134770622992, 819692222},  // Normalize by 7; multiply/subtract requires add-back.
    // Estimate equals base with false second-limb test; also add-back and repeated correction.
    {1626423182298612, 81326081630},
    {1000000000000000, 999999999999},
    {9999999999999999, 1000000001},
    {1000000001, 9999999999999999},  // Dividend smaller than divisor.
    {999999999999, 999999999999},    // Exact cancellation.
  }};
  for (auto const pair : pairs) {
    for (int64_t lhs_sign : {-1, 1}) {
      for (int64_t rhs_sign : {-1, 1}) {
        int64_t const lhs = pair.dividend * lhs_sign;
        int64_t const rhs = pair.divisor * rhs_sign;
        INFO("lhs = " << lhs << ", rhs = " << rhs << ", limb base = " << Number::base_);
        Number const original = number(lhs);
        Number const divisor = number(rhs);
        Number quotient = original;
        quotient.divide(divisor, 0);
        REQUIRE(quotient == number(lhs / rhs));
        REQUIRE(quotient.scale() == 0);
        Number remainder = original;
        remainder.modulo(divisor, 0);
        REQUIRE(remainder == number(lhs % rhs));
        REQUIRE(remainder.scale() == 0);
        REQUIRE(original == number(lhs));
        REQUIRE(divisor == number(rhs));
        Number alias = original;
        alias.divide(alias, 0);
        REQUIRE(alias == Number("1", 10));
        alias = original;
        alias.modulo(alias, 0);
        REQUIRE(alias == Number("0", 10));
      }
    }
  }
}

TEMPLATE_TEST_CASE("Quotient estimate boundaries hold at every limb width",
                   "[bc][number][oracle][quotient-estimate]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  uint64_t const base = Number::base_;
  auto block = [](uint64_t value) {
    auto const width = static_cast<std::size_t>(Number::base_log10_);
    auto text = std::to_string(value);
    return std::string(width - text.size(), '0') + text;
  };
  auto const high = block(base / 2);
  auto const zero = block(0);
  auto const one = block(1);
  auto const maximum = block(base - 1);
  struct Case
  {
    std::string dividend;
    std::string divisor;
    std::string quotient;
    std::string remainder;
  };
  // Construct decimal limb blocks, not inputs/results computed by bc kernels.
  // A: U = V*B-1, V = (B/2)*B^2+B+1. q_hat == B while the second test is false.
  // B: U = (B/2)*B^3, V = (B/2)*B^2+(B-1)*B+1. Two estimate decrements.
  // C: U = V*(B-1)-1, V = (B/2)*B^2+1. The low limb forces borrow/add-back.
  std::array<Case, 3> const cases{{
    {high + one + zero + maximum, high + one + one, maximum, high + one + zero},
    {high + zero + zero + zero, high + maximum + one, block(base - 2),
     block(2) + block(base - 3) + block(2)},
    {block(base / 2 - 1) + high + zero + block(base - 2), high + zero + one, block(base - 2),
     high + zero + zero},
  }};
  for (auto const& test : cases) {
    for (bool const negative_lhs : {false, true}) {
      for (bool const negative_rhs : {false, true}) {
        INFO("base = " << base << ", U = " << test.dividend << ", V = " << test.divisor
                       << ", lhs negative = " << negative_lhs
                       << ", rhs negative = " << negative_rhs);
        Number original(test.dividend, 10);
        Number divisor(test.divisor, 10);
        Number expected_quotient(test.quotient, 10);
        Number expected_remainder(test.remainder, 10);
        Number saved_original(test.dividend, 10);
        Number saved_divisor(test.divisor, 10);
        if (negative_lhs) {
          original.negate();
          saved_original.negate();
          expected_remainder.negate();
        }
        if (negative_rhs) {
          divisor.negate();
          saved_divisor.negate();
        }
        if (negative_lhs != negative_rhs) {
          expected_quotient.negate();
        }
        Number quotient = original;
        quotient.divide(divisor, 0);
        Number remainder = original;
        remainder.modulo(divisor, 0);
        REQUIRE(quotient == expected_quotient);
        REQUIRE(remainder == expected_remainder);
        REQUIRE(quotient.scale() == 0);
        REQUIRE(remainder.scale() == 0);
        REQUIRE(original == saved_original);
        REQUIRE(divisor == saved_divisor);
      }
    }
  }
}

TEMPLATE_TEST_CASE("Knuth normalization can promote inline storage before quotient views",
                   "[bc][number][division][storage]", GD::Bc::NumberTraits8, GD::Bc::NumberTraits16,
                   GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  auto const width = Number::base_log10_;
  // With B = 10^width, (B^3 - 1) / (B + 1) has quotient B^2 - B
  // and remainder B - 1. Normalization multiplies by B/2, growing the
  // three-limb dividend before Algorithm D captures its stable buffers.
  std::string const numerator_text(3 * width, '9');
  std::string const divisor_text = "1" + std::string(width - 1, '0') + "1";
  Number const numerator(numerator_text, 10);
  Number const divisor(divisor_text, 10);
  auto quotient = numerator;
  quotient.divide(divisor, 0);
  CHECK(quotient == Number(std::string(width, '9') + std::string(width, '0'), 10));
  auto remainder = numerator;
  remainder.modulo(divisor, 0);
  CHECK(remainder == Number(std::string(width, '9'), 10));
  CHECK(numerator == Number(numerator_text, 10));
  CHECK(divisor == Number(divisor_text, 10));
}

TEMPLATE_TEST_CASE("Zero rescale and whole-limb shifts retain normalized coefficients",
                   "[bc][number][storage]", GD::Bc::NumberTraits8, GD::Bc::NumberTraits16,
                   GD::Bc::NumberTraits32)
{
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  using Limb = typename Digits::NumType;
  auto show = [](Digits const& digits) {
    std::ostringstream output;
    digits.debug(output);
    return output.str();
  };
  Digits const original(static_cast<Limb>(Digits::base_ + 1));
  // Force the two-limb base case, whose spare output slots need trimming.
  Digits::multiply_split_point(3);
  auto product = original;
  product.multiply(original, 0);
  CHECK(show(product) == "{1, 2, 1}");
  auto shifted = original;
  auto const shift = static_cast<Limb>(2 * Digits::base_log10_);
  shifted.mul_pow10(shift);
  CHECK(show(shifted) == "{0, 0, 1, 1}");
  shifted.div_pow10(shift);
  CHECK(show(shifted) == "{1, 1}");
  shifted.mul_pow10(0);
  CHECK(show(shifted) == "{1, 1}");
  CHECK(show(original) == "{1, 1}");
}

TEMPLATE_TEST_CASE("Scaled division agrees with native rational arithmetic",
                   "[bc][number][oracle][scaled-division]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using N = typename Number::NumType;
  auto decimal = [](std::uint64_t coefficient, unsigned places) {
    auto text = std::to_string(coefficient);
    if (places != 0) {
      if (text.size() <= places) {
        text.insert(0, places + 1 - text.size(), '0');
      }
      text.insert(text.size() - places, 1, '.');
    }
    return text;
  };
  auto power = [](unsigned exponent) {
    std::uint64_t result = 1;
    while (exponent != 0) {
      result *= 10;
      --exponent;
    }
    return result;
  };
  std::array<std::uint64_t, 6> const coefficients{0, 1, 7, 23, 99, 1001};
  for (auto const a : coefficients) {
    for (auto const b : coefficients) {
      if (b == 0) {
        continue;  // Zero-divisor diagnostics have separate subprocess tests.
      }
      for (unsigned sa : {0U, 1U, 2U, 3U, 4U}) {
        for (unsigned sb : {0U, 1U, 2U, 3U, 4U}) {
          for (unsigned places : {0U, 1U, 2U, 3U, 4U}) {
            // This unreduced rational formula fits uint64_t throughout this
            // bounded matrix; it shares no bc scale-alignment/division code.
            auto const coefficient = a * power(places + sb) / (b * power(sa));
            for (int left_sign : {-1, 1}) {
              for (int right_sign : {-1, 1}) {
                INFO("coefficients=" << a << "," << b << " scales=" << sa << "," << sb
                     << "," << places << " signs=" << left_sign << "," << right_sign
                     << " limb base=" << Number::base_);
                Number lhs(decimal(a, sa), 10);
                Number rhs(decimal(b, sb), 10);
                Number expected(decimal(coefficient, places), 10);
                if (left_sign < 0) {
                  lhs.negate();
                }
                if (right_sign < 0) {
                  rhs.negate();
                }
                if (left_sign != right_sign) {
                  expected.negate();
                }
                Number result = lhs;
                Number const rhs_before = rhs;
                result.divide(rhs, static_cast<N>(places));
                REQUIRE(result == expected);
                REQUIRE(result.scale() == places);
                REQUIRE(rhs == rhs_before);
                Number original(decimal(a, sa), 10);
                if (left_sign < 0) {
                  original.negate();
                }
                REQUIRE(lhs == original);
                if (a == b && sa == sb) {
                  Number alias = lhs;
                  alias.divide(alias, static_cast<N>(places));
                  REQUIRE(alias == Number("1", 10));
                  REQUIRE(alias.scale() == places);
                }
              }
            }
          }
        }
      }
    }
  }
}
