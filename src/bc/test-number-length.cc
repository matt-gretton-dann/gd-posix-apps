/** \file test-number-length.cc
 * Exact digit lengths for ordinary and unnormalized public-operation results.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include <string>

#include "bc.hh"

TEMPLATE_TEST_CASE("Digit length traverses high zero limbs without mutation",
                   "[bc][number][oracle][digit-length]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  using N = typename Digits::NumType;
  REQUIRE(Digits().length() == 1);
  REQUIRE(Digits(N(0)).length() == 1);
  for (N const value : {N(0), N(1), N(9), N(10), N(99), N(Digits::base_ - 1), N(Digits::base_)}) {
    INFO("base = " << Digits::base_ << ", value = " << static_cast<unsigned long long>(value));
    REQUIRE(Digits(value).length() == std::to_string(value).size());
    Digits digits(N(1));
    digits.mul_pow10(N(3 * Digits::base_log10_));
    auto const saved = digits;
    // mac deliberately does not normalize. These are supported operations,
    // not injected private digits or invalid iterator ranges.
    digits.mac(N(0), value);
    auto const shared = digits;
    REQUIRE(digits.length() == std::to_string(value).size());
    REQUIRE(shared.length() == std::to_string(value).size());
    REQUIRE(saved.length() == 3 * Digits::base_log10_ + 1);
    REQUIRE(digits.is_zero() == (value == 0));
  }
}
