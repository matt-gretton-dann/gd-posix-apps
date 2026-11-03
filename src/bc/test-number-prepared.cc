/** \file test-number-prepared.cc
 * Independent integer oracles for reusable divisor snapshots.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <catch2/catch.hpp>

#include <array>
#include <cstdint>
#include <limits>
#include <sstream>
#include <string>
#include <string_view>

#include "bc.hh"

namespace {
template<typename Digits>
auto coefficient(std::uint64_t value) -> Digits
{
  Digits result(static_cast<typename Digits::NumType>(0));
  for (char digit : std::to_string(value)) {
    result.mac(10, static_cast<typename Digits::NumType>(digit - '0'));
  }
  return result;
}

template<typename Digits>
auto decimal(Digits const& value) -> std::string
{
  if (value.is_zero()) {
    return "0";
  }
  std::ostringstream output;
  value.output_base10(output, 0, 0);
  return output.str();
}

template<typename Digits>
void unprepared(Digits& value, Digits const& divisor, bool remainder)
{
  using C = GD::Bc::Details::ComparisonResult;
  auto const relation = value.compare(divisor, 0);
  if (relation == C::less_than) {
    if (!remainder) {
      value.reset();
    }
  }
  else if (relation == C::equality) {
    value = coefficient<Digits>(remainder ? 0 : 1);
  }
  else if (remainder) {
    value.modulo(divisor);
  }
  else {
    value.divide(divisor);
  }
}
}  // namespace

TEMPLATE_TEST_CASE("Repeated coefficient division preserves divisor snapshots",
                   "[bc][number][prepared][oracle]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  constexpr std::array<std::uint64_t, 16> dividends{
    0, 1, 2, 99, 100, 101, 9999, 10000, 10001, 99999999, 1000000000,
    3128583549859727ULL, 6687218857121369ULL, 8623134770622992ULL,
    1626423182298612ULL, std::numeric_limits<std::uint64_t>::max()};
  constexpr std::array<std::uint64_t, 11> divisors{
    1, 2, 7, 97, 101, 10000, 34970, 63964738, 819692222, 81326081630ULL,
    1000000000000000001ULL};
  for (auto b : divisors) {
    auto original = coefficient<Digits>(b);
    auto const snapshot = original;
    // A later cache must preserve this value even if the source is mutated
    // or its last external owner disappears between requests.
    original.mac(1, 1);
    original.reset();
    for (auto a : dividends) {
      INFO("A=" << a << " B=" << b << " radix=" << Digits::base_);
      for (unsigned repeat = 0; repeat != 3; ++repeat) {
        auto quotient = coefficient<Digits>(a);
        auto remainder = quotient;
        unprepared(quotient, snapshot, false);
        unprepared(remainder, snapshot, true);
        REQUIRE(decimal(quotient) == std::to_string(a / b));
        REQUIRE(decimal(remainder) == std::to_string(a % b));
        REQUIRE(decimal(snapshot) == std::to_string(b));
      }
    }
  }
}
