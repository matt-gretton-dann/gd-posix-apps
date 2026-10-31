/** \file test-benchmark-support.cc
 * Deterministic calibration-input and timing boundary regressions.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include <array>
#include <random>
#include <sstream>

#include "bc.hh"
#include "benchmark-support.hh"

TEMPLATE_TEST_CASE("Random limb inputs are valid decimal coefficients", "[bc][benchmark]",
                   GD::Bc::NumberTraits8, GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  auto distribution = GD::Bc::Bench::limb_distribution<Number>();
  REQUIRE(distribution.min() == 0);
  REQUIRE(distribution.max() < Number::base_);
  std::mt19937_64 engine(197234);
  std::array<bool, 100> small_seen{};
  for (unsigned sample = 0; sample < 10000; ++sample) {
    auto value = distribution(engine);
    INFO("seed = 197234 sample = " << sample << " limb = " << value);
    REQUIRE(value < Number::base_);
    if (Number::base_ == 100) {
      small_seen.at(value) = true;
    }
    for (unsigned digit = 0; digit < Number::base_log10_; ++digit) {
      REQUIRE(value / (Number::base_ / 10) < 10);
      value = static_cast<typename Number::PrintType>((value % (Number::base_ / 10)) * 10);
    }
  }
  if (Number::base_ == 100) {
    for (bool seen : small_seen) {
      REQUIRE(seen);
    }
  }
}

TEST_CASE("Calibration time arithmetic normalizes nanoseconds", "[bc][benchmark]")
{
  using namespace GD::Bc::Bench;
  auto [lhs, rhs, expected] = GENERATE(table<Time, Time, Time>({
    {{1, 100}, {2, 200}, {3, 300}},
    {{1, 900000000}, {2, 200000000}, {4, 100000000}},
    {{1, 500000000}, {2, 500000000}, {4, 0}},
    {{1, 0}, {2, 999999999}, {3, 999999999}},
  }));
  auto const sum = lhs + rhs;
  REQUIRE(sum.tv_sec == expected.tv_sec);
  REQUIRE(sum.tv_nsec == expected.tv_nsec);
  REQUIRE(as_nsec(sum) == as_nsec(lhs) + as_nsec(rhs));
  auto const difference = sum - rhs;
  REQUIRE(difference.tv_sec == lhs.tv_sec);
  REQUIRE(difference.tv_nsec == lhs.tv_nsec);
  REQUIRE((lhs < rhs) == (as_nsec(lhs) < as_nsec(rhs)));
  std::ostringstream output;
  output << Time{1, 2};
  REQUIRE(output.str() == "1.000000002");
}
