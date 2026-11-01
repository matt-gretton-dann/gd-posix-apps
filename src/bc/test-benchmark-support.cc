/** \file test-benchmark-support.cc
 * Deterministic calibration-input and timing boundary regressions.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <new>
#include <catch2/catch.hpp>

#include <array>
#include <limits>
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

TEST_CASE("Calibration batches grow after a zero clock duration", "[bc][benchmark][batch]")
{
  using GD::Bc::Bench::next_batch_size;
  // Read at runtime so the pre-fix divide-by-zero is not constant-folded.
  volatile int64_t elapsed_ns = 0;
  REQUIRE(next_batch_size(100, elapsed_ns, 3500000000LL, 10000) == 200);
}

TEST_CASE("Calibration selection tolerates outliers and nonmonotonic curves",
          "[bc][benchmark][sweep]")
{
  using GD::Bc::Bench::choose_split;
  using GD::Bc::Bench::median;
  CHECK(median({2, 1, 1000, 1, 1}) == 1);
  CHECK(median({4, 2, 1, 3}) == 2.5);
  CHECK(choose_split({{16, 1.3}, {32, 1.1}, {64, 1.2}, {96, .9}, {126, 1.1}, {192, 1.2}}) == 96);
  CHECK(choose_split({{64, 1.0}, {126, 1.005}, {192, 1.2}}) == 126);
  CHECK(choose_split({{64, 1.0}, {126, 1.02}, {192, 1.2}}) == 64);
  CHECK(choose_split({{126, 1.005}, {192, 1.2}, {64, 1.0}}) == 126);
  CHECK_THROWS_AS(median({}), std::invalid_argument);
  CHECK_THROWS_AS(median({0, 1}), std::invalid_argument);
  CHECK_THROWS_AS(median({-1, 1}), std::invalid_argument);
  CHECK_THROWS_AS(median({std::numeric_limits<double>::infinity()}), std::invalid_argument);
  CHECK_THROWS_AS(median({std::numeric_limits<double>::quiet_NaN()}), std::invalid_argument);
  CHECK_THROWS_AS(choose_split({}), std::invalid_argument);
  CHECK_THROWS_AS(choose_split({{0, 1}}), std::invalid_argument);
  CHECK_THROWS_AS(choose_split({{126, 0}}), std::invalid_argument);
}

TEST_CASE("Calibration batch estimates progress without overflow", "[bc][benchmark][batch]")
{
  using GD::Bc::Bench::next_batch_size;
  REQUIRE(next_batch_size(100, 1, 3500000000LL, 10000) == 400);
  REQUIRE(next_batch_size(100, 1000000000, 3500000000LL, 10000) == 300);
  REQUIRE(next_batch_size(100, 2000000000, 3500000000LL, 10000) == 200);
  REQUIRE(next_batch_size(100, 3500000000LL, 3500000000LL, 10000) == 100);
  REQUIRE(next_batch_size(100, 4000000000LL, 3500000000LL, 10000) == 100);
  REQUIRE(next_batch_size(100, 1, 3500000000LL, 250) == 250);
  auto const maximum = std::numeric_limits<std::size_t>::max();
  REQUIRE(next_batch_size(maximum - 1, 0, 1, maximum) == maximum);
  REQUIRE(next_batch_size(maximum / 2 + 1, 1, 4, maximum) == maximum);
  REQUIRE_THROWS_AS(next_batch_size(maximum, 0, 1, maximum), std::length_error);
  REQUIRE_THROWS_AS(next_batch_size(0, 0, 1, 100), std::invalid_argument);
  REQUIRE_THROWS_AS(next_batch_size(101, 0, 1, 100), std::invalid_argument);
  REQUIRE_THROWS_AS(next_batch_size(100, -1, 1, 100), std::invalid_argument);
  REQUIRE_THROWS_AS(next_batch_size(100, 0, 0, 100), std::invalid_argument);
}
