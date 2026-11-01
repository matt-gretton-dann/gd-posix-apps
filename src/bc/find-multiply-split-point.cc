/** \file   find-multiply-split-point.cc
 *  \brief  Find a good value for the multiply split point.
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include <algorithm>
#include <array>
#include <charconv>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <numeric>
#include <optional>
#include <random>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "bc.hh"
#include "benchmark-support.hh"

using Number = GD::Bc::Number;
namespace {
constexpr Number::NumType portable_point = 126;
constexpr std::array<Number::NumType, 8> candidates{16, 32, 64, 96, 126, 192, 256, 512};

// Fixed-width, nonzero-leading operands make each reported shape exact. Parse
// once outside timing; repeated products always start from the original values.
auto operand(std::size_t limbs, std::mt19937_64& engine) -> Number
{
  std::uniform_int_distribution<unsigned> digit(0, 9);
  std::string text(limbs * Number::base_log10_, '0');
  text.front() = static_cast<char>('1' + digit(engine) % 9);
  for (std::size_t i = 1; i < text.size(); ++i)
    text[i] = static_cast<char>('0' + digit(engine));
  return Number(text, 10);
}

struct Fixture
{
  Number left;
  Number right;
  Number expected;
  std::size_t left_limbs;
  std::size_t right_limbs;
  bool square;
};

auto trial(Fixture const& fixture, Number::NumType split, std::size_t batch) -> int64_t
{
  Number::multiply_split_point(split);
  std::vector<Number> results(batch, fixture.left);
  auto const start = std::chrono::steady_clock::now();
  for (auto& value : results) {
    if (fixture.square)
      value.multiply(value, 0);
    else
      value.multiply(fixture.right, 0);
  }
  // A portable monotonic elapsed clock avoids coarse process-accounting
  // quanta on Windows. Scheduling/host contention remain measurement noise.
  auto const elapsed =
    std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::steady_clock::now() - start)
      .count();
  for (auto const& value : results) {
    if (value != fixture.expected)
      throw std::runtime_error("Candidate multiplication failed its exact oracle");
  }
  if (elapsed < 0)
    throw std::runtime_error("Monotonic calibration clock moved backwards");
  return elapsed;
}

auto check(uint64_t seed, bool quick) -> Number::NumType
{
  std::mt19937_64 engine(seed);
  std::vector<std::size_t> sizes =
    quick ? std::vector<std::size_t>{2, 12, 32}
          : std::vector<std::size_t>{1, 2, 10, 12, 112, 128, 256, 512, 1112};
  unsigned const rounds = quick ? 3 : 5;
  std::array<double, candidates.size()> log_scores{};
  std::size_t fixture_count = 0;
  std::cout << "Tuner clock: steady monotonic elapsed nanoseconds\n";
  std::cout << "sample,left_limbs,right_limbs,square,round,split,batch,elapsed_ns,checked\n";
  for (auto const left_limbs : sizes) {
    // Include balanced, asymmetric and aliased-square shapes independently.
    for (unsigned shape = 0; shape < 4; ++shape) {
      std::size_t const right_limbs = left_limbs * (shape == 1 ? 2 : shape == 2 ? 8 : 1);
      Fixture fixture{operand(left_limbs, engine),
                      operand(right_limbs, engine),
                      Number{},
                      left_limbs,
                      right_limbs,
                      shape == 3};
      fixture.expected = fixture.left;
      Number::multiply_split_point(static_cast<Number::NumType>(right_limbs + 1));
      if (fixture.square)
        fixture.expected.multiply(fixture.expected, 0);
      else
        fixture.expected.multiply(fixture.right, 0);

      std::size_t batch = 1;
      auto const max_batch =
        std::max(std::size_t{1},
                 std::min(std::size_t{256}, std::size_t{65536} / (left_limbs + right_limbs)));
      constexpr int64_t target_ns = 2000000;
      while (true) {
        auto const elapsed = trial(fixture, portable_point, batch);
        if (elapsed >= target_ns || batch == max_batch)
          break;
        batch = GD::Bc::Bench::next_batch_size(batch, elapsed, target_ns, max_batch);
      }
      // Every candidate gets an exact-checked warm-up before paired rounds.
      for (auto split : candidates)
        (void)trial(fixture, split, 1);
      std::array<std::vector<double>, candidates.size()> times;
      for (unsigned round = 0; round < rounds; ++round) {
        std::array<std::size_t, candidates.size()> order{};
        std::iota(order.begin(), order.end(), std::size_t{0});
        std::shuffle(order.begin(), order.end(), engine);
        std::array<double, candidates.size()> current{};
        for (auto index : order) {
          auto const elapsed = trial(fixture, candidates[index], batch);
          if (elapsed == 0)
            throw std::runtime_error("Insufficient monotonic clock resolution");
          current[index] = static_cast<double>(elapsed);
          std::cout << "sample," << left_limbs << ',' << right_limbs << ',' << fixture.square << ','
                    << round << ',' << candidates[index] << ',' << batch << ',' << elapsed
                    << ",1\n";
        }
        constexpr std::size_t portable_index = 4;
        for (std::size_t i = 0; i < candidates.size(); ++i)
          times[i].push_back(current[i] / current[portable_index]);
      }
      for (std::size_t i = 0; i < candidates.size(); ++i)
        log_scores[i] += std::log(GD::Bc::Bench::median(times[i]));
      ++fixture_count;
    }
  }
  std::vector<std::pair<std::size_t, double>> scores;
  for (std::size_t i = 0; i < candidates.size(); ++i) {
    double const score = std::exp(log_scores[i] / static_cast<double>(fixture_count));
    scores.emplace_back(candidates[i], score);
    std::cout << "score," << candidates[i] << ',' << std::setprecision(9) << score << '\n';
  }
  auto const selected = GD::Bc::Bench::choose_split(scores, portable_point);
  std::cout << "Best point: " << selected << " (finite candidate sweep; not a global optimum)\n";
  return static_cast<Number::NumType>(selected);
}
}  // namespace

void output(std::ostream& os, Number::NumType value, uint64_t seed)
{
  os << "/** \\file  find-multiply-split-point.hh\n"
        "  *  \\brief Define BC_MULTIPLY_SPLIT_POINT - a calibrated multiply algorithm split "
        "point.\n"
        "  *\n"
        "  * Autogenerated use find-multiply-split-point to update.\n"
        "  * Input seed: "
     << seed
     << "\n"
        "  */\n"
        "#undef BC_MULTIPLY_SPLIT_POINT\n"
        "#define BC_MULTIPLY_SPLIT_POINT "
     << value << "\n";
}

auto main(int argc, char** argv) -> int
try {
  uint64_t seed = 197234;
  std::optional<std::string_view> output_path;
  bool parse_options = true;
  bool quick = false;
  for (int i = 1; i < argc; ++i) {
    std::string_view const argument(argv[i]);
    if (parse_options && argument == "--") {
      parse_options = false;
    }
    else if (parse_options && argument == "--help") {
      std::cout << "Usage: find-multiply-split-point [--seed INTEGER] [--quick] [output-header]\n";
      return EXIT_SUCCESS;
    }
    else if (parse_options && argument == "--quick") {
      quick = true;
    }
    else if (parse_options && argument == "--seed") {
      if (++i == argc) {
        throw std::invalid_argument("Missing seed value");
      }
      std::string_view const value(argv[i]);
      auto const result = std::from_chars(value.data(), value.data() + value.size(), seed);
      if (result.ec != std::errc{} || result.ptr != value.data() + value.size()) {
        throw std::invalid_argument("Invalid seed: expected an unsigned 64-bit decimal integer");
      }
    }
    else if (parse_options && !argument.empty() && argument.front() == '-') {
      throw std::invalid_argument("Unknown option");
    }
    else if (output_path) {
      throw std::invalid_argument("Only one output header may be specified");
    }
    else {
      output_path = argument;
    }
  }

  // Fail before spending time calibrating if the output cannot be created.
  std::ofstream header;
  if (output_path) {
    header.open(std::string(*output_path));
    if (!header) {
      throw std::runtime_error("Unable to open output header");
    }
  }

  // Flush before calibration so a failure log always contains its replay seed.
  std::cout << "Tuner seed: " << seed << std::endl;
  auto const result = check(seed, quick);
  if (!output_path) {
    output(std::cout, result, seed);
  }
  else {
    output(header, result, seed);
    header.close();
    if (!header) {
      throw std::runtime_error("Unable to write output header");
    }
  }
  return 0;
}
catch (std::exception const& error) {
  std::cerr << "Calibration failed: " << error.what() << '\n';
  return EXIT_FAILURE;
}
catch (...) {
  std::cerr << "Calibration failed: unknown error\n";
  return EXIT_FAILURE;
}
