/** \file   find-multiply-split-point.cc
 *  \brief  Find a good value for the multiply split point.
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include "gd/time.h"

#include <charconv>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <optional>
#include <random>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>

#include "bc.hh"
#include "benchmark-support.hh"

using Number = GD::Bc::Number;
class RandomNumberGenerator
{
public:
  RandomNumberGenerator(GD::Bc::Number::NumType digit_count, uint64_t seed)
      : rand_(seed), dist_(GD::Bc::Bench::limb_distribution<Number>()), digit_count_(digit_count)
  {
    next();
  }

  [[nodiscard]] auto get() const -> GD::Bc::Number const& { return number_; }

  auto next() -> bool
  {
    auto digit_count = dist_(rand_) % (digit_count_ * GD::Bc::Number::base_log10_);
    constexpr int base10 = 10;

    std::string num;
    GD::Bc::Number::NumType digits = 0;
    for (decltype(digit_count) i = 0; i < digit_count; ++i) {
      if (i % GD::Bc::Number::base_log10_ == 0) {
        digits = dist_(rand_);
      }
      auto digit = digits / (GD::Bc::Number::base_ / base10);
      digits = (digits % (GD::Bc::Number::base_ / base10)) * base10;
      assert(digit < base10);  // NOLINT

      num += static_cast<char>('0' + digit);
    }

    number_ = GD::Bc::Number(num, base10);
    return true;
  }

private:
  std::mt19937_64 rand_;
  std::uniform_int_distribution<GD::Bc::Number::NumType> dist_;
  GD::Bc::Number::NumType digit_count_;
  GD::Bc::Number number_;
};

void multiply(std::vector<Number>& n)
{
  auto it = n.begin();
  while (it != n.end() - 1) {
    it->multiply(*(it + 1), 0);
    ++it;
  }
}

using GD::Bc::Bench::as_nsec;
using GD::Bc::Bench::Time;
using GD::Bc::Bench::operator-;
using GD::Bc::Bench::operator+;
using GD::Bc::Bench::operator<<;
using GD::Bc::Bench::operator<;

template<typename Fn>
auto time_run(Fn f) -> Time
{
  Time start;
  Time end;
  if (clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start) != 0) {
    throw std::runtime_error("Unable to read the process CPU clock");
  }
  f();
  if (clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end) != 0) {
    throw std::runtime_error("Unable to read the process CPU clock");
  }

  return end - start;
}

auto check(Number::NumType initial_digit, uint64_t seed) -> Number::NumType
{
  std::vector<Number> canon_numbers;
  RandomNumberGenerator rng(initial_digit, seed);

  Time base_time{0, 0};

  constexpr unsigned num_numbers = 100;
  for (unsigned i = 0; i < num_numbers; ++i) {
    canon_numbers.push_back(rng.get());
    rng.next();
  }
  Number::multiply_split_point(initial_digit + 1);

  /* Generate enough numbers that the initial run takes >3 secs.
   * We expect the best time to be ~50% of initial run - so this ensures we get good numbers.
   */
  constexpr Time min_time{3, 0};          // NOLINT - we're defining numbers here they're not magic
  constexpr Time half_sec{0, 500000000};  // NOLINT - ditto
  while (base_time < min_time) {
    std::vector<Number> numbers = canon_numbers;
    base_time = time_run([&numbers]() { multiply(numbers); });
    std::cout << "Initial timing run: " << base_time << " for " << canon_numbers.size()
              << " multiplications.\n";
    if (base_time < min_time) {
      auto const new_size =
        GD::Bc::Bench::next_batch_size(canon_numbers.size(), as_nsec(base_time),
                                       as_nsec(min_time + half_sec), canon_numbers.max_size());
      for (auto i = canon_numbers.size(); i < new_size; ++i) {
        canon_numbers.push_back(rng.get());
        rng.next();
      }
    }
  }
  std::cout << "Basic speed: " << base_time << "\n";

  Number::NumType split_point_top = initial_digit + 1;
  Number::NumType split_point_bottom = 1;
  Number::NumType best_split = initial_digit + 1;
  Time best_time = base_time;

  while (split_point_top - 1 > split_point_bottom) {
    Number::NumType const split_point = (split_point_bottom + split_point_top) / 2;
    std::vector<Number> numbers = canon_numbers;
    Number::multiply_split_point(split_point);
    Time const st = time_run([&numbers]() { multiply(numbers); });
    std::cout << "Split [" << split_point_bottom << ", " << split_point_top << "] at "
              << split_point << " has speed = " << st << "\n";
    if (st < best_time) {
      split_point_top = split_point;
      best_time = st;
      best_split = split_point;
    }
    else {
      split_point_bottom = split_point;
    }
  }

  std::cout << "Base speed: " << base_time << "\n";
  std::cout << "Best speed:  " << best_time << "\n";
  std::cout << "Best point:  " << best_split << "\n";
  return best_split;
}

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
  for (int i = 1; i < argc; ++i) {
    std::string_view const argument(argv[i]);
    if (parse_options && argument == "--") {
      parse_options = false;
    }
    else if (parse_options && argument == "--help") {
      std::cout << "Usage: find-multiply-split-point [--seed INTEGER] [output-header]\n";
      return EXIT_SUCCESS;
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
  constexpr Number::NumType start_at = 500;
  auto const result = check(start_at, seed);
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
