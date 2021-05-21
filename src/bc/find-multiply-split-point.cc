/** \file   find-multiply-split-point.cc
 *  \brief  Find a good value for the multiply split point.
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include <random>
#include <sstream>
#include <stdint.h>
#include <string>
#include <time.h>
#include <utility>

#include "bc.hh"
#include <string_view>

using Number = GD::Bc::Number;
class RandomNumberGenerator
{
public:
  RandomNumberGenerator(GD::Bc::Number::NumType digit_count)
      : rand_(), dist_(0, GD::Bc::Number::base_), digit_count_(digit_count)
  {
    next();
  }

  GD::Bc::Number const& get() const { return number_; }

  bool next()
  {
    auto digit_count =
      (digit_count_ - dist_(rand_) % GD::Bc::Number::base_log10_) * GD::Bc::Number::base_log10_;

    std::string num;
    GD::Bc::Number::NumType digits = 0;
    for (decltype(digit_count) i = 0; i < digit_count; ++i) {
      if (i % GD::Bc::Number::base_log10_ == 0) {
        digits = dist_(rand_);
      }
      auto digit = digits / (GD::Bc::Number::base_ / 10);
      digits = (digits % (GD::Bc::Number::base_ / 10)) * 10;
      assert(digit < 10);

      num += static_cast<char>('0' + digit);
    }

    number_ = GD::Bc::Number(num, 10);
    return true;
  }

private:
  std::mt19937_64 rand_;
  std::uniform_int_distribution<uint64_t> dist_;
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

using Time = unsigned long long;
template<typename Fn>
Time time_run(Fn f)
{
  timespec start, end;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
  f();
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);

  Time result = end.tv_sec * 1000000000ULL + end.tv_nsec;
  result -= start.tv_sec * 1000000000ULL + start.tv_nsec;
  return result;
}

void check(Number::NumType initial_digit)
{
  std::vector<Number> canon_numbers;
  RandomNumberGenerator rng(initial_digit);
  for (unsigned i = 0; i < 1000; ++i) {
    canon_numbers.push_back(rng.get());
    rng.next();
  }

  std::vector<Number> numbers;

  numbers = canon_numbers;
  numbers = canon_numbers;
  Number::multiply_split_point(Number::base_);
  Time bt = time_run([&numbers]() { multiply(numbers); });
  std::cout << "Basic speed: " << bt << "\n";

  Number::NumType split_point_top = initial_digit;
  Number::NumType split_point_bottom = 1;
  Time best_time = bt;
  while (split_point_top - 1 > split_point_bottom) {
    Number::NumType split_point = (split_point_bottom + split_point_top) / 2;
    numbers = canon_numbers;
    Number::multiply_split_point(split_point);
    Time st = time_run([&numbers]() { multiply(numbers); });
    std::cout << "Split [" << split_point_bottom << ", " << split_point_top << "] at "
              << split_point << " has speed = " << st << "\n";
    if (st < best_time) {
      split_point_top = split_point;
      best_time = st;
    }
    else {
      split_point_bottom = split_point;
    }
  }

  std::cout << "Basic speed: " << bt << "\n";
  std::cout << "Best speed:  " << best_time << "\n";
  std::cout << "Best point: " << split_point_bottom << "\n";
}

int main() { check(500); }
