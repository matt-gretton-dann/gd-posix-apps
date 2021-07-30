/** \file   find-multiply-split-point.cc
 *  \brief  Find a good value for the multiply split point.
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include "gd/time.h"

#include <random>
#include <sstream>
#include <stdint.h>
#include <string>
#include <string_view>
#include <utility>

#include "bc.hh"

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
    auto digit_count = dist_(rand_) % (digit_count_ * GD::Bc::Number::base_log10_);

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

using Time = struct timespec;
constexpr decltype(Time::tv_nsec) sec_as_nsec = 1000000000;

constexpr Time operator-(Time const& lhs, Time const& rhs)
{
  Time result{lhs.tv_sec - rhs.tv_sec, lhs.tv_nsec - rhs.tv_nsec};
  if (rhs.tv_nsec > lhs.tv_nsec) {
    --result.tv_sec;
    result.tv_nsec += sec_as_nsec;
  }
  return result;
}

constexpr Time operator+(Time const& lhs, Time const& rhs)
{
  Time result{lhs.tv_sec + rhs.tv_sec, lhs.tv_nsec + lhs.tv_nsec};
  if (lhs.tv_nsec + rhs.tv_nsec > sec_as_nsec) {
    ++result.tv_sec;
    result.tv_nsec -= sec_as_nsec;
  }
  return result;
}

std::ostream& operator<<(std::ostream& os, Time const& t)
{
  os << t.tv_sec << '.' << std::setw(9) << std::setfill('0') << std::right << t.tv_nsec;
  return os;
}

constexpr bool operator<(Time const& lhs, Time const& rhs)
{
  return lhs.tv_sec < rhs.tv_sec || (lhs.tv_sec == rhs.tv_sec && lhs.tv_nsec < rhs.tv_nsec);
}

constexpr int64_t as_nsec(Time const& t)
{
  return static_cast<int64_t>(t.tv_sec) * sec_as_nsec + t.tv_nsec;
}

template<typename Fn>
Time time_run(Fn f)
{
  Time start, end;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
  f();
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);

  return end - start;
}

Number::NumType check(Number::NumType initial_digit)
{
  std::vector<Number> canon_numbers;
  RandomNumberGenerator rng(initial_digit);

  Time base_time{0, 0};

  for (unsigned i = 0; i < 100; ++i) {
    canon_numbers.push_back(rng.get());
    rng.next();
  }
  Number::multiply_split_point(initial_digit + 1);

  /* Generate enough numbers that the initial run takes >3 secs.
   * We expect the best time to be ~50% of initial run - so this ensures we get good numbers.
   */
  constexpr Time min_time{3, 0};
  constexpr Time half_sec{0, 500000000};
  while (base_time < min_time) {
    std::vector<Number> numbers = canon_numbers;
    base_time = time_run([&numbers]() { multiply(numbers); });
    std::cout << "Initial timing run: " << base_time << " for " << canon_numbers.size()
              << " multiplications.\n";
    if (base_time < min_time) {
      auto new_size = (canon_numbers.size() * (as_nsec(min_time + half_sec))) / as_nsec(base_time);
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
    Number::NumType split_point = (split_point_bottom + split_point_top) / 2;
    std::vector<Number> numbers = canon_numbers;
    Number::multiply_split_point(split_point);
    Time st = time_run([&numbers]() { multiply(numbers); });
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

void output(std::ostream& os, Number::NumType value)
{
  os << "/** \\file  multiply-split-point.h\n"
        "  *  \\brief Define BC_MULTIPLY_SPLIT_POINT - the optimal multiply algorithm split "
        "point.\n"
        "  *\n"
        "  * Autogenerated use find-multiply-split-point to update.\n"
        "  */\n"
        "#undef BC_MULTIPLY_SPLIT_POINT\n"
        "#define BC_MULTIPLY_SPLIT_POINT "
     << value << "\n";
}

int main(int argc, char** argv)
{
  auto result = check(500);
  if (argc == 1) {
    output(std::cout, result);
  }
  else {
    std::ofstream of(argv[1]);
    output(of, result);
  }
  return 0;
}
