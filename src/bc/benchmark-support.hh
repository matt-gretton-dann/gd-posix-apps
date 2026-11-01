/** \file benchmark-support.hh
 * Shared input and timing helpers for arithmetic tests and calibration tools.
 * Copyright 2021, Matthew Gretton-Dann
 * These helpers are not part of bc's runtime arithmetic implementation.
 * SPDX-License-Identifier: Apache-2.0
 */

#ifndef GD_BC_BENCHMARK_SUPPORT_HH
#define GD_BC_BENCHMARK_SUPPORT_HH

#include "gd/time.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <iomanip>
#include <ostream>
#include <random>
#include <stdexcept>
#include <utility>
#include <vector>

namespace GD::Bc::Bench {
template<typename Number>
auto limb_distribution() -> std::uniform_int_distribution<typename Number::PrintType>
{
  // PrintType promotes an 8-bit limb to a supported distribution integer type.
  return std::uniform_int_distribution<typename Number::PrintType>(
    0, static_cast<typename Number::PrintType>(Number::base_ - 1));
}

using Time = struct timespec;
constexpr decltype(Time::tv_nsec) sec_as_nsec = 1000000000;

constexpr auto operator-(Time const& lhs, Time const& rhs) -> Time
{
  Time result{lhs.tv_sec - rhs.tv_sec, lhs.tv_nsec - rhs.tv_nsec};
  if (rhs.tv_nsec > lhs.tv_nsec) {
    --result.tv_sec;
    result.tv_nsec += sec_as_nsec;
  }
  return result;
}

constexpr auto operator+(Time const& lhs, Time const& rhs) -> Time
{
  Time result{lhs.tv_sec + rhs.tv_sec, lhs.tv_nsec + rhs.tv_nsec};
  if (result.tv_nsec >= sec_as_nsec) {
    ++result.tv_sec;
    result.tv_nsec -= sec_as_nsec;
  }
  return result;
}

inline auto operator<<(std::ostream& os, Time const& time) -> std::ostream&
{
  constexpr int nsec_digits = 9;
  os << time.tv_sec << '.' << std::setw(nsec_digits) << std::setfill('0') << std::right
     << time.tv_nsec;
  return os;
}

constexpr auto operator<(Time const& lhs, Time const& rhs) -> bool
{
  return lhs.tv_sec < rhs.tv_sec || (lhs.tv_sec == rhs.tv_sec && lhs.tv_nsec < rhs.tv_nsec);
}

constexpr auto as_nsec(Time const& time) -> int64_t
{
  return static_cast<int64_t>(time.tv_sec) * sec_as_nsec + time.tv_nsec;
}
// Grow by a bounded factor, including when a coarse CPU clock reports zero.
// Strict progress up to the container limit avoids overflow and stalled loops.
inline auto next_batch_size(std::size_t count, int64_t elapsed_ns, int64_t target_ns,
                            std::size_t max_count) -> std::size_t
{
  if (count == 0 || count > max_count || elapsed_ns < 0 || target_ns <= 0) {
    throw std::invalid_argument("Invalid calibration batch or duration");
  }
  if (elapsed_ns >= target_ns) {
    return count;
  }
  if (count == max_count) {
    throw std::length_error("Calibration batch reached the container limit");
  }
  std::size_t factor = 2;
  if (elapsed_ns > 0) {
    auto const ratio = target_ns / elapsed_ns;
    factor = ratio >= 4 ? 4 : (ratio >= 3 ? 3 : 2);
  }
  return count > max_count / factor ? max_count : count * factor;
}

// Calibration uses medians of repeated, paired ratios. Reject incomplete or
// nonpositive clock observations rather than interpreting them as a speedup.
inline auto median(std::vector<double> values) -> double
{
  if (values.empty() || std::any_of(values.begin(), values.end(),
                                    [](double v) { return !std::isfinite(v) || v <= 0; })) {
    throw std::invalid_argument("Invalid calibration samples");
  }
  std::sort(values.begin(), values.end());
  auto const middle = values.size() / 2;
  return values.size() % 2 != 0 ? values[middle] : values[middle - 1] / 2 + values[middle] / 2;
}

// Sweep every candidate: scores need not be monotonic in the split point.
// Within 1% of the best score, prefer the point closest to the portable policy
// to avoid moving the boundary in response to a marginal timing difference.
inline auto choose_split(std::vector<std::pair<std::size_t, double>> const& scores,
                         std::size_t portable = 126) -> std::size_t
{
  if (scores.empty())
    throw std::invalid_argument("No calibration candidates");
  double best = scores.front().second;
  for (auto [point, score] : scores) {
    if (point == 0 || !std::isfinite(score) || score <= 0)
      throw std::invalid_argument("Invalid calibration score");
    best = std::min(best, score);
  }
  auto distance = [portable](std::size_t point) {
    return point > portable ? point - portable : portable - point;
  };
  std::size_t selected = 0;
  for (auto [point, score] : scores) {
    if (score <= best * 1.01 && (selected == 0 || distance(point) < distance(selected) ||
                                 (distance(point) == distance(selected) && point < selected))) {
      selected = point;
    }
  }
  return selected;
}

}  // namespace GD::Bc::Bench

#endif
