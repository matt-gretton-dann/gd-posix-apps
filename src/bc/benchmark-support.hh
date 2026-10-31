/** \file benchmark-support.hh
 * Shared input and timing helpers for arithmetic tests and calibration tools.
 * Copyright 2021, Matthew Gretton-Dann
 * These helpers are not part of bc's runtime arithmetic implementation.
 * SPDX-License-Identifier: Apache-2.0
 */

#ifndef GD_BC_BENCHMARK_SUPPORT_HH
#define GD_BC_BENCHMARK_SUPPORT_HH

#include "gd/time.h"

#include <cstdint>
#include <iomanip>
#include <ostream>
#include <random>

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
  Time result{lhs.tv_sec + rhs.tv_sec, lhs.tv_nsec + lhs.tv_nsec};
  if (lhs.tv_nsec + rhs.tv_nsec > sec_as_nsec) {
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
}  // namespace GD::Bc::Bench

#endif
