/** \file limb-square.hh
 * Portable triangular coefficient square, with a generic multiplication fallback.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_LIMB_SQUARE_HH
#define GD_BC_LIMB_SQUARE_HH
#include <cassert>
#include <cstddef>
#include <limits>
#include <span>

#include "profiling.hh"
namespace GD::Bc::Details {
/** Square a borrowed coefficient into disjoint, zero-filled output.
 * Diagonal terms occur once and off-diagonal terms twice. Each row reduces
 * before the next product, so no accumulator grows with the input length.
 * The caller retains decimal scale/sign handling and all allocation.
 */
template<typename Traits>
class LimbSquare
{
  using Limb = typename Traits::NumType;
  using Wide = typename Traits::WideType;
  static constexpr Wide base = Traits::base_;

public:
  // A doubled off-diagonal product plus carry/result stays below 2*base^2.
  // Traits with less headroom retain the existing generic multiply kernel.
  static constexpr bool supported = base <= std::numeric_limits<Wide>::max() / base / 2;
  static void multiply(std::span<Limb const> input, std::span<Limb> result)
  {
    static_assert(supported);
    assert(result.size() >= 2 * input.size());
    GD_BC_PROFILE_EVENT(Profile::Event::square_multiply_calls, 1);
    for (std::size_t i = 0; i < input.size(); ++i) {
      Wide const digit = input[i];
      if (digit == 0) {
        continue;  // A zero row preserves all previously accumulated digits.
      }
      Wide carry = digit * digit + result[2 * i];
      result[2 * i] = static_cast<Limb>(carry % base);
      carry /= base;
      std::size_t j = i + 1;
      for (; j < input.size(); ++j) {
        carry += 2 * digit * input[j] + result[i + j];
        result[i + j] = static_cast<Limb>(carry % base);
        carry /= base;
      }
      while (carry != 0) {
        assert(i + j < result.size());
        carry += result[i + j];
        result[i + j++] = static_cast<Limb>(carry % base);
        carry /= base;
      }
    }
  }
};
}  // namespace GD::Bc::Details
#endif
