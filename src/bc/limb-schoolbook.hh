/** \file limb-schoolbook.hh
 * Portable bounded deferred-carry schoolbook multiplication.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_LIMB_SCHOOLBOOK_HH
#define GD_BC_LIMB_SCHOOLBOOK_HH
#include <algorithm>
#include <array>
#include <cassert>
#include <cstddef>
#include <limits>
#include <span>

#include "profiling.hh"
namespace GD::Bc::Details {
template<typename Traits>
class LimbSchoolbook
{
  using Limb = typename Traits::NumType;
  using Wide = typename Traits::WideType;
  using Input = std::span<Limb const>;
  using Buffer = std::span<Limb>;
  static constexpr Wide base = Traits::base_;
  static constexpr Wide permitted_rows = std::numeric_limits<Wide>::max() / base / base;
  static constexpr std::size_t rows =
    permitted_rows < 16 ? static_cast<std::size_t>(permitted_rows) : std::size_t{16};
  static constexpr std::size_t columns = 16;
  static void scalar(Input a, Input b, Buffer result)
  {
    for (std::size_t i = 0; i < a.size(); ++i) {
      Wide carry = 0;
      std::size_t j = 0;
      for (; j < b.size(); ++j) {
        carry += Wide(a[i]) * b[j] + result[i + j];
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

public:
  static auto eligible(std::size_t a, std::size_t b) -> bool
  {
    return rows >= 2 && std::min(a, b) >= 4;
  }
  // Input/output do not overlap, output starts zero, and every digit is <base.
  // A tile coefficient accumulates at most rows products. With incoming carry
  // <=rows*(base-1) and a prior result digit <base, normalization stays strictly
  // below rows*base^2, bounded by Wide. No architecture-specific ISA is needed.
  static void multiply(Input a, Input b, Buffer result)
  {
    assert(result.size() >= a.size() + b.size());
    if constexpr (rows < 2) {
      scalar(a, b, result);
    }
    else {
      if (std::min(a.size(), b.size()) < 4) {
        scalar(a, b, result);
        return;
      }
      GD_BC_PROFILE_EVENT(Profile::Event::blocked_multiply_calls, 1);
      std::array<Wide, rows + columns> accum;
      for (std::size_t a_start = 0; a_start < a.size(); a_start += rows) {
        auto const a_count = std::min(rows, a.size() - a_start);
        for (std::size_t b_start = 0; b_start < b.size(); b_start += columns) {
          auto const b_count = std::min(columns, b.size() - b_start);
          auto const count = a_count + b_count;
          std::fill_n(accum.begin(), count, Wide{0});
          for (std::size_t i = 0; i < a_count; ++i) {
            Wide const digit = a[a_start + i];
            for (std::size_t j = 0; j < b_count; ++j) {
              accum[i + j] += digit * b[b_start + j];
            }
          }
          auto index = a_start + b_start;
          Wide carry = 0;
          for (std::size_t k = 0; k < count; ++k, ++index) {
            carry += accum[k] + result[index];
            result[index] = static_cast<Limb>(carry % base);
            carry /= base;
          }
          while (carry != 0) {
            assert(index < result.size());
            carry += result[index];
            result[index++] = static_cast<Limb>(carry % base);
            carry /= base;
          }
        }
      }
    }
  }
};
}  // namespace GD::Bc::Details
#endif
