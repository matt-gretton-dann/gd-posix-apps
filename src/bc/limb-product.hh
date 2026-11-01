/** \file limb-product.hh
 * Portable coefficient multiplication with caller-owned recursive scratch.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_LIMB_PRODUCT_HH
#define GD_BC_LIMB_PRODUCT_HH

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <limits>
#include <span>
#include <stdexcept>

#include "limb-schoolbook.hh"
#include "limb-square.hh"
#include "profiling.hh"

namespace GD::Bc::Details {
/** A coefficient kernel; decimal scale, sign and COW stay with BasicDigits.
 *
 * Inputs and output must not overlap. Output starts zero-filled. Each recursive
 * low/high product writes a disjoint part of it; only the middle product needs
 * clearing scratch. No recursive call allocates, grows or relocates storage.
 * Arithmetic uses the same radix/widened type contract as BasicDigits.
 */
template<typename Traits>
class LimbProduct
{
  using Limb = typename Traits::NumType;
  using Wide = typename Traits::WideType;
  using Input = std::span<Limb const>;
  using Buffer = std::span<Limb>;
  static constexpr Wide base = Traits::base_;

  static auto trim(Input value) -> Input
  {
    while (!value.empty() && value.back() == 0) {
      value = value.first(value.size() - 1);
    }
    return value;
  }

  static auto sum(Input a, Input b, Buffer result) -> Input
  {
    auto const count = std::max(a.size(), b.size());
    assert(result.size() >= count + 1);
    Wide carry = 0;
    for (std::size_t i = 0; i < count; ++i) {
      carry += (i < a.size() ? a[i] : 0) + Wide(i < b.size() ? b[i] : 0);
      result[i] = static_cast<Limb>(carry % base);
      carry /= base;
    }
    result[count] = static_cast<Limb>(carry);
    return trim(result.first(count + 1));
  }

  // Middle = (a0+a1)*(b0+b1)-a0*b0-a1*b1 is nonnegative after
  // either subtraction. Long leading zero ranges are allowed in either input.
  static void subtract(Buffer value, Input rhs)
  {
    assert(value.size() >= rhs.size());
    Wide borrow = 0;
    for (std::size_t i = 0; i < value.size(); ++i) {
      Wide const subtrahend = borrow + (i < rhs.size() ? rhs[i] : 0);
      Wide const digit = value[i];
      borrow = digit < subtrahend ? 1 : 0;
      value[i] = static_cast<Limb>(digit + borrow * base - subtrahend);
    }
    assert(borrow == 0);
  }

  static void accumulate(Buffer result, Input value)
  {
    value = trim(value);
    assert(value.size() <= result.size());
    Wide carry = 0;
    std::size_t i = 0;
    for (; i < value.size(); ++i) {
      carry += Wide(result[i]) + value[i];
      result[i] = static_cast<Limb>(carry % base);
      carry /= base;
    }
    while (carry != 0) {
      assert(i < result.size());
      carry += result[i];
      result[i++] = static_cast<Limb>(carry % base);
      carry /= base;
    }
  }

  static void basic(Input a, Input b, Buffer result)
  {
    if constexpr (LimbSquare<Traits>::supported) {
      if (a.data() == b.data() && a.size() == b.size()) {
        LimbSquare<Traits>::multiply(a, result);
        return;
      }
    }
    GD_BC_PROFILE_EVENT(Profile::Event::basic_multiply_calls, 1);
    if (LimbSchoolbook<Traits>::eligible(a.size(), b.size())) {
      LimbSchoolbook<Traits>::multiply(a, b, result);
      return;
    }
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
  /** Reserve a conservative bound for the largest recursive branch.
   * Each level uses two sums of at most k limbs and their 2k-limb product,
   * where k=ceil(n/2)+1. Low/high calls reuse the whole buffer sequentially;
   * the middle call uses its suffix. At n>=4, k<n, so the bound terminates.
   * A three-limb leaf avoids nonshrinking carry sums at tiny split points.
   */
  static auto workspace_size(std::size_t count, std::size_t split_point) -> std::size_t
  {
    auto const leaf = std::max(split_point, std::size_t{3});
    std::size_t size = 0;
    while (count > leaf) {
      count = count / 2 + count % 2 + 1;
      if (count > (std::numeric_limits<std::size_t>::max() - size) / 4) {
        throw std::length_error("bc multiplication workspace too large");
      }
      size += 4 * count;
    }
    return size;
  }

  static void multiply(Input a, Input b, Buffer result, Buffer scratch, std::size_t split_point)
  {
    a = trim(a);
    b = trim(b);
    assert(result.size() >= a.size() + b.size());
    if (a.empty() || b.empty()) {
      return;
    }
    auto const longer = std::max(a.size(), b.size());
    auto const shorter = std::min(a.size(), b.size());
    // A half of an asymmetric operand would be empty. Keep the portable
    // schoolbook fallback; blocked asymmetric dispatch is a separate experiment.
    if (shorter <= std::max(split_point, std::size_t{3}) || shorter <= longer / 2) {
      basic(a, b, result);
      return;
    }
    GD_BC_PROFILE_EVENT(Profile::Event::karatsuba_multiply_calls, 1);
    auto const half = longer / 2;
    auto const a0 = a.first(half);
    auto const a1 = a.subspan(half);
    auto const b0 = b.first(half);
    auto const b1 = b.subspan(half);
    auto const low = result.first(2 * half);
    auto const high = result.subspan(2 * half, a1.size() + b1.size());
    multiply(a0, b0, low, scratch, split_point);
    multiply(a1, b1, high, scratch, split_point);

    auto const a_count = std::max(a0.size(), a1.size()) + 1;
    auto const b_count = std::max(b0.size(), b1.size()) + 1;
    auto const middle_count = a_count + b_count;
    assert(scratch.size() >= 2 * middle_count);
    auto const a_sum = sum(a0, a1, scratch.first(a_count));
    // Preserve coefficient identity through the middle square as well.
    auto const b_sum = a.data() == b.data() && a.size() == b.size()
                         ? a_sum
                         : sum(b0, b1, scratch.subspan(a_count, b_count));
    auto const middle = scratch.subspan(middle_count, middle_count);
    std::fill(middle.begin(), middle.end(), Limb{0});
    multiply(a_sum, b_sum, middle, scratch.subspan(2 * middle_count), split_point);
    subtract(middle, low);
    subtract(middle, high);
    accumulate(result.subspan(half), middle);
  }
};
}  // namespace GD::Bc::Details
#endif
