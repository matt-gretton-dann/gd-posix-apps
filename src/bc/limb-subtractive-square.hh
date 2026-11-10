/** \file limb-subtractive-square.hh
 * Portable Karatsuba squares with caller-owned, reusable scratch.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_LIMB_SUBTRACTIVE_SQUARE_HH
#define GD_BC_LIMB_SUBTRACTIVE_SQUARE_HH

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <limits>
#include <span>
#include <stdexcept>
#include <utility>

#include "limb-square.hh"

namespace GD::Bc::Details {
/** Square disjoint borrowed coefficients, leaving scale/sign to BasicDigits.
 * Output starts zero-filled. For x=a+B^m*b, compute a^2 and b^2 directly
 * into disjoint output spans and d^2=abs(a-b)^2 in scratch. The middle
 * coefficient a^2+b^2-d^2=2ab is nonnegative. Difference operands need no
 * extra carry limb, so every recursive operand is at most ceil(n/2).
 * No recursive call allocates or invalidates a borrowed span.
 */
template<typename Traits>
class LimbSubtractiveSquare
{
  using Limb = typename Traits::NumType;
  using Wide = typename Traits::WideType;
  using Input = std::span<Limb const>;
  using Buffer = std::span<Limb>;
  static constexpr Wide base = Traits::base_;

  static auto trim(Input value) -> Input
  {
    while (!value.empty() && value.back() == 0)
      value = value.first(value.size() - 1);
    return value;
  }

  static auto difference(Input a, Input b, Buffer result) -> Input
  {
    a = trim(a);
    b = trim(b);
    if (a.size() < b.size() ||
        (a.size() == b.size() &&
         std::lexicographical_compare(a.rbegin(), a.rend(), b.rbegin(), b.rend())))
      std::swap(a, b);
    assert(result.size() >= a.size());
    Wide borrow = 0;
    for (std::size_t i = 0; i < result.size(); ++i) {
      Wide const digit = i < a.size() ? a[i] : 0;
      Wide const subtrahend = borrow + (i < b.size() ? b[i] : 0);
      borrow = digit < subtrahend ? 1 : 0;
      result[i] = static_cast<Limb>(digit + borrow * base - subtrahend);
    }
    assert(borrow == 0);
    return trim(result);
  }

  // Replace d^2 with a^2+b^2-d^2. Carry is -1, 0 or 1; no signed
  // widened arithmetic is required, including for uint64_t accumulators.
  static void middle(Buffer value, Input low, Input high)
  {
    int carry = 0;
    for (std::size_t i = 0; i < value.size(); ++i) {
      Wide const addition = Wide(i < low.size() ? low[i] : 0) +
                            Wide(i < high.size() ? high[i] : 0) + (carry > 0 ? 1 : 0);
      Wide const subtrahend = Wide(value[i]) + (carry < 0 ? 1 : 0);
      if (addition < subtrahend) {
        value[i] = static_cast<Limb>(addition + base - subtrahend);
        carry = -1;
      }
      else {
        Wide const digit = addition - subtrahend;
        value[i] = static_cast<Limb>(digit % base);
        carry = static_cast<int>(digit / base);
      }
    }
    assert(carry == 0);
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

public:
  /** A level borrows h limbs for abs(a-b), 2h+1 for its square/middle,
   * and the scratch for an h-limb child, where h=ceil(n/2). Low and high
   * squares reuse the entire buffer before either temporary becomes live.
   * All capacity arithmetic is checked before the caller allocates.
   */
  static auto workspace_size(std::size_t count, std::size_t split_point) -> std::size_t
  {
    auto const maximum = std::numeric_limits<std::size_t>::max();
    if (count > maximum / 2)
      throw std::length_error("bc square result too large");
    auto const leaf = std::max(split_point, std::size_t{1});
    std::size_t size = 0;
    while (count > leaf) {
      count = count / 2 + count % 2;
      if (size == maximum || count > (maximum - size - 1) / 3)
        throw std::length_error("bc square workspace too large");
      size += 3 * count + 1;
    }
    return size;
  }

  static void multiply(Input input, Buffer result, Buffer scratch, std::size_t split_point)
  {
    static_assert(LimbSquare<Traits>::supported);
    input = trim(input);
    assert(input.size() <= result.size() / 2);
    if (input.empty())
      return;
    if (input.size() <= std::max(split_point, std::size_t{1})) {
      LimbSquare<Traits>::multiply(input, result);
      return;
    }
    auto const half = input.size() / 2;
    GD_BC_PROFILE_EVENT(Profile::Event::karatsuba_multiply_calls, 1);
    auto const high_count = input.size() - half;
    auto const low = result.first(2 * half);
    auto const high = result.subspan(2 * half, 2 * high_count);
    multiply(input.first(half), low, scratch, split_point);
    multiply(input.subspan(half), high, scratch, split_point);

    auto const temporary_size = 3 * high_count + 1;
    assert(scratch.size() >= temporary_size);
    auto const diff = difference(input.first(half), input.subspan(half), scratch.first(high_count));
    auto const cross = scratch.subspan(high_count, 2 * high_count + 1);
    std::fill(cross.begin(), cross.end(), Limb{0});
    multiply(diff, cross, scratch.subspan(temporary_size), split_point);
    middle(cross, low, high);
    accumulate(result.subspan(half), cross);
  }
};
}  // namespace GD::Bc::Details
#endif
