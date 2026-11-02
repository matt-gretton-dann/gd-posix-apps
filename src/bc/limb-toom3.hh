/** \file limb-toom3.hh
 * Experimental exact Toom-3 coefficient multiplication.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_LIMB_TOOM3_HH
#define GD_BC_LIMB_TOOM3_HH

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <limits>
#include <span>
#include <stdexcept>
#include <utility>
#include <vector>

#include "limb-product.hh"
#include "profiling.hh"

namespace GD::Bc::Details {
/** Evaluate at 0, 1, -1, 2 and infinity, then interpolate exactly.
 *
 * Scale, sign and ownership remain in BasicDigits. Negative evaluations at -1
 * use a separate sign and an unsigned magnitude. All interpolated polynomial
 * coefficients are nonnegative. Temporary vectors deliberately make the first
 * experiment independent of a recursive workspace schedule; their allocation
 * and peak live storage must be charged to measurements before adoption.
 *
 * The point equations are documented in the GMP algorithm manual:
 * https://gmplib.org/manual/Toom-3_002dWay-Multiplication
 */
template<typename Traits>
class LimbToom3
{
  using Limb = typename Traits::NumType;
  using Wide = typename Traits::WideType;
  using Input = std::span<Limb const>;
  using Buffer = std::span<Limb>;
  using Digits = std::vector<Limb, Profile::Allocator<Limb, Profile::AllocationKind::digits>>;
  static constexpr Wide base = Traits::base_;
  // Evaluations have at most seven times a chunk's magnitude. Scalar factors
  // up to sixteen and division remainders below three must fit in Wide.
  static constexpr bool supported = base >= 8 && std::numeric_limits<Wide>::max() / base >= 16;

  static auto trim(Input value) -> Input
  {
    while (!value.empty() && value.back() == 0)
      value = value.first(value.size() - 1);
    return value;
  }

  static void tidy(Digits& value) { value.resize(trim(value).size()); }

  static auto compare(Input a, Input b) -> int
  {
    a = trim(a);
    b = trim(b);
    if (a.size() != b.size())
      return a.size() < b.size() ? -1 : 1;
    for (auto i = a.size(); i != 0; --i) {
      if (a[i - 1] != b[i - 1])
        return a[i - 1] < b[i - 1] ? -1 : 1;
    }
    return 0;
  }

  static auto add(Input a, Input b) -> Digits
  {
    auto const count = std::max(a.size(), b.size());
    if (count == std::numeric_limits<std::size_t>::max())
      throw std::length_error("bc Toom evaluation too large");
    Digits result(count + 1);
    Wide carry = 0;
    for (std::size_t i = 0; i < count; ++i) {
      carry += Wide(i < a.size() ? a[i] : 0) + Wide(i < b.size() ? b[i] : 0);
      result[i] = static_cast<Limb>(carry % base);
      carry /= base;
    }
    result[count] = static_cast<Limb>(carry);
    tidy(result);
    return result;
  }

  // The caller proves lhs >= rhs from the nonnegative coefficient equations.
  static void subtract(Digits& lhs, Input rhs)
  {
    rhs = trim(rhs);
    assert(compare(lhs, rhs) >= 0);
    Wide borrow = 0;
    for (std::size_t i = 0; i < lhs.size(); ++i) {
      Wide const sub = borrow + Wide(i < rhs.size() ? rhs[i] : 0);
      Wide const digit = lhs[i];
      borrow = digit < sub ? 1 : 0;
      lhs[i] = static_cast<Limb>(digit + borrow * base - sub);
    }
    assert(borrow == 0);
    tidy(lhs);
  }

  static auto times(Input value, Wide factor) -> Digits
  {
    Digits result;
    result.reserve(value.size());
    Wide carry = 0;
    for (auto const digit : value) {
      carry += Wide(digit) * factor;
      result.push_back(static_cast<Limb>(carry % base));
      carry /= base;
    }
    while (carry != 0) {
      result.push_back(static_cast<Limb>(carry % base));
      carry /= base;
    }
    tidy(result);
    return result;
  }

  static void divide_exact(Digits& value, Wide divisor)
  {
    Wide remainder = 0;
    for (auto i = value.size(); i != 0; --i) {
      Wide const digit = remainder * base + value[i - 1];
      value[i - 1] = static_cast<Limb>(digit / divisor);
      remainder = digit % divisor;
    }
    assert(remainder == 0);
    tidy(value);
  }

  static auto absolute_difference(Input a, Input b) -> std::pair<Digits, bool>
  {
    bool const negative = compare(a, b) < 0;
    if (negative)
      std::swap(a, b);
    Digits result(a.begin(), a.end());
    subtract(result, b);
    return {std::move(result), negative};
  }

  static auto product(Input a, Input b, std::size_t split, std::size_t threshold) -> Digits
  {
    a = trim(a);
    b = trim(b);
    if (a.size() > std::numeric_limits<std::size_t>::max() - b.size())
      throw std::length_error("bc Toom product too large");
    Digits result(a.size() + b.size(), Limb{0});
    multiply(a, b, result, split, threshold);
    tidy(result);
    return result;
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
  /** Both operands must have three nonempty chunks. Unsupported widened types,
   * disabled thresholds and asymmetric shapes keep the existing scalar kernel.
   * k=ceil(n/3), and a point evaluation uses at most k+1 limbs for radix >=8;
   * n>=6 ensures recursive evaluations strictly shrink.
   */
  static auto eligible(std::size_t a, std::size_t b, std::size_t threshold) -> bool
  {
    auto const longer = std::max(a, b);
    auto const chunk = longer / 3 + (longer % 3 != 0 ? 1 : 0);
    return supported && threshold != 0 && longer >= std::max(threshold, std::size_t{6}) &&
           std::min(a, b) > 2 * chunk;
  }

  /** Nonoverlapping inputs/output; output is zero-filled and has a.size()+b.size()
   * limbs. Leading zero limbs are allowed. Every input limb is below radix.
   */
  static void multiply(Input a, Input b, Buffer result, std::size_t split, std::size_t threshold)
  {
    a = trim(a);
    b = trim(b);
    assert(a.size() <= result.size() && b.size() <= result.size() - a.size());
    if (a.empty() || b.empty())
      return;
    if (!eligible(a.size(), b.size(), threshold) ||
        std::min(a.size(), b.size()) <= std::max(split, std::size_t{3})) {
      Digits scratch(LimbProduct<Traits>::workspace_size(std::max(a.size(), b.size()), split));
      LimbProduct<Traits>::multiply(a, b, result, scratch, split);
      return;
    }
    GD_BC_PROFILE_EVENT(Profile::Event::toom3_multiply_calls, 1);
    bool const square = a.data() == b.data() && a.size() == b.size();
    auto const n = std::max(a.size(), b.size());
    auto const k = n / 3 + (n % 3 != 0 ? 1 : 0);
    auto const a0 = a.first(k), a1 = a.subspan(k, k), a2 = a.subspan(2 * k);
    auto const b0 = b.first(k), b1 = b.subspan(k, k), b2 = b.subspan(2 * k);
    auto const ae = add(a0, a2), be = add(b0, b2);
    auto const ap = add(ae, a1), bp = add(be, b1);
    auto const [am, a_negative] = absolute_difference(ae, a1);
    auto const [bm, b_negative] = absolute_difference(be, b1);
    auto const at = add(add(a0, times(a1, 2)), times(a2, 4));
    auto const bt = add(add(b0, times(b1, 2)), times(b2, 4));
    auto const v0 = product(a0, b0, split, threshold);
    auto const v4 = product(a2, b2, split, threshold);
    // Preserve the existing square leaf at every recursive point. Values
    // equal by coincidence are not enough: the original input ranges must
    // share identity before either evaluation can stand for both operands.
    auto const vp = product(ap, square ? Input(ap) : Input(bp), split, threshold);
    auto const vm = product(am, square ? Input(am) : Input(bm), split, threshold);
    auto v2 = product(at, square ? Input(at) : Input(bt), split, threshold);
    // s = c1+c3 and c2 = (W(1)+W(-1))/2-c0-c4. W(1)
    // dominates |W(-1)|, so no signed coefficient storage is needed.
    auto s = a_negative == b_negative ? Digits(vp) : add(vp, vm);
    auto c2 = a_negative == b_negative ? add(vp, vm) : Digits(vp);
    if (a_negative == b_negative)
      subtract(s, vm);
    else
      subtract(c2, vm);
    divide_exact(s, 2);
    divide_exact(c2, 2);
    subtract(c2, v0);
    subtract(c2, v4);
    // (W(2)-c0-16*c4)/2 = c1+2*c2+4*c3. Subtracting
    // s+2*c2 leaves exactly 3*c3; all intermediate values stay nonnegative.
    subtract(v2, v0);
    subtract(v2, times(v4, 16));
    divide_exact(v2, 2);
    subtract(v2, s);
    subtract(v2, times(c2, 2));
    divide_exact(v2, 3);
    subtract(s, v2);
    accumulate(result, v0);
    accumulate(result.subspan(k), s);
    accumulate(result.subspan(2 * k), c2);
    accumulate(result.subspan(3 * k), v2);
    accumulate(result.subspan(4 * k), v4);
  }
};
}  // namespace GD::Bc::Details
#endif
