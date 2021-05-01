/** \file   include/util/number.hh
 *  \brief  Arbitrary precision number
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef _SRC_INCLUDE_UTIL_NUMBER_HH_INCLUDED
#define _SRC_INCLUDE_UTIL_NUMBER_HH_INCLUDED

#include "gd/string.h"

#include <memory>
#include <ostream>
#include <stdint.h>
#include <vector>

#include <string_view>

namespace GD::Bc {

/** \brief  Traits for BasicNumber when uint32_t is a good storage format.  Usual for P64 machines.
 */
struct NumberTraits32
{
  using NumType = ::uint32_t;
  using WideType = ::uint64_t;
  static constexpr WideType base_ = 1000000000U;
  static constexpr unsigned base_log10_ = 9;
};

/** \brief  Traits for BasicNumber when uint16_t is a good storage format.  Usual for P32 machines.
 */
struct NumberTraits16
{
  using NumType = ::uint16_t;
  using WideType = ::uint32_t;
  static constexpr WideType base_ = 10000U;
  static constexpr unsigned base_log10_ = 4;
};

/** \brief  Traits for BasicNumber when uint8_t is a good storage format.  Usual for testing.
 */
struct NumberTraits8
{
  using NumType = ::uint8_t;
  using WideType = ::uint16_t;
  static constexpr WideType base_ = 100U;
  static constexpr unsigned base_log10_ = 2;
};

namespace Details {

/** \brief  calculate 10 ^ \a pow. */
template<typename NumType>
NumType pow10(NumType pow)
{
  NumType r = 1;
  while (pow-- > 0) {
    r *= 10;
  }
  return r;
}

/** \brief       Report an error and exit with exit code 1.
 *  \param  msg  Message ID
 *  \param  args Arguments for the message.
 */
template<typename... Ts>
[[noreturn]] void error(Msg msg, Ts... args)
{
  std::cerr << GD::Bc::Messages::get().format(GD::Bc::Set::bc, msg, args...) << '\n';
  ::exit(1);
}

enum class ComparisonResult { less_than, equality, greater_than };

template<typename Traits>
class BasicDigits
{
public:
  using NumType = Traits::NumType;    ///< Underlying storage type.
  using WideType = Traits::WideType;  ///< Type able to hold result of NumType::max * NumType::max
  static constexpr WideType base_ = Traits::base_;              ///< Base we're storing in.
  static constexpr unsigned base_log10_ = Traits::base_log10_;  ///< Log10 of base_.

  BasicDigits() : digits_(nullptr) {}
  ~BasicDigits() = default;
  BasicDigits(BasicDigits const&) = default;
  BasicDigits& operator=(BasicDigits const&) = default;
  BasicDigits(BasicDigits&&) = default;
  BasicDigits& operator=(BasicDigits&&) = default;

  /** \brief Is this zero?  */
  bool is_zero() const
  {
    /* We're zero if we have no digits_ vector or all the digits in it are zero.  */
    if (!digits_) {
      return true;
    }

    for (auto d : *digits_) {
      if (d != 0) {
        return false;
      }
    }
    return true;
  }

  Details::ComparisonResult compare(BasicDigits const& rhs, NumType scale) const
  {
    assert(digits_);
    assert(rhs.digits_);
    Details::ComparisonResult result = Details::ComparisonResult::equality;
    for_each(rhs, scale, [&result](NumType lhs, NumType rhs) {
      if (lhs < rhs) {
        result = Details::ComparisonResult::less_than;
      }
      else if (lhs > rhs) {
        result = Details::ComparisonResult::greater_than;
      }
    });
    return result;
  }

  /** \brief  Get the number as the unsigned value.  Errors if out of range or not an integer.
   *  \param scale Scale to treat the number as having.
   */
  NumType to_unsigned(NumType scale) const
  {
    if (!digits_) {
      return 0;
    }

    auto it = digits_->begin();
    while (scale >= base_log10_) {
      if (it == digits_->end()) {
        return 0;
      }
      if (*it != 0) {
        error(Msg::number_to_unsigned_failed_fractional, "<NUMBER>");
      }
      ++it;
      scale -= base_log10_;
    }

    if (it == digits_->end()) {
      return 0;
    }

    auto pow10_scale = pow10(scale);
    NumType result = *it / pow10_scale;

    ++it;
    if (it == digits_->end()) {
      return result;
    }

    NumType temp = *it;
    auto t2 = temp % pow10_scale;
    result += t2 * (base_ / pow10_scale);
    temp -= t2;
    if (temp > std::numeric_limits<NumType>::max() / pow10_scale) {
      error(Msg::number_to_unsigned_failed_too_large, "<NUMBER>");
    }

    ++it;
    if (it == digits_->end()) {
      return result;
    }

    error(Msg::number_to_unsigned_failed_too_large, "<NUMBER>");
  }

  void debug(std::ostream& os) const
  {
    char const* sep = "{";
    if (digits_) {
      for (auto d : *digits_) {
        os << sep << d;
        sep = ", ";
      }
    }
    os << "}";
  }

  /** \brief      Multiply-accumulate of over the underlying digits.
   *  \param  mul Number to multiply by
   *  \param  acc Number to add.
   *
   * Does `digits_ * mul + acc`.  Treating `digits_` as an integer.
   */
  void mac(NumType mul, NumType acc)
  {
    copy_on_write();

    WideType carry = acc;

    for (auto d = digits_->begin(); d != digits_->end(); ++d) {
      WideType dw = *d;
      dw = dw * mul + carry;
      carry = dw / base_;
      *d = static_cast<NumType>(dw % base_);
    }

    if (carry != 0) {
      digits_->push_back(carry);
    }
  }

private:
  using DigitVector = std::vector<NumType>;

  /** \brief  Ensure digits_ is populated in a way that is modifiable.
   */
  void copy_on_write()
  {
    if (!digits_) {
      digits_ = std::make_shared<DigitVector>();
    }
    if (digits_.use_count() > 1) {
      digits_ = std::make_shared<DigitVector>(*digits_);
    }
  }

  /** \brief       Iterate over the digits of \c *this & \a rhs calling \a fn.
   *  \param rhs   Right hand side set of digits to iterate over.
   *  \param scale Scale differnce between \c *this & \a rhs.
   *  \param fn    Function to call, prototype compatable with void Fn(NumType lhs, NumType rhs);
   *
   * \a scale is how many more fractional digits \c *this has compared to \a rhs.
   *
   * \c *this and \a rhs are lined up so that they have the same scale, and then \a Fn is called for
   * every set of digits.
   *
   * It is required that the caller has checked that \c digits_ & \c rhs.digits_ are not \c nullptr
   * before calling.
   */
  template<typename Fn>
  void for_each(BasicDigits const& rhs, NumType scale, Fn fn) const
  {
    assert(digits_ && rhs.digits_);

    auto it_lhs = digits_->begin();
    auto it_rhs = rhs.digits_->begin();

    while (scale >= base_log10_) {
      NumType d_lhs = (it_lhs == digits_->end()) ? 0 : *it_lhs;
      Fn(d_lhs, 0);
      if (it_lhs != digits_->end()) {
        ++it_lhs;
      }
      scale -= base_log10_;
    }

    auto pow10_scale = pow10(scale);
    WideType carry = 0;
    for (auto it_rhs : *rhs.digits_) {
      carry += it_rhs * pow10_scale;
      NumType d_rhs = static_cast<NumType>(carry % base_);
      carry /= base_;

      NumType d_lhs = (it_lhs == digits_->end()) ? 0 : *it_lhs;
      assert(carry < base_);
      Fn(d_lhs, d_rhs);
      if (it_lhs != digits_->end()) {
        ++it_lhs;
      }
    }

    if (carry != 0) {
      NumType d_lhs = (it_lhs == digits_->end()) ? 0 : *it_lhs;
      Fn(d_lhs, static_cast<NumType>(carry));
      if (it_lhs != digits_->end()) {
        ++it_lhs;
      }
    }

    while (it_lhs != digits_->end()) {
      Fn(*it_lhs, 0);
    }
  }

  std::shared_ptr<DigitVector> digits_ = nullptr;  ///< BasicDigits in number.
};

}  // namespace Details

/** \brief   Arbitrary precision number.
 *  \tparam Traits Traits type.
 *
 * This represents an arbitrary precision number, as used by `bc`.
 *
 * Use `Number` as the best choice for the current target.
 *
 * \subsection Traits type.
 *
 * The traits type supplied for the template needs to provide the following using/typedefs:
 *
 *  * `NumType`: Underlying arithmetic type used for storage - an unsigned integral type.  Must be
 *     able to hold `base_` without overflow.
 *  * `WideType`: Type able to store `base_ * base_` without overflow.  Normally double the width of
 *    `NumType`.
 *
 * It should also define the following values:
 *
 *  * `base_`: Base of values stored in underlying storage (should be multiple of 10).
 *  * `base_log10_`: Log-10 of `base_`.
 *
 * \subsection Implementation details.
 *
 * We store a number in three components:
 *
 *  * `digits_`: a vector of digits stored in base `Traits::base_`.
 *  * `sign_`: Sign of the number.
 *  * `scale_`: Position of decimal point.
 *
 * Because of how `bc` is specified we store digits in a base that is a multiple of 10, as this
 * simplifies calculations.
 *
 * Reconstructing a number can be done by doing:
 *
 * \code
 *  sign_ * (digits_[0] * (Traits::base_ ** 0)
 *           + ... + digits_[i] * (Traits::base_ ** i)
 *           + ... + digits_[n - 1] * (Traits::base_ ** (n - 1))) * (10 ** (-scale))
 * \endcode
 *
 * Where `x ** y` means `x` to the power of `y`, and `n` is `digits_.size()`.
 *
 * `digits_` is stored as a shared pointer so that we don't need to keep duplicating it when we're
 * not going to change the values.
 */
template<typename Traits>
class BasicNumber
{
public:
  using NumType = Traits::NumType;    ///< Underlying storage type.
  using WideType = Traits::WideType;  ///< Type able to hold result of NumType::max * NumType::max
  static constexpr WideType base_ = Traits::base_;              ///< Base we're storing in.
  static constexpr unsigned base_log10_ = Traits::base_log10_;  ///< Log10 of base_.

  /** \brief  Sign enumeration. */
  enum class Sign {
    positive,  ///< Positive number
    negative   ///< Negative number
  };

  BasicNumber() : digits_(), sign_(Sign::positive), scale_(0) {}
  ~BasicNumber() = default;
  BasicNumber(BasicNumber const&) = default;
  BasicNumber& operator=(BasicNumber const&) = default;
  BasicNumber(BasicNumber&&) = default;
  BasicNumber& operator=(BasicNumber&&) = default;

  /** \brief       Construct a number.
   *  \param s     String representing the number.
   *  \param ibase Input base in range [2, 16]
   *
   * Can only handle positive numbers.
   */
  BasicNumber(std::string_view s, NumType ibase) : digits_(), sign_(Sign::positive), scale_(0)
  {
    assert(ibase >= 2);
    assert(ibase <= 16);

    static constexpr char const* digits = "0123456789ABCDEF"; /* Acceptable input characters.  */
    bool seen_period = false;                                 /* Have we seen the radix-point. */

    for (auto c : s) {
      if (seen_period) {
        ++scale_;
      }

      auto digit = ::strchr(digits, c);
      if (digit != nullptr) {
        assert((digit - digits < ibase) || s.size() == 1);
        digits_.mac(ibase, digit - digits);
      }
      else if (c == '.') {
        assert(!seen_period);
        seen_period = true;
      }
      else {
        assert(false);
      }
    }
  }

  /** \brief  Construct a basic number based upon the underlying type. */
  BasicNumber(NumType value) : digits_(), sign_(Sign::positive), scale_(0)
  {
    digits_.mac(base_, value);
  }

  NumType to_unsigned() const
  {
    if (sign_ == Sign::negative) {
      Details::error(Msg::number_to_unsigned_failed_negative, "<NUMBER>");
      return 0;
    }
    return digits_.to_unsigned(scale_);
  }

  /** \brief  Debug output of a number.  */
  void debug(std::ostream& os) const
  {
    os << "Number(";
    digits_.debug(os);
    os << ", sign=" << (sign_ == Sign::positive ? "+" : "-") << ", scale=" << scale_ << ")";
  }

  bool operator==(BasicNumber const& rhs) const
  {
    return compare(rhs) == Details::ComparisonResult::equality;
  }

  bool operator<(BasicNumber const& rhs) const
  {
    return compare(rhs) == Details::ComparisonResult::less_than;
  }

  bool operator>(BasicNumber const& rhs) const
  {
    return compare(rhs) == Details::ComparisonResult::greater_than;
  }

private:
  Details::ComparisonResult compare(BasicNumber const& rhs) const
  {
    bool lhs_zero = digits_.is_zero();
    bool rhs_zero = digits_.is_zero();

    if (lhs_zero && rhs_zero) {
      return Details::ComparisonResult::equality;
    }
    if (lhs_zero) {
      return rhs.sign_ == Sign::positive ? Details::ComparisonResult::less_than
                                         : Details::ComparisonResult::greater_than;
    }
    if (rhs_zero || (sign_ != rhs.sign_)) {
      return sign_ == Sign::negative ? Details::ComparisonResult::less_than
                                     : Details::ComparisonResult::greater_than;
    }

    bool lhs_scale_bigger = scale_ >= rhs.scale_;
    auto result = lhs_scale_bigger ? digits_.compare(rhs.digits_, scale_ - rhs.scale_)
                                   : rhs.digits_.compare(digits_, rhs.scale_ - scale_);
    if (result == Details::ComparisonResult::equality) {
      return result;
    }
    return lhs_scale_bigger ? result
                            : (result == Details::ComparisonResult::less_than
                                 ? Details::ComparisonResult::greater_than
                                 : Details::ComparisonResult::less_than);
  }

  Details::BasicDigits<Traits> digits_;  ///< The digits.
  Sign sign_ = Sign::positive;           ///< Sign (+/-1).
  NumType scale_ = 0;                    ///< Scale.
};

template<typename Traits>
bool operator!=(BasicNumber<Traits> const& lhs, BasicNumber<Traits> const& rhs)
{
  return !(lhs == rhs);
}

template<typename Traits>
bool operator>=(BasicNumber<Traits> const& lhs, BasicNumber<Traits> const& rhs)
{
  return !(lhs < rhs);
}

template<typename Traits>
bool operator<=(BasicNumber<Traits> const& lhs, BasicNumber<Traits> const& rhs)
{
  return !(lhs > rhs);
}

/** Number using efficient machine format for storage.
 *
 * Currently we assume 32-bit storage.
 */
using Number = BasicNumber<NumberTraits32>;

}  // namespace GD::Bc

#endif  // _SRC_INCLUDE_UTIL_NUMBER_HH_INCLUDED
