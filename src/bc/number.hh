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
        assert(digit - digits < ibase);
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

  /** \brief  Debug output of a number.  */
  void debug(std::ostream& os) const
  {
    os << "Number(";
    digits_.debug(os);
    os << ", sign=" << (sign_ == Sign::positive ? "+" : "-") << ", scale=" << scale_ << ")";
  }

private:
  Details::BasicDigits<Traits> digits_;  ///< The digits.
  Sign sign_ = Sign::positive;           ///< Sign (+/-1).
  NumType scale_ = 0;                    ///< Scale.
};

/** Number using efficient machine format for storage.
 *
 * Currently we assume 32-bit storage.
 */
using Number = BasicNumber<NumberTraits32>;

}  // namespace GD::Bc

#endif  // _SRC_INCLUDE_UTIL_NUMBER_HH_INCLUDED
