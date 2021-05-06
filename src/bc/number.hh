/** \file   include/util/number.hh
 *  \brief  Arbitrary precision number
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef _SRC_INCLUDE_UTIL_NUMBER_HH_INCLUDED
#define _SRC_INCLUDE_UTIL_NUMBER_HH_INCLUDED

#include "gd/string.h"

#include <iomanip>
#include <memory>
#include <ostream>
#include <sstream>
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
  using NumType = typename Traits::NumType;  ///< Underlying storage type.
  using WideType =
    typename Traits::WideType;  ///< Type able to hold result of NumType::max * NumType::max
  static constexpr WideType base_ = Traits::base_;              ///< Base we're storing in.
  static constexpr unsigned base_log10_ = Traits::base_log10_;  ///< Log10 of base_.

  /** \brief  Basic constructor.  */
  BasicDigits() : digits_(nullptr) {}

  /** \brief   Construct holding the single digit \a num
   *  \param num Number to hold.  Must be less than \c base_.
   */
  explicit BasicDigits(NumType num)
      : digits_(std::make_shared<DigitVector>(std::initializer_list{num}))
  {
    assert(num < base_);
  }

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

  void output(std::ostream& os, NumType obase, NumType scale) const
  {
    assert(obase >= 2);
    auto [whole, frac] = split_frac(scale);
    DigitVector whole_digits;
    while (!whole.is_zero()) {
      whole_digits.push_back(whole.divide(obase));
    }
    std::string number;
    for (auto it = whole_digits.rbegin(); it != whole_digits.rend(); ++it) {
      number += to_string(*it, obase);
    }

    if (number.empty()) {
      number = "0";
    }

    if (scale != 0) {
      number += '.';
      BasicDigits s(1);
      s.mul_pow10(scale);
      while (!s.is_zero()) {
        number += to_string(frac.mul_mod_pow10(obase, scale), obase);
        s.divide(obase);
      }
    }

    std::string::size_type p = 0;
    if (number.length() > 70) {
      while (p >= number.length() - 70) {
        os << number.substr(p, 70) << "\\\n";
        p += 70;
      }
    }
    if (p != number.length()) {
      os << number.substr(p);
    }
  }

  /** \brief       Convert \a num to a string in base \a obase.
   *  \param num   Number to convert, must be less than \a obase.
   *  \param obase Base for output
   *  \return      String representation of number.
   */
  static std::string to_string(NumType num, NumType obase)
  {
    assert(num < obase);
    static char const* nums = "0123456789ABCDEF";
    if (obase <= 16) {
      return std::string(1, nums[num]);
    }
    else {
      auto obase_width = std::to_string(obase - 1).size();
      std::string str = std::to_string(num);
      return std::string(" ") + std::string(obase_width - str.size(), '0') + str;
    }
  }

  /** \brief        Multiply by \a mul mod 10 ^ \a scale.
   *  \param  mul   Multiplicand
   *  \param  scale Power of ten to mod by
   *  \return       The "remainder" that falls off the most significant end.
   *
   * Basically this sets digits_ to (digits_ * mul) % (10 ^ scale) and returns
   * (digits_ * mul) / (10 ^ scale).
   *
   * We assume that the result is less than base_.
   */
  NumType mul_mod_pow10(NumType mul, NumType scale)
  {
    if (!digits_) {
      return 0;
    }

    mac(mul, 0);
    auto it = digits_->begin();
    if (it == digits_->end()) {
      return 0;
    }
    while (scale >= base_log10_) {
      if (++it == digits_->end()) {
        return 0;
      }
      scale -= base_log10_;
    }

    WideType scale_pow10 = pow10(scale);
    WideType result = *it / scale_pow10;
    *it++ -= static_cast<NumType>(result * scale_pow10);

    if (it != digits_->end()) {
      result += *it++ * (base_ / scale_pow10);
      assert(it == digits_->end());
      assert(result < base_);
      digits_->pop_back();
    }

    tidy();
    return static_cast<NumType>(result);
  }

  /**               Split ourselves into the whole number part and the fractional part.
   *  \param  scale Digit to do the split at.
   *  \return       {whole, frac} pair. \c whole has effective scale 0, and \c frac has effective
   *                scale \a scale.
   */
  std::pair<BasicDigits, BasicDigits> split_frac(NumType scale) const
  {
    BasicDigits whole;
    BasicDigits frac;

    if (is_zero()) {
      return std::make_pair(whole, frac);
    }

    whole.copy_on_write();
    frac.copy_on_write();

    /* Handle digits_ entries that are completely in the fraction.  */
    auto it = digits_->begin();
    while (scale >= base_log10_) {
      frac.digits_->push_back(it == digits_->end() ? 0 : *it++);
      scale -= base_log10_;
    }

    WideType scale_pow10 = pow10(scale);

    /* Handle the one (and only) digits_ entry that has both fractional and whole part.  */
    if (scale > 0) {
      if (it == digits_->end()) {
        /* Guarantee that frac has the correct number of digits available for the scale.  */
        frac.digits_->push_back(0);
        return std::make_pair(whole, frac);
      }

      frac.digits_->push_back(*it % scale_pow10);
    }

    /* Handle the digits_ entries that are whole numbers.  */
    WideType carry = it == digits_->end() ? 0 : *it++ / scale_pow10;
    while (it != digits_->end()) {
      carry += (*it++) * (base_ / scale_pow10);
      whole.digits_->push_back(static_cast<NumType>(carry % base_));
      carry /= base_;
    }

    whole.digits_->push_back(static_cast<NumType>(carry));
    whole.tidy();
    /* We don't tidy frac as we want it to have the correct number of scale units.  */

    return std::make_pair(whole, frac);
  }

  void output_base10(std::ostream& os, NumType scale) const
  {
    /* For base 10 we can rely on the underlying system-library's output routines the only corner
     * cases are to: 1) Ensure we pad with zeroes where necessary; and 2) Insert the decimal point
     * in the correct place.
     */
    assert(!is_zero());

    /* Generate string without radix point. Note we reverse through the digits as we want to print
     * big-endian, but numbers are stored little-endian.
     */
    std::ostringstream ss;
    unsigned width = 0;
    for (auto rit = digits_->rbegin(); rit != digits_->rend(); ++rit) {
      ss << std::setfill('0') << std::setw(width) << *rit;
      width = base_log10_;
    }
    std::string result = ss.str();
    auto it = result.begin();
    bool print = false;
    NumType digits_printed = 0;
    while (result.end() - it > scale) {
      if (*it != '0') {
        print = true;
      }
      if (print) {
        os << *it;
        if (++digits_printed == 70) {
          digits_printed = 0;
          os << "\\\n";
        }
      }
      ++it;
    }

    if (!print) {
      ++digits_printed;
      os << '0';
    }

    if (it != result.end()) {
      os << '.';
      ++digits_printed;
      if (digits_printed >= 70) {
        os << "\\\n";
        digits_printed = 0;
      }

      assert(scale >= result.end() - it);
      while (result.end() - it > 70 - digits_printed) {
        os << result.substr(result.end() - it, 70 - digits_printed) << "\\\n";
        scale -= 70 - digits_printed;
        it += 70 - digits_printed;
        digits_printed = 0;
      }
      if (it != result.end()) {
        os << std::setfill('0') << std::right << std::setw(scale)
           << result.substr(it - result.begin());
      }
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

  /** Multiply by 10^ \a scale. */
  void mul_pow10(NumType scale)
  {
    copy_on_write();

    /* Do the whole digit steps first as this is just memory shuffling.  */
    NumType extra_digits = scale / base_log10_;
    auto init_size = digits_->size();
    digits_->resize(init_size + extra_digits);
    for (auto i = init_size; i > 0; --i) {
      (*digits_)[i + extra_digits - 1] = (*digits_)[i - 1];
    }
    for (typename DigitVector::size_type i = 0; i < extra_digits; ++i) {
      (*digits_)[i] = 0;
    }

    /* Now we have to multply by a number < base so can just use mac.  */
    mac(pow10(scale % base_log10_), 0);

    tidy();
  }

  /** \brief  Divide by the number \a div.  Returns remainder.  */
  NumType divide(NumType div)
  {
    copy_on_write();

    WideType carry = 0;
    for (auto d = digits_->rbegin(); d != digits_->rend(); ++d) {
      carry += *d;
      assert(carry / div < base_);
      *d = static_cast<NumType>(carry / div);
      carry = (carry % div) * base_;
    }
    tidy();

    return static_cast<NumType>(carry / base_);
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

  /** \brief        Tidy up the digits_ vector.
   */
  void tidy()
  {
    if (digits_) {
      while (!digits_->empty() && digits_->back() == 0) {
        digits_->pop_back();
      }
    }
  }

  /** \brief       Iterate over the digits of \c *this & \a rhs calling \a fn.
   *  \param rhs   Right hand side set of digits to iterate over.
   *  \param scale Scale differnce between \c *this & \a rhs.
   *  \param fn    Function to call, prototype compatable with void Fn(NumType lhs, NumType rhs);
   *
   * \a scale is how many more fractional digits \c *this has compared to \a rhs.
   *
   * \c *this and \a rhs are lined up so that they have the same scale, and then \a Fn is called
   * for every set of digits.
   *
   * It is required that the caller has checked that \c digits_ & \c rhs.digits_ are not \c
   * nullptr before calling.
   */
  template<typename Fn>
  void for_each(BasicDigits const& rhs, NumType scale, Fn fn) const
  {
    assert(digits_ && rhs.digits_);

    auto it_lhs = digits_->begin();
    auto it_rhs = rhs.digits_->begin();

    while (scale >= base_log10_) {
      NumType d_lhs = (it_lhs == digits_->end()) ? 0 : *it_lhs;
      fn(d_lhs, 0);
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
      fn(d_lhs, d_rhs);
      if (it_lhs != digits_->end()) {
        ++it_lhs;
      }
    }

    if (carry != 0) {
      NumType d_lhs = (it_lhs == digits_->end()) ? 0 : *it_lhs;
      fn(d_lhs, static_cast<NumType>(carry));
      if (it_lhs != digits_->end()) {
        ++it_lhs;
      }
    }

    while (it_lhs != digits_->end()) {
      fn(*it_lhs, 0);
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
  using NumType = typename Traits::NumType;  ///< Underlying storage type.
  using WideType =
    typename Traits::WideType;  ///< Type able to hold result of NumType::max * NumType::max
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

    /* Construct the basic number a digit at a time, working out the scale when we see digits after
     * the radix point.
     *
     * Ultimately we want this to be digits_ * 10 ^ scale_.
     * But we start by calculating digits_ * ibase ^ scale_.
     */
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

    /* We now have number as digits_ * ibase ^ scale_.  If we're in base 10 or the scale is zero
     * or the digits are zero we have finished.  */
    if (ibase == 10 || scale_ == 0 || digits_.is_zero()) {
      return;
    }

    /* The number is now scaled by a power of not-10.  We need to convert it to the appropriate
     * power of 10.  To do this we multiply by 10^scale and divide by ibase ^ scale.
     */
    digits_.mul_pow10(scale_);

    /* We divide by ibase ^ scale_ using a sequence of NumType sized divides, assuming these to be
     * quicker than calculating an arbitrary precision ibase^scale_ value and then using arbitrary
     * precision divide.
     *
     * To reduce the number of divides we work out the largest power of ibase which is less than
     * base_.
     */
    NumType t = base_;
    NumType pw = 1;
    NumType pwc = 0;
    while (t > ibase && pwc < scale_) {
      t /= ibase;
      pw *= ibase;
      ++pwc;
    }

    NumType i = scale_;
    while (i >= pwc) {
      digits_.divide(pw);
      i -= pwc;
    }
    pw = 1;
    while (i-- > 0) {
      pw *= ibase;
    }
    digits_.divide(pw);
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

  void output(std::ostream& os, NumType obase) const
  {
    if (digits_.is_zero()) {
      os << "0";
      return;
    }
    if (sign_ == Sign::negative) {
      os << "-";
    }
    if (obase == 10) {
      digits_.output_base10(os, scale_);
    }
    else {
      digits_.output(os, obase, scale_);
    }
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
