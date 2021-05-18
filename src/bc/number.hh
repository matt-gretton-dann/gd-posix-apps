/** \file   include/util/number.hh
 *  \brief  Arbitrary precision number
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef _SRC_INCLUDE_UTIL_NUMBER_HH_INCLUDED
#define _SRC_INCLUDE_UTIL_NUMBER_HH_INCLUDED

#include "gd/string.h"

#include <iomanip>
#include <iterator>
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
  using PrintType = NumType;
  static constexpr WideType base_ = 1000000000U;
  static constexpr unsigned base_log10_ = 9;
};

/** \brief  Traits for BasicNumber when uint16_t is a good storage format.  Usual for P32 machines.
 */
struct NumberTraits16
{
  using NumType = ::uint16_t;
  using WideType = ::uint32_t;
  using PrintType = NumType;
  static constexpr WideType base_ = 10000U;
  static constexpr unsigned base_log10_ = 4;
};

/** \brief  Traits for BasicNumber when uint8_t is a good storage format.  Usual for testing.
 */
struct NumberTraits8
{
  using NumType = ::uint8_t;
  using WideType = ::uint16_t;
  using PrintType = unsigned;
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
  using PrintType = typename Traits::PrintType;                 ///< Type to use to print
  static constexpr WideType base_ = Traits::base_;              ///< Base we're storing in.
  static constexpr unsigned base_log10_ = Traits::base_log10_;  ///< Log10 of base_.

  /** \brief  Basic constructor.  */
  BasicDigits() : digits_(nullptr) {}

  /** \brief   Construct holding the single digit \a num
   *  \param num Number to hold.  Must be less than \c base_.
   */
  explicit BasicDigits(NumType num) : digits_(std::make_shared<DigitVector>())
  {
    while (num != 0) {
      digits_->push_back(static_cast<NumType>(num % base_));
      num /= static_cast<NumType>(base_);
    }
  }

  ~BasicDigits() = default;
  BasicDigits(BasicDigits const&) = default;
  BasicDigits& operator=(BasicDigits const&) = default;
  BasicDigits(BasicDigits&&) = default;
  BasicDigits& operator=(BasicDigits&&) = default;

  /** \brief  Reset the digits - sets us to zero. */
  void reset() { digits_.reset(); }

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

  /** Is this an even number? */
  bool is_even() const
  {
    if (!digits_) {
      return true;
    }

    return ((*digits_)[0] & 1) == 0;
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

  /** \brief       Output to stream
   *  \param os    Stream to output to
   *  \param obase Output base
   *  \param scale Output scale.
   *
   * Use output_base10() if output is base-10, as that uses significantly less memory to do the
   * output.
   */
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
      s.sub(NumType{1}, 0);
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

  /** \brief       Output number in base 10
   *  \param os    Stream to output to
   *  \param scale Scale of number.
   *
   * This is slightly more efficient than output(os, 10, scale) would be in terms of memory usage.
   */
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
      ss << std::setfill('0') << std::setw(width) << static_cast<typename Traits::PrintType>(*rit);
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
        os << result.substr(it - result.begin, 70 - digits_printed) << "\\\n";
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

  /** \brief  Output a debug form of the digits to \a os. */
  void debug(std::ostream& os) const
  {
    char const* sep = "{";
    if (digits_) {
      for (auto d : *digits_) {
        os << sep << static_cast<typename Traits::PrintType>(d);
        sep = ", ";
      }
    }
    os << "}";
  }

  /** \brief  Get the number of significant digits.
   *  \return Number of significant digits
   */
  NumType length() const
  {
    if (!digits_) {
      return 1;
    }

    /* The length is at most the number of entries in the digits_ vector times the log base.  We
     * then correct by reducing length by the number of leading zeros.
     */
    NumType length = digits_->size() * base_log10_;
    auto rit = digits_->rbegin();
    while (rit != digits_->rend() && *rit == 0) {
      --rit;
      length -= base_log10_;
    }
    if (rit != digits_->rend()) {
      length -= base_log10_;
      NumType d = *rit;
      assert(d != 0);
      while (d != 0) {
        length += 1;
        d /= 10;
      }
    }

    return std::max(NumType{1}, length);
  }

  /** \brief        Add a NumType value to the digits_ at a given scale
   *  \param add    Addend.
   *  \param scale  Scale to add it at.
   *
   * digits_ += add * 10 ^ scale.
   */
  void add(NumType add, NumType scale)
  {
    BasicDigits add_d(add);
    add(add_d, scale);
  }

  /** \brief       Do \c *this += \a rhs * 10 ^ \a scale.
   *  \param rhs   Right hand side
   *  \param scale Number of digits to scale \a rhs by.
   */
  void add(BasicDigits const& rhs, NumType scale)
  {
    copy_on_write();

    NumType carry = for_each(rhs, scale, [](DigitVector::iterator it, WideType carry) {
      WideType result = *it + carry;
      *it = result % base_;
      return result / base_;
    });

    if (carry != 0) {
      digits_->push_back(carry);
    }

    tidy();
  }

  /** \brief         Add a NumType value to the digits_ at a given scale
   *  \param  s      Subtractend ?.
   *  \param  scale  Scale to subtract it at.
   *  \return        True if the sign has changed.
   *
   * digits_ = abs(digits_ - sub * 10 ^ scale).
   *
   * If *this (on entry) is less than sub * 10 ^ scale we return true.  Otherwise we return false.
   */
  bool sub(NumType s, NumType scale)
  {
    BasicDigits sub_d(s);
    return sub(sub_d, scale);
  }

  /** \brief        Do \c *this = abs(\a rhs * 10 ^ \a scale), return true if sign flipped.
   *  \param  rhs   Right hand side
   *  \param  scale Number of digits to scale \a rhs by.
   *  \return       True if *this was less than rhs
   *
   * If *this (on entry) is less than sub * 10 ^ scale we return true.  Otherwise we return false.
   */
  bool sub(BasicDigits const& rhs, NumType scale)
  {
    copy_on_write();

    NumType carry = for_each(rhs, scale, [](DigitVector::iterator it, WideType carry) {
      WideType rhs_value = carry % base_;
      carry /= base_;
      if (rhs_value > *it) {
        *it += base_;
        ++carry;
      }
      *it -= rhs_value;
      return carry;
    });

    if (carry != 0) {
      if (carry != 1) {
        digits_->push_back(base_ - carry);
      }

      /* Having a carry here means that we've flipped the sign - so digits_ holds
       * result - 1 * 10^(digits_.size_ * base_log10_).  We do
       * 1 - 10^(digits_.size() * base_log10_) - digits_ to get the result back.
       *
       * We do this by hand as its a constant number and so we don't have to waste memory on a
       * temporary large number.
       */
      bool have_borrowed = false; /* Have we borrowed yet?  */
      for (auto it = digits_->begin(); it != digits_->end(); ++it) {
        /* Until we've encountered a non-zero digit the operation is 0 - 0 = 0 so don't need to
         * do anything.  Once we have a non-zero digit we've had to borrow.  For that first digit
         * the result is base_ - D, and for every other digit it is base_ - 1 - D.  Where the 1
         * is the borrow.
         */
        if (!have_borrowed && *it != 0) {
          *it = base_ - *it;
          have_borrowed = true;
        }
        else if (have_borrowed) {
          *it = base_ - 1 - *it;
        }
      }
    }

    tidy();
    return carry != 0;
  }

  void power_mul(BasicDigits const& power_in)
  {
    if (is_zero()) {
      reset();
      return;
    }

    BasicDigits<Traits> power(power_in);
    power.copy_on_write();
    BasicDigits<Traits> result(1);

    /* Divide and conquer to make number of multiplications needed O(lg2(power)).  */
    BasicDigits<Traits> one(1);
    while (!power.is_zero()) {
      if (((*power.digits_)[0] & 1) != 0) {
        result.multiply(*this, 0);
        power.sub(one, 0);
      }
      multiply(*this, 0);
      power.divide(2);
    }

    std::swap(digits_, result.digits_);
  }

  void multiply(BasicDigits const& rhs, NumType rescale)
  {
    /* Multiplying by zero is easy.  */
    if (is_zero() || rhs.is_zero()) {
      digits_.reset();
      return;
    }

    std::shared_ptr<DigitVector> result = std::make_shared<DigitVector>();
    result->reserve(digits_->size() + rhs.digits_->size());

    for (auto lhs_it = digits_->begin(); lhs_it != digits_->end(); ++lhs_it) {
      WideType carry = 0;
      typename DigitVector::size_type dist = std::distance(digits_->begin(), lhs_it);
      if (result->size() <= dist) {
        result->resize(dist, 0);
      }
      auto result_it = result->begin() + dist;
      for (auto rhs_it = rhs.digits_->begin(); rhs_it != rhs.digits_->end(); ++rhs_it) {
        result_it = ensure_it_valid(result_it, result);
        WideType lhs_d = *lhs_it;
        WideType rhs_d = *rhs_it;
        carry += lhs_d * rhs_d + *result_it;
        *result_it++ = static_cast<NumType>(carry % base_);
        carry /= base_;
      }

      while (carry != 0) {
        result_it = ensure_it_valid(result_it, result);
        carry += *result_it;
        *result_it++ = static_cast<NumType>(carry % base_);
        carry /= base_;
      }
    }

    std::swap(digits_, result);
    div_pow10(rescale);
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

  /** Return modulo.  */
  void divide(BasicDigits v)
  {
    /* Algorithm D of Knuth TAOCP 4.3.1.  */

    /* D1. Normalise.  */
    /* u =*this; */
    copy_on_write();
    tidy();
    v.copy_on_write();
    v.tidy();
    assert(digits_->size() >= v.digits_->size());

    if (v.digits_->size() == 1) {
      divide(v.digits_->at(0));
      return;
    }

    if (digits_->size() == 2) {
      assert(v.digits_->size() == 2);
      WideType u2 = (*digits_)[0] + (base_ * (*digits_)[1]);
      WideType v2 = (*v.digits_)[0] + (base_ * (*v.digits_)[1]);
      WideType q = u2 / v2;

      digits_ = std::make_shared<DigitVector>(std::initializer_list<NumType>{
        static_cast<NumType>(q % base_), static_cast<NumType>(q / base_)});
      return;
    }

    /* D1. Normalize.  */
    /* Note this is not the number Knuth picks as that one causes me problems.  This choice still
     * guarantees that d > b/2, but that we don't extend the v vector at all.
     */
    auto n = v.digits_->size();
    auto m = digits_->size() - n;

    auto d = (base_ / 2 - 1) / v.digits_->back();
    d += 1;
    if (d != 1) {
      mac(d, 0);
      v.mac(d, 0);
    }
    assert(n == v.digits_->size());
    assert(v.digits_->back() >= base_ / 2);
    digits_->push_back(0);

    BasicDigits q;
    q.digits_ = std::make_shared<DigitVector>(m + 1, 0);

    /* D2. Initialize j.  */
    auto j = m;

    do {
      /* D3. Calculate q_hat. */
      WideType t = digits_->at(n + j) * base_ + digits_->at(n + j - 1);
      WideType q_hat = t / v.digits_->at(n - 1);
      WideType r_hat = t % v.digits_->at(n - 1);

      bool cont = true;

      while (cont) {
        cont = false;
        WideType q_hat_v_n2 = q_hat * v.digits_->at(n - 2);
        WideType b_r_hat_u_j_n2 = base_ * r_hat + digits_->at(j + n - 2);
        if (q_hat == base_ || q_hat_v_n2 > b_r_hat_u_j_n2) {
          --q_hat;
          r_hat += v.digits_->at(n - 1);
          if (r_hat < base_) {
            cont = true;
          }
        }
      }

      /* D4. Multiply and subtract.   */
      WideType carry = 0;
      for (unsigned i = 0; i < n; ++i) {
        carry += v.digits_->at(i) * q_hat;
        NumType rhs_d = carry % base_;
        carry /= base_;
        if (rhs_d > digits_->at(j + i)) {
          digits_->at(j + i) += base_;
          carry += 1;
        }
        digits_->at(j + i) -= rhs_d;
      }
      if (carry != 0) {
        NumType rhs_d = carry % base_;
        carry /= base_;
        if (rhs_d > digits_->at(j + n)) {
          digits_->at(j + n) += base_;
          carry += 1;
        }
        digits_->at(j + n) -= rhs_d;
      }
      bool borrowed = (carry != 0);

      if (borrowed) {
        /* D6. Add back.  Do this now as it is simpler. */
        WideType carry = 0;
        for (typename DigitVector::size_type i = 0; i < n; ++i) {
          carry += v.digits_->at(i) + digits_->at(j + i);
          digits_->at(j + i) = carry % base_;
          carry /= base_;
        }
        carry += digits_->at(j + n);
        digits_->at(j + n) = carry % base_;
        carry /= base_;
        assert(carry == 1);
        q_hat -= 1;
      }

      /* D5. Test remainder.  */
      assert(q_hat < base_);
      assert(digits_->at(n + j) == 0);
      q.digits_->at(j) = q_hat;

    } while (j-- > 0);

    /* D7. Unormalise.  */
    std::swap(q.digits_, digits_);
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

  /** Divide by 10^ \a scale. */
  void div_pow10(NumType scale)
  {
    copy_on_write();

    /* Do the whole digit steps first as this is just memory shuffling.  */
    auto init_size = digits_->size();
    NumType offset = scale / base_log10_;
    if (offset > init_size) {
      offset = init_size;
    }
    if (offset != 0) {
      for (decltype(offset) i = 0; i < init_size - offset; ++i) {
        (*digits_)[i] = (*digits_)[i + offset];
      }
    }
    digits_->resize(init_size - offset);
    if (digits_->empty()) {
      return;
    }

    WideType scale_pow10 = pow10(scale % base_log10_);
    auto it = digits_->begin();
    WideType carry = *it / scale_pow10;
    while (++it != digits_->end()) {
      carry += static_cast<WideType>(*it) * (base_ / scale_pow10);
      *(it - 1) = carry % base_;
      carry /= base_;
    }
    *(it - 1) = carry % base_;
    tidy();
  }

  /**               Split ourselves into the whole number part and the fractional part.
   *  \param  scale Digit to do the split at.
   *  \return       {whole, frac} pair. \c whole has effective scale 0, and \c frac has
   * effective scale \a scale.
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

  /** \brief       Iterate over the digits of \c *this & \a rhs calling \a fn.
   *  \param rhs   Right hand side set of digits to iterate over.
   *  \param scale Scale differnce between \c *this & \a rhs.
   *  \param fn    Function to call, prototype compatable with void Fn(NumType lhs, NumType
   * rhs);
   *
   * \a scale is how many more fractional digits \c *this has compared to \a rhs.
   *
   * \c *this and \a rhs are lined up so that they have the same scale, and then \a Fn is
   * called for every set of digits.
   *
   * It is required that the caller has checked that \c digits_ & \c rhs.digits_ are not \c
   * nullptr before calling.
   */
  template<typename Fn>
  void for_each(BasicDigits const& rhs, NumType scale, Fn fn) const
  {
    assert(digits_ && rhs.digits_);

    auto it_lhs = digits_->begin();

    while (scale >= base_log10_) {
      NumType d_lhs = (it_lhs == digits_->end()) ? 0 : *it_lhs;
      fn(d_lhs, 0);
      if (it_lhs != digits_->end()) {
        ++it_lhs;
      }
      scale -= base_log10_;
    }

    WideType pow10_scale = pow10(scale);
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
      fn(*it_lhs++, 0);
    }
  }

  DigitVector::iterator ensure_it_valid(DigitVector::iterator it,
                                        std::shared_ptr<DigitVector> digits)
  {
    if (it == digits->end()) {
      it = digits->insert(it, 0);
    }
    return it;
  }

  /** \brief       Iterate over the digits of \c *this & \a rhs calling \a fn.
   *  \param rhs   Right hand side set of digits to iterate over.
   *  \param scale Scale differnce between \c *this & \a rhs.
   *  \param fn    Function to call, prototype compatable with void Fn(NumType lhs, NumType
   * rhs);
   *
   * \a scale is how many more fractional digits \c *this has compared to \a rhs.
   *
   * \c *this and \a rhs are lined up so that they have the same scale, and then \a Fn is
   * called for every set of digits.
   *
   * It is required that the caller has checked that \c digits_ & \c rhs.digits_ are not \c
   * nullptr before calling.
   */
  template<typename Fn>
  NumType for_each(BasicDigits const& rhs, NumType scale, Fn fn, NumType initial_carry = 0)
  {
    assert(digits_ && rhs.digits_);

    auto it = digits_->begin();
    WideType carry = initial_carry;

    while (scale >= base_log10_) {
      it = ensure_it_valid(it, digits_);
      carry = fn(it++, carry);
      assert(carry < base_);
      scale -= base_log10_;
    }

    WideType pow10_scale = pow10(scale);
    for (auto it_rhs : *rhs.digits_) {
      carry += it_rhs * pow10_scale;
      it = ensure_it_valid(it, digits_);
      carry = fn(it++, carry);
      assert(carry < base_);
    }

    while (it != digits_->end()) {
      carry = fn(it++, carry);
      assert(carry < base_);
    }

    assert(carry < base_);
    return static_cast<NumType>(carry);
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
 *  * `NumType`: Underlying arithmetic type used for storage - an unsigned integral type.  Must
 * be able to hold `base_` without overflow.
 *  * `WideType`: Type able to store `base_ * base_` without overflow.  Normally double the
 * width of `NumType`.
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
 * `digits_` is stored as a shared pointer so that we don't need to keep duplicating it when
 * we're not going to change the values.
 */
template<typename Traits>
class BasicNumber
{
public:
  using NumType = typename Traits::NumType;  ///< Underlying storage type.
  using WideType =
    typename Traits::WideType;  ///< Type able to hold result of NumType::max * NumType::max
  using PrintType = typename Traits::PrintType;                 ///< Type to use to print
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

    /* Construct the basic number a digit at a time, working out the scale when we see digits
     * after the radix point.
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

    /* We divide by ibase ^ scale_ using a sequence of NumType sized divides, assuming these to
     * be quicker than calculating an arbitrary precision ibase^scale_ value and then using
     * arbitrary precision divide.
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
    os << ", sign=" << (sign_ == Sign::positive ? "+" : "-")
       << ", scale=" << static_cast<typename Traits::PrintType>(scale_) << ")";
  }

  /* Get the scale of the number.  */
  NumType scale() const { return scale_; }

  /* Get the number of significant digits.  */
  NumType length() const
  {
    /* Note that length(x) >= scale(x) so length(0.00) is 2. */
    return std::max(scale(), digits_.length());
  }

  /** \brief  Negate \c *this.  */
  void negate() { sign_ = (sign_ == Sign::positive) ? Sign::negative : Sign::positive; }

  /** \brief  Add \a rhs to *this.
   *
   * Scale on exit is max of rhs scale and *this scale.
   */
  void add(BasicNumber const& rhs)
  {
    if (rhs.digits_.is_zero()) {
      return;
    }
    if (digits_.is_zero()) {
      *this = rhs;
      return;
    }

    Details::BasicDigits<Traits> rhs_digits;
    NumType scale_diff;
    Sign rhs_sign;
    if (scale_ >= rhs.scale_) {
      rhs_digits = rhs.digits_;
      rhs_sign = rhs.sign_;
      scale_diff = scale_ - rhs.scale_;
    }
    else {
      rhs_digits = digits_;
      rhs_sign = sign_;
      scale_diff = rhs.scale_ - scale_;
      *this = rhs;
    }

    if (sign_ == rhs_sign) {
      digits_.add(rhs_digits, scale_diff);
    }
    else {
      if (digits_.sub(rhs_digits, scale_diff)) {
        negate();
      }
    }
  }

  /** \brief  Subtract \a rhs from *this.
   *
   * Scale on exit is max of rhs scale and *this scale.
   */
  void sub(BasicNumber const& rhs)
  {
    if (rhs.digits_.is_zero()) {
      return;
    }
    if (digits_.is_zero()) {
      *this = rhs;
      negate();
      return;
    }

    if (scale_ < rhs.scale_) {
      negate();
      add(rhs);
      negate();
      return;
    }
    else if (sign_ == rhs.sign_) {
      if (digits_.sub(rhs.digits_, scale_ - rhs.scale_)) {
        negate();
      }
    }
    else {
      digits_.add(rhs.digits_, scale_ - rhs.scale_);
    }
  }

  void power(BasicNumber const& rhs, NumType target_scale)
  {
    auto [power_whole, power_frac] = rhs.digits_.split_frac(rhs.scale());

    if (!power_frac.is_zero()) {
      std::ostringstream ss;
      output(ss, 10);
      Details::error(Msg::raising_to_fractional_power, ss.str());
      return;
    }

    auto one = BasicNumber(1);
    if (power_whole.is_zero()) {
      /* x ^ 0 = 1.  Scale = min (scale(x) * 0, max(target_scale, scale(x))) = 0.  */
      *this = one;
      return;
    }

    if (rhs.sign_ == Sign::positive &&
        power_whole.compare(one.digits_, 0) == Details::ComparisonResult::equality) {
      /* x ^ 1.  Scale = min (scale(x) * 1, max(target_scale, scale(x))) = scale(x).  */
      return;
    }

    if (rhs.sign_ == Sign::positive) {
      /* x ^ +p.  Scale = min(scale(x) * p, max(target_scale, scale(x)))
       *                = min(scale(x) * p, target_scale). (p > 1)
       */
      digits_.power_mul(power_whole);
      /* digits_ now has scale: scale_ * power_whole.  We want to rescale to the target_scale if
       * that is less than the new scale.  But we have to cope with the fact that the new scale
       * may be larger than we can publicly cope with.  */
      power_whole.mac(scale_, 0);
      auto base = BasicNumber(base_);
      if (power_whole.compare(base.digits_, 0) == Details::ComparisonResult::less_than) {
        /* If power_whole is <= base_ then we may already be at the scale we want.  Otherwise we
         * definitely will need to scale down (as target_scale < base_ < power_whole).
         */
        target_scale = std::min(power_whole.to_unsigned(0), target_scale);
      }

      auto base_plus_target_scale = BasicNumber(target_scale);
      base_plus_target_scale.add(base_);
      while (power_whole.compare(base_plus_target_scale.digits_, 0) !=
             Details::ComparisonResult::less_than) {
        /* Scale is significantly larger than we can deal with - scale down by base_ until we get
         * to the level we want.  Expect this to only be hit if using very small data types as
         * having a scale > 1_000_000_000 is likely to mean we've run out of memory elsewhere.
         */
        digits_.div_pow10(base_);
        power_whole.sub(base.digits_, 0);
      }
      power_whole.sub(Details::BasicDigits<Traits>(target_scale), 0);
      digits_.div_pow10(power_whole.to_unsigned(0));
      scale_ = target_scale;
      return;
    }

    assert(rhs.sign_ == Sign::negative);
    /* x ^ -p.  Scale = target_scale.  */
    digits_.power_mul(power_whole);
    power_whole.mac(scale_, 0);
    /* digits_ now has scale: scale_ * power_whole.  */
    /* We need to do 1.0/digits_ with resultant scale target_scale.
     * Scale digits_ to 2 * target_scale, and 1 to target_scale and then do the division.
     */
    auto target_scale_num = Details::BasicDigits<Traits>(target_scale);

    if (power_whole.compare(target_scale_num, 0) != Details::ComparisonResult::greater_than) {
      scale_ = power_whole.to_unsigned(0);
    }
    else {
      Details::BasicDigits<Traits> base(base_);
      auto target_scale_base = Details::BasicDigits<Traits>(base_);
      target_scale_base.add(target_scale_num, 0);
      while (power_whole.compare(target_scale_base, 0) != Details::ComparisonResult::less_than) {
        digits_.div_pow10(base_);
        power_whole.sub(base, 0);
      }

      if (power_whole.compare(target_scale_num, 0) == Details::ComparisonResult::greater_than) {
        power_whole.sub(target_scale_num, 0);
        digits_.div_pow10(power_whole.to_unsigned(0));
      }
      scale_ = target_scale;
    }

    one.divide(*this, target_scale);
    std::swap(one, *this);
  }

  void multiply(BasicNumber const& rhs, NumType target_scale)
  {
    NumType result_scale = scale() + rhs.scale();
    scale_ = std::min(result_scale, std::max({scale(), rhs.scale(), target_scale}));
    NumType rescale = result_scale - scale_;
    digits_.multiply(rhs.digits_, rescale);
    sign_ = sign_ == rhs.sign_ ? Sign::positive : Sign::negative;
  }

  void divide(BasicNumber rhs, NumType target_scale)
  {
    if (rhs.is_zero()) {
      Details::error(Msg::divide_by_zero);
      return;
    }

    /* We want both sides to have the same scale.  */
    if (scale() > rhs.scale()) {
      rhs.digits_.mul_pow10(scale() - rhs.scale());
    }
    else if (scale() < rhs.scale()) {
      digits_.mul_pow10(rhs.scale() - scale());
    }

    digits_.mul_pow10(target_scale);
    auto comparison = digits_.compare(rhs.digits_, 0);
    switch (comparison) {
    case Details::ComparisonResult::less_than:
      digits_.reset();
      break;
    case Details::ComparisonResult::equality:
      digits_ = Details::BasicDigits<Traits>(1);
      break;
    case Details::ComparisonResult::greater_than:
      digits_.divide(rhs.digits_);
      break;
    }

    scale_ = target_scale;
    sign_ = sign_ == rhs.sign_ ? Sign::positive : Sign::negative;
  }

  void modulo(BasicNumber rhs, NumType target_scale)
  {
    BasicNumber u(*this);
    u.divide(rhs, target_scale);
    u.multiply(rhs, target_scale);
    sub(u);
  }

  /** \brief  Are we equal to zero?
   *  \return True iff equal to zero.
   */
  bool is_zero() const { return digits_.is_zero(); }

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
    bool rhs_zero = rhs.digits_.is_zero();

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
