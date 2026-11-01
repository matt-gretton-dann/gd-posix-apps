/** \file   include/util/number.hh
 *  \brief  Arbitrary precision number
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef SRC_INCLUDE_UTIL_NUMBER_HH_INCLUDED
#define SRC_INCLUDE_UTIL_NUMBER_HH_INCLUDED

#include "gd/format.hh"
#include "gd/string.h"

#include <algorithm>
#include <array>
#include <cstdint>
#include <iomanip>
#include <iterator>
#include <limits>
#include <memory>
#include <ostream>
#include <span>
#include <sstream>
#include <string_view>
#include <utility>
#include <vector>

#include "bc.hh"
#include "digit-storage.hh"
#include "limb-product.hh"

namespace GD::Bc {

/** \brief  calculate 10 ^ \a pow. */
template<typename NumType>
constexpr auto pow10(NumType pow) -> NumType
{
  NumType r = 1;
  while (pow-- > 0) {
    r *= 10;  // NOLINT - 10 is obvious what we're doing here.
  }
  return r;
}

/** \brief       NumberTraits base class
 *  \tparam NT   Number type.
 *  \tparam WT   Wide type - should be twice the size of NT.
 *  \tparam bl10 Number of base10 digits to use (default: std::numeric_limits<NT>::digits10).
 *
 * This provides all the types and constants needed to implement the NumberTraits class as required
 * by BasicNumber<>.
 */
template<typename NT, typename WT, unsigned bl10 = std::numeric_limits<NT>::digits10>
struct NumberTraitsBase
{
  using NumType = NT;
  using WideType = WT;
  using PrintType = std::conditional_t<sizeof(NT) == 1, unsigned, NT>;
  static constexpr WideType base_ = static_cast<WideType>(pow10(bl10));
  static constexpr unsigned base_log10_ = bl10;

  /* Check types are wide enough.  */
  static_assert(std::numeric_limits<NT>::digits10 >= bl10);
  /* Wide type should be wide enough to hold twice the number of digits as NT.  */
  static_assert(std::numeric_limits<WT>::digits10 >= bl10 * 2);
};

/** Number traits with underlying 32-bit Number type.  */
using NumberTraits32 = NumberTraitsBase<::uint32_t, ::uint64_t>;

/** Number traits with 16-bit limbs and 32-bit widened arithmetic.  */
using NumberTraits16 = NumberTraitsBase<::uint16_t, ::uint32_t>;

/** Base-100 traits with 8-bit limbs and 16-bit widened arithmetic.
 *
 * The small radix makes exhaustive generic-arithmetic tests practical with
 * independently checked native-integer expectations.
 */
using NumberTraits8 = NumberTraitsBase<::uint8_t, ::uint16_t>;

namespace Details {

/** \brief       Report an error and exit with exit code 1.
 *  \param  msg  Message ID
 *  \param  args Arguments for the message.
 */
template<typename... Ts>
[[noreturn]] void error(Msg msg, Ts... args)
{
  std::cerr << Messages::get().format(Set::bc, msg, args...) << '\n';
  std::exit(1);  // NOLINT(concurrency-mt-unsafe)
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
  static constexpr unsigned line_length = 69;                   ///< Length of a line in output.

  /** \brief  Basic constructor.  */
  BasicDigits() : digits_(nullptr) {}

  /** \brief   Construct holding the single digit \a num
   *  \param num Number to hold.  Must be less than \c base_.
   */
  explicit BasicDigits(NumType num) : digits_(DigitVector())
  {
    while (num != 0) {
      digits_->push_back(static_cast<NumType>(num % base_));
      num /= static_cast<NumType>(base_);
    }
  }

  ~BasicDigits() = default;
#if GD_BC_PROFILE
  BasicDigits(BasicDigits const& other) noexcept : digits_(other.digits_) { record_copy(); }
  auto operator=(BasicDigits const& other) noexcept -> BasicDigits&
  {
    digits_ = other.digits_;
    record_copy();
    return *this;
  }
  BasicDigits(BasicDigits&& other) noexcept : digits_(std::move(other.digits_))
  {
    GD_BC_PROFILE_EVENT(Profile::Event::digit_moves, 1);
  }
  auto operator=(BasicDigits&& other) noexcept -> BasicDigits&
  {
    digits_ = std::move(other.digits_);
    GD_BC_PROFILE_EVENT(Profile::Event::digit_moves, 1);
    return *this;
  }
#else
  BasicDigits(BasicDigits const&) = default;
  auto operator=(BasicDigits const&) -> BasicDigits& = default;
  BasicDigits(BasicDigits&&) noexcept = default;
  auto operator=(BasicDigits&&) noexcept -> BasicDigits& = default;
#endif

  /** \brief  Reset the digits - sets us to zero. */
  void reset() { digits_.reset(); }

  /** \brief Is this zero?
   * Storage is not universally normalized: range copies and fractional splits
   * retain zero padding, and mac(0, 0) can leave nonempty zero storage. Do not
   * replace this numeric check with an empty-container or final-limb test.
   */
  [[nodiscard]] auto is_zero() const -> bool
  {
    /* We're zero if we have no digits_ vector or all the digits in it are zero.  */
    if (!digits_) {
      return true;
    }

    return std::all_of(digits_->begin(), digits_->end(), [](auto d) { return d == 0; });
  }

  /** Is this an even number? Both absent and empty storage represent zero. */
  [[nodiscard]] auto is_even() const -> bool
  {
    if (!digits_ || digits_->empty()) {
      return true;
    }

    return ((*digits_)[0] & 1) == 0;
  }

  [[nodiscard]] auto compare(BasicDigits const& rhs, NumType scale) const
    -> Details::ComparisonResult
  {
    assert(digits_);      // NOLINT
    assert(rhs.digits_);  // NOLINT
    if (scale == 0) {
      // Inspect the most significant unequal limb first. Zero-extension keeps
      // this valid for temporary ranges that still contain leading zero limbs;
      // storage length alone does not establish numeric ordering.
      // Neither operand changes during comparison. Select inline/heap storage
      // and capture both extents once rather than rechecking each limb access.
      auto const lhs_size = digits_->size();
      auto const rhs_size = rhs.digits_->size();
      auto const lhs_first = digits_->begin();
      auto const rhs_first = rhs.digits_->begin();
      auto index = std::max(lhs_size, rhs_size);
      while (index != 0) {
        --index;
        NumType const lhs_digit = index < lhs_size ? lhs_first[index] : 0;
        NumType const rhs_digit = index < rhs_size ? rhs_first[index] : 0;
        if (lhs_digit < rhs_digit) {
          return Details::ComparisonResult::less_than;
        }
        if (lhs_digit > rhs_digit) {
          return Details::ComparisonResult::greater_than;
        }
      }
      return Details::ComparisonResult::equality;
    }
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

  static constexpr NumType to_unsigned_error_too_large_ = base_;  /// Error code for to_unsigned()
  static constexpr NumType to_unsigned_error_fractional_ =
    base_ + 1;  /// Error code for to_unsigned()

  /** \brief        Convert to a value in [0, base_), or a reserved error code.
   *  \param scale   Decimal scale of the coefficient.
   *  \param truncate_fraction Discard the fraction instead of rejecting it.
   *
   * Strict conversion rejects any nonzero fractional digit. Both policies
   * reject an integer part at or above base_; base_ and base_+1 are reserved
   * for the too-large and fractional error codes respectively.
   */
  [[nodiscard]] auto to_unsigned(NumType scale, bool truncate_fraction = false) const -> NumType
  {
    if (!digits_) {
      return 0;
    }
    auto it = digits_->begin();
    while (scale >= base_log10_) {
      if (it == digits_->end()) {
        return 0;
      }
      if (!truncate_fraction && *it != 0) {
        return to_unsigned_error_fractional_;
      }
      ++it;
      scale -= base_log10_;
    }
    if (it == digits_->end()) {
      return 0;
    }

    auto const pow10_scale = pow10(scale);
    if (!truncate_fraction && *it % pow10_scale != 0) {
      return to_unsigned_error_fractional_;
    }
    NumType result = *it++ / pow10_scale;
    if (it == digits_->end()) {
      return result;
    }
    // The next limb contributes at weight base_/pow10_scale. A remaining
    // whole part there makes the value at least base_, outside this domain.
    if (*it >= pow10_scale) {
      return to_unsigned_error_too_large_;
    }
    result = static_cast<NumType>(result + *it++ * (base_ / pow10_scale));
    // Public operations can leave leading zero limbs without normalization.
    while (it != digits_->end()) {
      if (*it++ != 0) {
        return to_unsigned_error_too_large_;
      }
    }
    return result;
  }

  /** \brief          Output to stream
   *  \param os       Stream to output to
   *  \param obase    Output base
   *  \param scale    Output scale.
   *  \param line_pos Current position on the line
   *
   * Use output_base10() if output is base-10, as that uses significantly less memory to do the
   * output.
   *
   * Assumes that zero output has already been handled.
   */
  void output(std::ostream& os, NumType obase, NumType scale, unsigned line_pos) const
  {
    assert(!is_zero());  // NOLINT
    assert(obase >= 2);  // NOLINT
    if (line_pos >= line_length) {
      os << '\n';
      line_pos = 0;
    }
    auto [whole, frac] = split_frac(scale);
    DigitVector whole_digits;
    while (!whole.is_zero()) {
      whole_digits.push_back(whole.divide(obase));
    }
    std::string number;
    for (auto it = whole_digits.rbegin(); it != whole_digits.rend(); ++it) {
      number += to_string(*it, obase);
    }

    if (!extensions_enabled() && number.empty()) {
      // In extensions mode we don't add a zero before the radix point.
      number = "0";
    }

    if (scale != 0) {
      number += '.';
      BasicDigits s(1);
      s.mul_pow10(scale);
      s.sub(NumType{1}, 0);
      number += to_string(frac.mul_mod_pow10(obase, scale), obase, false);
      s.divide(obase);
      while (!s.is_zero()) {
        number += to_string(frac.mul_mod_pow10(obase, scale), obase);
        s.divide(obase);
      }
    }

    std::string::size_type p = 0;
    // Ensure we handle line breaks appropriately.  Note that line_length includes the \\.
    if (number.length() > line_length - line_pos) {
      // Compare the remaining output with the reserved chunk capacity, avoiding
      // unsigned subtraction from the full length. A zero-sized first chunk
      // still advances layout by starting a new line.
      while (number.length() - p > line_length - line_pos - 1) {
        os << number.substr(p, line_length - line_pos - 1) << "\\\n";
        p += line_length - line_pos - 1;
        line_pos = 0;
      }
    }
    if (p != number.length()) {
      os << number.substr(p);
    }
  }

  /** \brief                Output number in base 10
   *  \param os             Stream to output to
   *  \param scale          Scale of number.
   *  \param digits_printed Number of digits printed so far.
   *
   * This is slightly more efficient than output(os, 10, scale) would be in terms of memory usage.
   */
  void output_base10(std::ostream& os, NumType scale, unsigned digits_printed) const
  {
    /* For base 10 we can rely on the underlying system-library's output routines the only corner
     * cases are to: 1) Ensure we pad with zeroes where necessary; and 2) Insert the decimal point
     * in the correct place.
     */
    assert(!is_zero());  // NOLINT
    if (digits_printed >= line_length) {
      os << '\n';
      digits_printed = 0;
    }

    /* Generate string without radix point. Note we reverse through the digits as we want to print
     * big-endian, but numbers are stored little-endian.
     */
    std::ostringstream ss;
    int width = 0;
    for (auto rit = digits_->rbegin(); rit != digits_->rend(); ++rit) {
      ss << std::setfill('0') << std::setw(width) << static_cast<typename Traits::PrintType>(*rit);
      width = static_cast<int>(base_log10_);
    }
    std::string result = ss.str();
    unsigned const break_at = (result.length() <= line_length) ? line_length : (line_length - 1);
    // A long coefficient reserves an extra column before its first chunk.
    // Continue the current number if its sign has already filled that space.
    if (digits_printed >= break_at) {
      os << "\\\n";
      digits_printed = 0;
    }
    auto it = result.begin();
    bool print = false;
    while (result.end() - it > scale) {
      if (*it != '0') {
        print = true;
      }
      if (print) {
        os << *it;
        if (++digits_printed == break_at) {
          digits_printed = 0;
          os << "\\\n";
        }
      }
      ++it;
    }

    if (!extensions_enabled() && !print) {
      os << '0';
      if (++digits_printed == break_at) {
        os << "\\\n";
        digits_printed = 0;
      }
    }

    if (it != result.end()) {
      os << '.';
      ++digits_printed;
      if (digits_printed > break_at) {
        os << "\\\n";
        digits_printed = 0;
      }

      assert(scale >= result.end() - it);  // NOLINT
      auto padding = std::size_t(scale) - static_cast<std::size_t>(result.end() - it);
      if (padding != 0) {
        // Leading fractional zeros use the same continuation capacity as real
        // coefficient digits. Keep the buffer bounded independently of scale.
        std::array<char, line_length> zeroes;
        zeroes.fill('0');
        while (padding != 0) {
          if (digits_printed >= break_at) {
            os << "\\\n";
            digits_printed = 0;
          }
          auto const count = std::min(padding, std::size_t(break_at - digits_printed));
          os.write(zeroes.data(), static_cast<std::streamsize>(count));
          padding -= count;
          scale = static_cast<NumType>(scale - count);
          digits_printed += static_cast<unsigned>(count);
        }
      }
      while (result.end() - it > break_at - digits_printed) {
        os << result.substr(it - result.begin(), break_at - digits_printed) << "\\\n";
        // The loop and the scale invariant above bound the chunk by scale.
        scale = static_cast<NumType>(scale - (break_at - digits_printed));
        it += break_at - digits_printed;
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
    os << '{';
    char const* sep = "";
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
  [[nodiscard]] auto length() const -> NumType
  {
    if (!digits_) {
      return 1;
    }

    /* The length is at most the number of entries in the digits_ vector times the log base.  We
     * then correct by reducing length by the number of leading zeros.
     */
    auto length = digits_->size() * base_log10_;
    auto rit = digits_->rbegin();
    auto const finish = digits_->rend();
    while (rit != finish && *rit == 0) {
      ++rit;
      length -= base_log10_;
    }
    if (rit != finish) {
      length -= base_log10_;
      NumType d = *rit;
      assert(d != 0);  // NOLINT
      while (d != 0) {
        length += 1;
        d /= 10;  // NOLINT - 10 is not a magic number
      }
    }

    assert(length < base_);  // NOLINT

    return std::max(NumType{1}, static_cast<NumType>(length));
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

    NumType const carry =
      for_each(rhs, scale, [](typename DigitVector::iterator it, WideType carry) {
        WideType const result = *it + carry;
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
  auto sub(NumType s, NumType scale) -> bool  // NOLINT
  {
    BasicDigits const sub_d(s);
    return sub(sub_d, scale);
  }

  /** \brief        Do \c *this = abs(\a rhs * 10 ^ \a scale), return true if sign flipped.
   *  \param  rhs   Right hand side
   *  \param  scale Number of digits to scale \a rhs by.
   *  \return       True if *this was less than rhs
   *
   * If *this (on entry) is less than sub * 10 ^ scale we return true.  Otherwise we return false.
   */
  auto sub(BasicDigits const& rhs, NumType scale) -> bool
  {
    copy_on_write();

    NumType const carry =
      for_each(rhs, scale, [](typename DigitVector::iterator it, WideType carry) {
        WideType const rhs_value = carry % base_;
        carry /= base_;
        if (rhs_value > *it) {
          *it += base_;
          ++carry;
        }
        *it -= static_cast<NumType>(rhs_value);
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
    BasicDigits<Traits> const one(1);
    while (!power.is_zero()) {
      if (((*power.digits_)[0] & 1) != 0) {
        result.multiply(*this, 0);
        power.sub(one, 0);
      }
      power.divide(2);
      // The remaining bits consume the next squared base. Once the exponent
      // is exhausted that square is unused, including in reciprocal powers.
      if (!power.is_zero()) {
        multiply(*this, 0);
      }
    }

    std::swap(digits_, result.digits_);
  }

  static void multiply_split_point(NumType point) { multiply_split_point_ = point; }

  void multiply(BasicDigits const& rhs, NumType rescale)
  {
    /* Multiplying by zero is easy.  */
    if (is_zero() || rhs.is_zero()) {
      digits_.reset();
      return;
    }

    BasicDigits result =
      multiply(digits_->begin(), digits_->end(), rhs.digits_->begin(), rhs.digits_->end());
    std::swap(*this, result);
    if (rescale != 0) {
      div_pow10(rescale);
    }
    else {
      // A zero decimal shift only needs to trim the product's spare limbs.
      tidy();
    }
  }

  /** \brief      Multiply-accumulate of over the underlying digits.
   *  \param  mul Number to multiply by
   *  \param  acc Number to add.
   *
   * Does `digits_ * mul + acc`.  Treating `digits_` as an integer.
   */
  void mac(NumType mul, NumType acc)  // NOLINT
  {
    copy_on_write();

    WideType carry = acc;

    // Storage cannot grow until the carry-draining phase below. A range keeps
    // inline/heap selection outside the per-limb arithmetic loop.
    for (auto& digit : *digits_) {
      WideType dw = digit;
      dw = dw * mul + carry;
      carry = dw / base_;
      digit = static_cast<NumType>(dw % base_);
    }

    // The accumulator may be a full native value, including the radix itself
    // in BasicNumber's constructor. Drain every carry limb canonically.
    while (carry != 0) {
      digits_->push_back(static_cast<NumType>(carry % base_));
      carry /= base_;
    }
  }

  /** Multiply by 10^ \a scale. */
  void mul_pow10(NumType scale)
  {
    copy_on_write();

    /* Do the whole digit steps first as this is just memory shuffling.  */
    NumType const extra_digits = scale / base_log10_;
    auto const init_size = digits_->size();
    digits_->resize(init_size + extra_digits);
    if (extra_digits != 0) {
      auto const first = digits_->begin();
      std::move_backward(first, first + init_size, first + init_size + extra_digits);
      std::fill_n(first, extra_digits, NumType{0});
    }

    /* Now we have to multply by a number < base so can just use mac.  */
    auto const fractional_shift = scale % base_log10_;
    if (fractional_shift != 0) {
      mac(static_cast<NumType>(pow10(fractional_shift)), 0);
    }

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
  auto mul_mod_pow10(NumType mul, NumType scale) -> NumType  // NOLINT
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

    WideType const scale_pow10 = pow10(scale);
    WideType result = *it / scale_pow10;
    *it++ -= static_cast<NumType>(result * scale_pow10);

    if (it != digits_->end()) {
      result += *it++ * (base_ / scale_pow10);
      assert(it == digits_->end());  // NOLINT
      assert(result < base_);        // NOLINT
      digits_->pop_back();
    }

    tidy();
    return static_cast<NumType>(result);
  }

  /** Replace the coefficient with its integer quotient by v. */
#if defined(_MSC_VER)
  // Isolate the large Algorithm D kernel from MSVC caller inlining decisions.
  // Native #203 runs found a Windows division regression after VM/kernel
  // integration. Other compilers retain their ordinary inlining policy.
  __declspec(noinline)
#endif
  void divide(BasicDigits v)
  {
    /* Algorithm D of Knuth TAOCP 4.3.1.  */

    /* D1. Normalise.  */
    /* u =*this; */
    copy_on_write();
    tidy();
    v.copy_on_write();
    v.tidy();
    assert(digits_->size() >= v.digits_->size());  // NOLINT

    if (v.digits_->size() == 1) {
      divide(v.digits_->at(0));
      return;
    }

    if (digits_->size() == 2) {
      assert(v.digits_->size() == 2);  // NOLINT
      WideType const u2 = (*digits_)[0] + (base_ * (*digits_)[1]);
      WideType const v2 = (*v.digits_)[0] + (base_ * (*v.digits_)[1]);
      WideType const q = u2 / v2;

      digits_ = DigitVector(std::initializer_list<NumType>{static_cast<NumType>(q % base_),
                                                           static_cast<NumType>(q / base_)});
      return;
    }

    /* D1. Normalize.  */
    /* Note this is not the number Knuth picks as that one causes me problems.  This choice still
     * guarantees that d > b/2, but that we don't extend the v vector at all.
     */
    auto const n = v.digits_->size();
    auto const m = digits_->size() - n;

    NumType d = (static_cast<NumType>(base_) / 2 - 1) / v.digits_->back();
    d += 1;
    if (d != 1) {
      mac(d, 0);
      v.mac(d, 0);
    }
    assert(n == v.digits_->size());          // NOLINT
    assert(v.digits_->back() >= base_ / 2);  // NOLINT
    digits_->push_back(0);

    BasicDigits q;
    q.digits_ = DigitVector(m + 1, static_cast<NumType>(0));

    // D3-D6 change limb values but never resize these buffers. Resolve the
    // storage modes once, after normalization/growth and before the final swap.
    // The established extents bound every index in the quotient-digit loop.
    auto dividend = std::span(digits_->begin(), digits_->size());
    auto divisor = std::span<NumType const>(v.digits_->begin(), n);
    auto quotient = std::span(q.digits_->begin(), q.digits_->size());
    assert(n >= 2 && n + m < dividend.size());  // NOLINT
    assert(quotient.size() == m + 1);           // NOLINT

    /* D2. Initialize j.  */
    auto j = m;

    do {
      /* D3. Calculate q_hat. */
      WideType const t = dividend[n + j] * base_ + dividend[n + j - 1];
      WideType q_hat = t / divisor[n - 1];
      WideType r_hat = t % divisor[n - 1];

      bool cont = true;

      while (cont) {
        cont = false;
        WideType const q_hat_v_n2 = q_hat * divisor[n - 2];
        WideType const b_r_hat_u_j_n2 = base_ * r_hat + dividend[j + n - 2];
        if (q_hat == base_ || q_hat_v_n2 > b_r_hat_u_j_n2) {
          --q_hat;
          r_hat += divisor[n - 1];
          if (r_hat < base_) {
            cont = true;
          }
        }
      }

      /* D4. Multiply and subtract.   */
      WideType carry = 0;
      for (unsigned i = 0; i < n; ++i) {
        carry += divisor[i] * q_hat;
        NumType const rhs_d = carry % base_;
        carry /= base_;
        if (rhs_d > dividend[j + i]) {
          dividend[j + i] += base_;
          carry += 1;
        }
        dividend[j + i] -= rhs_d;
      }
      if (carry != 0) {
        NumType const rhs_d = carry % base_;
        carry /= base_;
        if (rhs_d > dividend[j + n]) {
          dividend[j + n] += base_;
          carry += 1;
        }
        dividend[j + n] -= rhs_d;
      }
      bool const borrowed = (carry != 0);

      if (borrowed) {
        /* D6. Add back.  Do this now as it is simpler. */
        carry = 0;
        for (typename DigitVector::size_type i = 0; i < n; ++i) {
          carry += divisor[i] + dividend[j + i];
          dividend[j + i] = carry % base_;
          carry /= base_;
        }
        carry += dividend[j + n];
        dividend[j + n] = carry % base_;
        carry /= base_;
        assert(carry == 1);  // NOLINT
        q_hat -= 1;
      }

      /* D5. Test remainder.  */
      assert(q_hat < base_);         // NOLINT
      assert(dividend[n + j] == 0);  // NOLINT
      quotient[j] = static_cast<NumType>(q_hat);

    } while (j-- > 0);

    /* D7. Unormalise.  */
    std::swap(q.digits_, digits_);
  }

  /** \brief  Divide by the number \a div.  Returns remainder.  */
  auto divide(NumType div) -> NumType
  {
    copy_on_write();

    WideType carry = 0;
    auto const finish = digits_->rend();
    for (auto d = digits_->rbegin(); d != finish; ++d) {
      carry += *d;
      assert(carry / div < base_);  // NOLINT
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
    decltype(init_size) offset = scale / base_log10_;
    if (offset > init_size) {
      offset = init_size;
    }
    if (offset != 0) {
      auto const first = digits_->begin();
      std::move(first + offset, first + init_size, first);
    }
    digits_->resize(init_size - offset);
    if (digits_->empty()) {
      return;
    }

    auto const scale_pow10 = static_cast<WideType>(pow10(scale % base_log10_));
    auto it = digits_->begin();
    auto const finish = digits_->end();
    WideType carry = *it / scale_pow10;
    while (++it != finish) {
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
  [[nodiscard]] auto split_frac(NumType scale) const -> std::pair<BasicDigits, BasicDigits>
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

    WideType const scale_pow10 = pow10(scale);

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
#if GD_BC_PROFILE
  void record_copy() const noexcept
  {
    GD_BC_PROFILE_EVENT(Profile::Event::digit_copies, 1);
    if (digits_.has_heap()) {
      GD_BC_PROFILE_EVENT(Profile::Event::owned_digit_copies, 1);
    }
  }
#endif
  using DigitVector = DigitStorage<NumType>;

  /** Construct basic digits based on an iterator range [begin, end).  */
  BasicDigits(typename DigitVector::const_iterator begin, typename DigitVector::const_iterator end)
      : digits_(begin, end)
  {
    GD_BC_PROFILE_EVENT(Profile::Event::range_limb_bytes,
                        static_cast<std::uint64_t>(std::distance(begin, end)) * sizeof(NumType));
  }

  /** \brief  Ensure digits_ is populated in a way that is modifiable.
   */
  void copy_on_write()
  {
    // A single ownership query distinguishes absent, independent inline and
    // shared heap values. Zero owners means absent at public boundaries.
    auto const owners = digits_.use_count();
    if (owners == 0) {
      digits_ = DigitVector();
    }
    else if (owners > 1) {
      GD_BC_PROFILE_EVENT(Profile::Event::cow_clones, 1);
      GD_BC_PROFILE_EVENT(Profile::Event::cow_limb_bytes, digits_->size() * sizeof(NumType));
      digits_ = digits_.clone();
    }
  }

  /** \brief        Remove most-significant zero limbs, retaining the owner.
   * This is an explicit operation, not a class-wide representation invariant.
   * Fractional splits deliberately retain padding until their consumer acts.
   */
  void tidy()
  {
    if (digits_) {
      auto const first = digits_->begin();
      auto const size = digits_->size();
      auto remaining = size;
      while (remaining != 0 && first[remaining - 1] == 0) {
        --remaining;
      }
      if (remaining != size) {
        digits_->resize(remaining);
      }
    }
  }

  /** \brief       Convert \a num to a string in base \a obase.
   *  \param num   Number to convert, must be less than \a obase.
   *  \param obase Base for output
   *  \param lspace Do we need leading space (default true)
   *  \return      String representation of number.
   */
  static auto to_string(NumType num, NumType obase, bool lspace = true) -> std::string  // NOLINT
  {
    assert(num < obase);  // NOLINT
    constexpr std::string_view nums = "0123456789ABCDEF";
    if (obase <= nums.size()) {
      return std::string(1, nums[num]);
    }
    auto const obase_width = std::to_string(obase - 1).size();
    std::string const str = std::to_string(num);
    if (lspace) {
      return std::string(" ") + std::string(obase_width - str.size(), '0') + str;
    }
    return std::string(obase_width - str.size(), '0') + str;
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
    assert(digits_ && rhs.digits_);  // NOLINT

    auto it_lhs = digits_->begin();
    auto const finish = digits_->end();

    while (scale >= base_log10_) {
      NumType const d_lhs = (it_lhs == finish) ? 0 : *it_lhs++;
      fn(d_lhs, 0);
      scale -= base_log10_;
    }

    WideType const pow10_scale = pow10(scale);
    WideType carry = 0;
    for (auto it_rhs : *rhs.digits_) {
      carry += it_rhs * pow10_scale;
      auto const d_rhs = static_cast<NumType>(carry % base_);
      carry /= base_;

      NumType const d_lhs = (it_lhs == finish) ? 0 : *it_lhs++;
      assert(carry < base_);  // NOLINT
      fn(d_lhs, d_rhs);
    }

    if (carry != 0) {
      NumType const d_lhs = (it_lhs == finish) ? 0 : *it_lhs++;
      fn(d_lhs, static_cast<NumType>(carry));
    }

    while (it_lhs != finish) {
      fn(*it_lhs++, 0);
    }
  }

  static auto ensure_it_valid(typename DigitVector::iterator it, DigitVector& digits) ->
    typename DigitVector::iterator
  {
    // The calling BasicDigits owns this container throughout the check. Vector
    // growth can move its elements, but does not change the container's owner.
    if (it == digits.end()) {
      digits.push_back(0);
      it = digits.end() - 1;
    }
    return it;
  }

  /** \brief       Iterate over the digits of \c *this & \a rhs calling \a fn.
   *  \param rhs   Right hand side set of digits to iterate over.
   *  \param scale Scale differnce between \c *this & \a rhs.
   *  \param fn    Policy Fn(iterator lhs, WideType rhs_and_carry) -> WideType.
   *  It writes the current digit and returns carry/borrow. A zero contribution
   *  must preserve that digit and return zero, as both add/subtract policies do.
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
  auto for_each(BasicDigits const& rhs, NumType scale, Fn fn, NumType initial_carry = 0) -> NumType
  {
    assert(digits_ && rhs.digits_);  // NOLINT

    if (scale == 0) {
      // Capture the source extent before growing the destination. Self-aliases
      // already have equal extents; distinct shared values were detached by COW.
      auto const rhs_size = rhs.digits_->size();
      digits_->resize(std::max(digits_->size(), rhs_size), NumType{0});
      auto it = digits_->begin();
      auto const finish = digits_->end();
      auto const source = rhs.digits_->begin();
      WideType carry = initial_carry;
      for (typename DigitVector::size_type index = 0; index < rhs_size; ++index) {
        carry = static_cast<WideType>(fn(it++, carry + source[index]));
        assert(carry < base_);  // NOLINT
      }
      // Both mutable policies are identities for a zero contribution. Higher
      // destination limbs therefore need no work after carry/borrow is clear.
      while (carry != 0 && it != finish) {
        carry = static_cast<WideType>(fn(it++, carry));
        assert(carry < base_);  // NOLINT
      }
      assert(carry < base_);  // NOLINT
      return static_cast<NumType>(carry);
    }

    auto it = digits_->begin();
    WideType carry = initial_carry;

    while (scale >= base_log10_) {
      it = ensure_it_valid(it, *digits_);
      carry = static_cast<WideType>(fn(it++, carry));
      assert(carry < base_);  // NOLINT
      scale -= base_log10_;
    }

    WideType const pow10_scale = pow10(scale);
    for (auto it_rhs : *rhs.digits_) {
      carry += it_rhs * pow10_scale;
      it = ensure_it_valid(it, *digits_);
      carry = static_cast<WideType>(fn(it++, carry));
      assert(carry < base_);  // NOLINT
    }

    auto const finish = digits_->end();
    while (it != finish) {
      carry = static_cast<WideType>(fn(it++, carry));
      assert(carry < base_);  // NOLINT
    }

    assert(carry < base_);  // NOLINT
    return static_cast<NumType>(carry);
  }

  static auto multiply(typename DigitVector::const_iterator lhs_begin,
                       typename DigitVector::const_iterator lhs_end,
                       typename DigitVector::const_iterator rhs_begin,
                       typename DigitVector::const_iterator rhs_end) -> BasicDigits
  {
    if (std::distance(lhs_begin, lhs_end) <= multiply_split_point_ ||
        std::distance(rhs_begin, rhs_end) <= multiply_split_point_) {
      if constexpr (LimbSquare<Traits>::supported) {
        // Keep the ordinary schoolbook body unchanged. Recognition belongs to
        // dispatch, before either coefficient kernel allocates its output.
        if (lhs_end - lhs_begin > 1 && std::to_address(lhs_begin) == std::to_address(rhs_begin) &&
            lhs_end - lhs_begin == rhs_end - rhs_begin) {
          return square_basic(lhs_begin, lhs_end);
        }
      }
      return multiply_basic(lhs_begin, lhs_end, rhs_begin, rhs_end);
    }
    return multiply_workspace(lhs_begin, lhs_end, rhs_begin, rhs_end);
  }

  static auto square_basic(typename DigitVector::const_iterator first,
                           typename DigitVector::const_iterator last) -> BasicDigits
  {
    auto const count = static_cast<std::size_t>(last - first);
    BasicDigits result;
    result.copy_on_write();
    result.digits_->resize(2 * count + 2, 0);
    LimbSquare<Traits>::multiply(
      {std::to_address(first), count},
      {std::to_address(result.digits_->begin()), result.digits_->size()});
    return result;
  }

  static auto multiply_basic(typename DigitVector::const_iterator lhs_begin,
                             typename DigitVector::const_iterator lhs_end,
                             typename DigitVector::const_iterator rhs_begin,
                             typename DigitVector::const_iterator rhs_end) -> BasicDigits
  {
    GD_BC_PROFILE_EVENT(Profile::Event::basic_multiply_calls, 1);
    /* Handle the case of nothing to multiply by - equivalent to multiplying by zero.  */
    if (lhs_begin == lhs_end || rhs_begin == rhs_end) {
      return BasicDigits();
    }

    if (lhs_end == lhs_begin + 1) {
      /* LHS is one digit use the Multiply accumulate function.  */
      BasicDigits r(rhs_begin, rhs_end);
      r.mac(*lhs_begin, 0);
      return r;
    }

    if (rhs_end == rhs_begin + 1) {
      /* RHS is one digit use the Multiply accumulate function.  */
      BasicDigits r(lhs_begin, lhs_end);
      r.mac(*rhs_begin, 0);
      return r;
    }

    // With unequal extents, fewer rows mean fewer carry-draining tails. The
    // one-limb MAC paths above stay unchanged; both ranges remain borrowed.
    if (lhs_end - lhs_begin > rhs_end - rhs_begin) {
      std::swap(lhs_begin, rhs_begin);
      std::swap(lhs_end, rhs_end);
    }

    /* Have to do the full thing... */
    BasicDigits result;
    result.copy_on_write();
    auto const result_size = (lhs_end - lhs_begin) + (rhs_end - rhs_begin) + 2;
    result.digits_->resize(result_size, 0);
    auto result_begin = result.digits_->begin();

    for (auto lhs_it = lhs_begin; lhs_it != lhs_end; ++lhs_it) {
      WideType carry = 0;
      auto result_it = result_begin++;
      assert(result_begin != result.digits_->end());  // NOLINT
      for (auto rhs_it = rhs_begin; rhs_it != rhs_end; ++rhs_it) {
        assert(result_it != result.digits_->end());  // NOLINT
        WideType const lhs_d = *lhs_it;
        WideType const rhs_d = *rhs_it;
        carry += lhs_d * rhs_d + *result_it;
        *result_it++ = static_cast<NumType>(carry % base_);
        carry /= base_;
      }

      while (carry != 0) {
        assert(result_it != result.digits_->end());  // NOLINT
        carry += *result_it;
        *result_it++ = static_cast<NumType>(carry % base_);
        carry /= base_;
      }
    }

    return result;
  }

  static auto multiply_workspace(typename DigitVector::const_iterator lhs_begin,
                                 typename DigitVector::const_iterator lhs_end,
                                 typename DigitVector::const_iterator rhs_begin,
                                 typename DigitVector::const_iterator rhs_end) -> BasicDigits
  {
    auto const lhs_size = static_cast<std::size_t>(lhs_end - lhs_begin);
    auto const rhs_size = static_cast<std::size_t>(rhs_end - rhs_begin);
    if (lhs_size > std::numeric_limits<std::size_t>::max() - rhs_size) {
      throw std::length_error("bc multiplication result too large");
    }
    auto const point = static_cast<std::size_t>(multiply_split_point_);
    using Kernel = LimbProduct<Traits>;
    // Allocate output and scratch once. Recursive products only borrow spans;
    // no owner escapes and no generic decimal rescaling is used to recombine.
    BasicDigits result;
    result.copy_on_write();
    result.digits_->resize(lhs_size + rhs_size, NumType{0});
    std::vector<NumType, Profile::Allocator<NumType, Profile::AllocationKind::digits>> workspace(
      Kernel::workspace_size(std::max(lhs_size, rhs_size), point));
    Kernel::multiply(std::span<NumType const>(lhs_begin, lhs_end),
                     std::span<NumType const>(rhs_begin, rhs_end),
                     std::span<NumType>(result.digits_->begin(), result.digits_->end()),
                     std::span<NumType>(workspace), point);
    result.tidy();
    return result;
  }

  // NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
  static typename DigitVector::difference_type multiply_split_point_;
  DigitVector digits_ = nullptr;  ///< BasicDigits in number.
};

template<typename Traits>
typename BasicDigits<Traits>::DigitVector::difference_type
  // NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
  BasicDigits<Traits>::multiply_split_point_ = 1;

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
 * Coefficients of up to three limbs are stored inline and copied by value.
 * Larger coefficients share heap storage until mutation detaches the value.
 * Shrinking preserves heap capacity; reset or a shared-small detach can return
 * to inline storage. Scale and sign remain independent of that storage choice.
 *
 * \brief Printing
 *
 * There is basic fmt::format support implemented, along with stream output.
 *
 * The format specifier has the following options:
 *
 *  {:d} - output in debug mode.
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
  ~BasicNumber() noexcept = default;
  BasicNumber(BasicNumber const&) = default;
  auto operator=(BasicNumber const&) -> BasicNumber& = default;
  BasicNumber(BasicNumber&&) noexcept = default;
  auto operator=(BasicNumber&&) noexcept -> BasicNumber& = default;

  /** \brief       Construct a number.
   *  \param s     String representing the number.
   *  \param ibase Input base in range [2, 16]
   *
   * Can only handle positive numbers.
   */
  BasicNumber(std::string_view s, NumType ibase) : digits_(), sign_(Sign::positive), scale_(0)
  {
    GD_BC_PROFILE_EVENT(Profile::Event::literal_conversions, 1);
    assert(ibase >= 2);   // NOLINT
    assert(ibase <= 16);  // NOLINT

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

      const auto* digit = ::strchr(digits, c);
      if (digit != nullptr) {
        assert((digit - digits < ibase) || (s.size() == 1 && digit - digits < 16));  // NOLINT
        digits_.mac(ibase, static_cast<NumType>(digit - digits));
      }
      else if (c == '.') {
        assert(!seen_period);  // NOLINT
        seen_period = true;
      }
      else {
        abort();
      }
    }

    /* We now have number as digits_ * ibase ^ scale_.  If we're in base 10 or the scale is zero
     * or the digits are zero we have finished.  */
    if (ibase == 10 || scale_ == 0 || digits_.is_zero()) {  // NOLINT - 10 isn't magic
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
  explicit BasicNumber(NumType value) : digits_(), sign_(Sign::positive), scale_(0)
  {
    digits_.mac(base_, value);
  }

  /** Convert a nonnegative bc value to its truncated integer part.
   * The current conversion domain is [0, base_); larger domains belong to
   * the separate size/scale work. Preserve the existing negative-input check.
   */
  [[nodiscard]] auto to_unsigned() const -> NumType
  {
    if (sign_ == Sign::negative) {
      Details::error(Msg::number_to_unsigned_failed_negative, *this);
    }
    // POSIX register assignments and array subscripts discard the fraction.
    NumType const result = digits_.to_unsigned(scale_, true);
    if (result == digits_.to_unsigned_error_too_large_) {
      Details::error(Msg::number_to_unsigned_failed_too_large, *this);
    }
    if (result == digits_.to_unsigned_error_fractional_) {
      Details::error(Msg::number_to_unsigned_failed_fractional, *this);
    }

    return result;
  }

  /** \brief          Output the value to a stream.
   *  \param os       Output stream
   *  \param obase    Base to output in
   *  \param line_pos Current position on output line (default = 0).
   */
  void output(std::ostream& os, NumType obase, unsigned line_pos) const
  {
    if (line_pos > digits_.line_length) {
      os << '\n';
      line_pos = 0;
    }

    if (digits_.is_zero()) {
      os << "0";
      return;
    }
    if (sign_ == Sign::negative) {
      if (line_pos > digits_.line_length - 1) {
        line_pos = 0;
        os << '\n';
      }
      os << '-';
      ++line_pos;
      if (line_pos > digits_.line_length - 1) {
        line_pos = 0;
        os << "\\\n";
      }
    }
    if (obase == 10) {  // NOLINT - 10 isn't magic
      digits_.output_base10(os, scale_, line_pos);
    }
    else {
      digits_.output(os, obase, scale_, line_pos);
    }
  }

  /** \brief  Debug output of a number.  */
  void debug(std::ostream& os) const
  {
    os << "Number(";
    digits_.debug(os);
    os << ", sign=" << (sign_ == Sign::positive ? "+" : "-")  // NOLINT
       << ", scale=" << static_cast<typename Traits::PrintType>(scale_) << ")";
  }

  /* Get the scale of the number.  */
  [[nodiscard]] auto scale() const -> NumType { return scale_; }

  /* Get the number of significant digits.  */
  [[nodiscard]] auto length() const -> NumType
  {
    if (digits_.is_zero()) {
      return std::max(static_cast<NumType>(1), scale());
    }
    return digits_.length();
  }

  /** \brief  Get the absolute value of \c *this.  */
  void abs() { sign_ = Sign::positive; }

  /** \brief  Negate \c *this.  */
  void negate() { sign_ = (sign_ == Sign::positive) ? Sign::negative : Sign::positive; }

  /** \brief  Add \a rhs to *this.
   *
   * Scale on exit is max of rhs scale and *this scale.
   */
  void add(BasicNumber const& rhs)
  {
    NumType const result_scale = std::max(scale_, rhs.scale_);
    if (rhs.digits_.is_zero()) {
      raise_scale(result_scale);
      return;
    }
    if (digits_.is_zero()) {
      *this = rhs;
      raise_scale(result_scale);
      return;
    }

    Details::BasicDigits<Traits> rhs_digits;
    NumType scale_diff;
    Sign rhs_sign = Sign::positive;
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

  /** \brief  Add 10^-scale() to this number.
   *
   * This is generally only useful for testing.
   */
  void add_epsilon()
  {
    if (is_zero()) {
      digits_ = Details::BasicDigits<Traits>(1);
      return;
    }

    digits_.add(Details::BasicDigits<Traits>(1), 0);
  }

  /** \brief  Subtract \a rhs from *this.
   *
   * Scale on exit is max of rhs scale and *this scale.
   */
  void sub(BasicNumber const& rhs)
  {
    NumType const result_scale = std::max(scale_, rhs.scale_);
    if (rhs.digits_.is_zero()) {
      raise_scale(result_scale);
      return;
    }
    if (digits_.is_zero()) {
      *this = rhs;
      negate();
      raise_scale(result_scale);
      return;
    }

    if (scale_ < rhs.scale_) {
      negate();
      add(rhs);
      negate();
      return;
    }
    if (sign_ == rhs.sign_) {
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
      rhs.output(ss, 10, 0);  // NOLINT - 10 isn't magic
      Details::error(Msg::raising_to_fractional_power, ss.str());
      return;
    }

    auto one = BasicNumber(1);
    if (power_whole.is_zero()) {
      /* x ^ 0 = 1.  Scale = min (scale(x) * 0, max(target_scale, scale(x))) = 0.  */
      *this = one;
      return;
    }
    /* Ensure we end up with the right sign.  */
    if (power_whole.is_even()) {
      sign_ = Sign::positive;
    }

    if (rhs.sign_ == Sign::positive &&
        power_whole.compare(one.digits_, 0) == Details::ComparisonResult::equality) {
      /* x ^ 1.  Scale = min (scale(x) * 1, max(target_scale, scale(x))) = scale(x).  */
      return;
    }

    if (rhs.sign_ == Sign::positive) {
      /* x ^ +p.  Scale = min(scale(x) * p, max(target_scale, scale(x)))
       * Keep at least the operand's precision before truncating the full power.
       */
      target_scale = std::max(target_scale, scale_);
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
      base_plus_target_scale.add(BasicNumber(base_));
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

    assert(rhs.sign_ == Sign::negative);  // NOLINT
    /* x ^ -p.  Scale = target_scale.  */
    digits_.power_mul(power_whole);
    scale_ *= power_whole.to_unsigned(0);
    /* digits_ now has scale: scale_ * power_whole.  */
    /* We need to do 1.0/digits_ with resultant scale target_scale.
     * Multiply one by 10^(scale_ + target_scale) and do the division.
     */
    one.digits_.mul_pow10(scale_ + target_scale);
    one.scale_ = scale_ + target_scale;
    one.divide(*this, target_scale);
    std::swap(one, *this);
  }

  /** \brief  Set the split point for choosing which multiplication algorithm to use.
   *  \param point New point (in range [1, base_]), unit is number of base_ digits.
   *
   * At and below \a point, use schoolbook multiplication (quadratic limb work).
   * Larger balanced products use recursive Karatsuba with reusable scratch.
   * Recursive leaves retain a minimum of three limbs; asymmetric children
   * with an empty high half also use the portable schoolbook fallback.
   */
  static void multiply_split_point(NumType point)
  {
    Details::BasicDigits<Traits>::multiply_split_point(point);
  }

  void multiply(BasicNumber const& rhs, NumType target_scale)
  {
    NumType const result_scale = scale() + rhs.scale();
    // Win32 doesn't support std::max({...});
    auto max_scale = std::max(scale(), rhs.scale());
    scale_ = std::min(result_scale, std::max(max_scale, target_scale));
    NumType const rescale = result_scale - scale_;
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
      // The by-value divisor coefficient is dead after this call; scale/sign
      // remain available below. Transfer it into normalization instead of copying.
      digits_.divide(std::move(rhs.digits_));
      break;
    }

    scale_ = target_scale;
    sign_ = sign_ == rhs.sign_ ? Sign::positive : Sign::negative;
  }

  void modulo(BasicNumber rhs, NumType target_scale)
  {
    // Public bc scales are below base_, so their sum fits in NumType for the
    // supported traits. Preserve the full quotient-product scale, not just the
    // division scale, before subtracting it from the original dividend.
    NumType const product_scale = static_cast<NumType>(target_scale + rhs.scale());
    NumType const result_scale = std::max(scale(), product_scale);
    BasicNumber u(*this);
    u.divide(rhs, target_scale);
    u.multiply(rhs, result_scale);
    sub(u);
    // Subtraction preserves the selected remainder scale, including zero.
    // Retain this fallback defensively if the scale-handling contract changes.
    if (scale_ < result_scale) {
      if (!is_zero()) {
        digits_.mul_pow10(result_scale - scale_);
      }
      scale_ = result_scale;
    }
  }

  void sqrt(NumType target_scale)
  {
    /* Scale we want to end up at.  */
    target_scale = std::max(target_scale, scale());

    if (is_zero()) {
      scale_ = target_scale;
      return;
    }

    if (sign_ == Sign::negative) {
      std::ostringstream ss;
      output(ss, 10, 0);  // NOLINT - 10 isn't magic
      Details::error(Msg::square_root_of_negative_number, ss.str());
      return;
    }

    /* We work to a scale which provides an extra block or two of digits at the bottom.  We also
     * rescale *this to the working_scale to save work later.
     */
    NumType const working_scale = target_scale + base_log10_ + 1;
    digits_.mul_pow10(working_scale - scale());
    scale_ = working_scale;

    /* Initial guess = 10^floor(N/2). N = number of integer digits (=length - scale).
     * This positive magnitude estimate may be below the root. Store it at working_scale.
     */
    NumType const initial_guess = (std::max(length(), scale()) - scale()) / 2;
    BasicNumber c(1);
    c.digits_.mul_pow10(initial_guess);
    c.digits_.mul_pow10(working_scale);
    c.scale_ = working_scale;
    BasicNumber p;

    /* c is the current guess,
     * p is the previous guess.
     *
     * According to Newton Rhapson c = p - f(p) / f'(p)  where f(x) = x * x - *this
     *                               = p - (p * p - *this) / (2 * p)
     *                               = (p * p + *this) / (2 * p)
     *                               = (p + *this / p) / 2
     *                               = (*this / p + p) / 2
     *
     * For integer coefficient n, the first Newton step is at least floor(sqrt(n)).
     * Subsequent steps strictly decrease above that floor; at the floor they may
     * stay fixed or increase by one. Stop on nondecrease and keep the previous
     * guess, avoiding the adjacent-integer two-cycle when n is one below a square.
     */
    bool first_iteration = true;
    bool improving = false;
    do {
      // The current guess is replaced immediately. Transfer its ownership to
      // the previous guess rather than copying and then releasing it.
      p = std::move(c);
      c = *this;
      c.divide(p, working_scale);
      assert(c.scale_ == p.scale_);  // NOLINT
      c.digits_.add(p.digits_, 0);
      c.digits_.divide(2);
      improving = first_iteration ? (p != c) : (c < p);
      first_iteration = false;
    } while (improving);

    // No later step reads p. Moving also keeps terminal rescaling from
    // detaching a result that would otherwise remain shared with this local.
    *this = std::move(p);
    if (scale() > target_scale) {
      digits_.div_pow10(scale() - target_scale);
      scale_ = target_scale;
    }
  }

  /** \brief  Are we equal to zero?
   *  \return True iff equal to zero.
   */
  [[nodiscard]] auto is_zero() const -> bool { return digits_.is_zero(); }

  auto operator==(BasicNumber const& rhs) const -> bool
  {
    return compare(rhs) == Details::ComparisonResult::equality;
  }

  auto operator<(BasicNumber const& rhs) const -> bool
  {
    return compare(rhs) == Details::ComparisonResult::less_than;
  }

  auto operator>(BasicNumber const& rhs) const -> bool
  {
    return compare(rhs) == Details::ComparisonResult::greater_than;
  }

private:
  /** Increase decimal scale without changing the value or allocating for zero. */
  void raise_scale(NumType new_scale)
  {
    if (new_scale > scale_) {
      if (!digits_.is_zero()) {
        digits_.mul_pow10(new_scale - scale_);
      }
      scale_ = new_scale;
    }
  }

  [[nodiscard]] auto compare(BasicNumber const& rhs) const -> Details::ComparisonResult
  {
    bool const lhs_zero = digits_.is_zero();
    bool const rhs_zero = rhs.digits_.is_zero();

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

    bool const lhs_scale_bigger = scale_ >= rhs.scale_;
    auto result = lhs_scale_bigger ? digits_.compare(rhs.digits_, scale_ - rhs.scale_)
                                   : rhs.digits_.compare(digits_, rhs.scale_ - scale_);
    if (result == Details::ComparisonResult::equality) {
      return result;
    }
    // Swapping the operands for scale alignment reverses magnitude ordering.
    // Two negative operands reverse it once more; equality is unchanged above.
    bool const keep_order = lhs_scale_bigger == (sign_ == Sign::positive);
    return keep_order ? result
                      : (result == Details::ComparisonResult::less_than
                           ? Details::ComparisonResult::greater_than
                           : Details::ComparisonResult::less_than);
  }

  Details::BasicDigits<Traits> digits_;  ///< The digits.
  Sign sign_ = Sign::positive;           ///< Sign (+/-1).
  NumType scale_ = 0;                    ///< Scale.
};

template<typename Traits>
auto operator<<(std::ostream& os, BasicNumber<Traits> const& num) -> std::ostream&
{
  num.debug(os);
  return os;
}

template<typename Traits>
auto operator!=(BasicNumber<Traits> const& lhs, BasicNumber<Traits> const& rhs) -> bool
{
  return !(lhs == rhs);
}

template<typename Traits>
auto operator>=(BasicNumber<Traits> const& lhs, BasicNumber<Traits> const& rhs) -> bool
{
  return !(lhs < rhs);
}

template<typename Traits>
auto operator<=(BasicNumber<Traits> const& lhs, BasicNumber<Traits> const& rhs) -> bool
{
  return !(lhs > rhs);
}

/** Default number format selected by pointer width.
 *
 * Use 32-bit limbs with 64-bit pointers, and 16-bit limbs otherwise. Explicit
 * trait instantiations remain available for portable generic-arithmetic tests.
 */
// NOLINTNEXTLINE
using Number = BasicNumber<std::conditional_t<sizeof(void*) == 8, NumberTraits32, NumberTraits16>>;

}  // namespace GD::Bc

template<typename NumberTraits>
struct fmt::formatter<GD::Bc::BasicNumber<NumberTraits>>
{
  using Number = GD::Bc::BasicNumber<NumberTraits>;

  constexpr auto parse(format_parse_context& ctx)
  {
    for (format_parse_context::iterator it = ctx.begin(); it != ctx.end(); ++it) {
      switch (*it) {
      case 'd':
        debug_ = true;
        break;
      case '}':
        return it;
      default:
        throw format_error("invalid format");
      }
    }

    if (ctx.begin() != ctx.end()) {
      throw format_error("invalid_format - not terminated");
    }

    return ctx.begin();
  }

  template<typename FormatContext>
  auto format(Number const& number, FormatContext& ctx) const
  {
    std::ostringstream os;
    if (debug_) {
      number.debug(os);
    }
    else {
      number.output(os, 10, 0);  // NOLINT - 10 isn't magic
    }
    // Work around Win32 STL bug:
    auto text = os.str();
    return fmt::vformat_to(ctx.out(), "{0}", fmt::make_format_args(text));
  }

private:
  bool debug_ = false;
};

#endif  // SRC_INCLUDE_UTIL_NUMBER_HH_INCLUDED
