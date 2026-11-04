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
#include <charconv>
#include <cstdint>
#include <iomanip>
#include <iterator>
#include <limits>
#include <locale>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <span>
#include <sstream>
#include <stdexcept>
#include <string_view>
#include <utility>
#include <vector>

#include "bc.hh"
#include "digit-storage.hh"
#include "limb-product.hh"
#if defined(GD_BC_EXPERIMENT_NTT)
#include "limb-ntt.hh"
#endif
#include "limb-schoolbook.hh"

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

/** Alphabet index for the existing raw constructor decoder, or 17 if absent.
 *  Preserve strchr's terminator index (16); existing per-base assertions still
 *  reject it in valid input. Wider character models retain the generic search.
 */
inline auto radix_character_index(char c) -> unsigned
{
  static constexpr char alphabet[] = "0123456789ABCDEF";
  if constexpr (std::numeric_limits<unsigned char>::max() == 255) {
    static constexpr auto indexes = [] {
      std::array<unsigned char, 256> table{};
      table.fill(17);
      for (unsigned i = 0; i < sizeof(alphabet); ++i)
        table[static_cast<unsigned char>(alphabet[i])] = static_cast<unsigned char>(i);
      return table;
    }();
    return indexes[static_cast<unsigned char>(c)];
  }
  else {
    auto const* found = ::strchr(alphabet, c);
    return found ? static_cast<unsigned>(found - alphabet) : 17;
  }
}

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

  /** Whether coefficient storage retains a heap owner rather than inline limbs. */
  [[nodiscard]] auto has_heap_storage() const noexcept -> bool { return digits_.has_heap(); }

  /** Whether another numeric value currently shares this coefficient owner. */
  [[nodiscard]] auto has_shared_heap_storage() const noexcept -> bool
  {
    return digits_.has_heap() && digits_.use_count() > 1;
  }

  /** Logical stored limbs for algorithm dimensions and optional diagnostics. */
  [[nodiscard]] auto coefficient_limbs() const noexcept -> std::size_t
  {
    return digits_ ? digits_->size() : 0;
  }

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

  /** Populate coefficient limbs directly from validated decimal text.
   * The caller supplies the sole radix-point position (or npos) and retains
   * its decimal scale. Leading coefficient zeros need no storage; fractional
   * trailing zeros remain part of the integer coefficient.
   */
  static auto from_decimal(std::string_view text, std::size_t point) -> BasicDigits
  {
    std::size_t first = 0;
    while (first < text.size() && (text[first] == '0' || text[first] == '.')) {
      ++first;
    }
    if (first == text.size()) {
      return BasicDigits();
    }
    auto const decimal_digits =
      text.size() - first - (point != std::string_view::npos && point >= first ? 1 : 0);
    auto const count = decimal_digits / base_log10_ + (decimal_digits % base_log10_ != 0);
    BasicDigits result;
    result.copy_on_write();
    result.digits_->resize(count, 0);
    // Decode the partial leading chunk first, then full chunks, storing into
    // little-endian limbs from the back. Horner accumulation avoids a separate
    // positional weight and wide multiply per character. Each prefix remains
    // below base_, which fits NumType for every supported trait.
    auto output = result.digits_->rbegin();
    NumType coefficient = 0;
    auto remaining = decimal_digits % base_log10_;
    if (remaining == 0) {
      remaining = base_log10_;
    }
    for (auto i = first; i < text.size(); ++i) {
      auto const c = text[i];
      if (c == '.') {
        continue;
      }
      coefficient = static_cast<NumType>(coefficient * NumType(10) + NumType(c - '0'));
      if (--remaining == 0) {
        *output++ = coefficient;
        coefficient = 0;
        remaining = base_log10_;
      }
    }
    assert(output == result.digits_->rend());  // NOLINT
    return result;
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

    return std::all_of(digits_->begin(), digits_->end(), [](auto d) {
      GD_BC_PROFILE_EVENT(Profile::Event::zero_scan_limbs, 1);
      return d == 0;
    });
  }

  /** Is this an even number? Both absent and empty storage represent zero. */
  [[nodiscard]] auto is_even() const -> bool
  {
    if (!digits_ || digits_->empty()) {
      return true;
    }

    return ((*digits_)[0] & 1) == 0;
  }

  // Isolate the limb comparator from surrounding VM/template inlining choices
  // on MSVC. Other compilers retain their existing generic inlining policy.
  [[nodiscard]]
#if defined(_MSC_VER)
  __declspec(noinline)
#endif
  auto compare(BasicDigits const& rhs, NumType scale) const -> Details::ComparisonResult
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
        GD_BC_PROFILE_EVENT(Profile::Event::comparison_limb_positions, 1);
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
      GD_BC_PROFILE_EVENT(Profile::Event::comparison_limb_positions, 1);
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
    NumType division_base = obase;
    std::size_t group_width = 1;
    // Keep short coefficients and the space-separated digits of bases above
    // sixteen on the scalar path. Long alphabetic output can divide once per
    // native group instead of once per printed character.
    if (obase <= 16 && whole.digits_ && whole.digits_->size() >= 4) {
      // The quotient is <= base_-1, which fits NumType. Name it in that type
      // so small-trait integer promotion cannot obscure the product bound.
      NumType const largest_factor = static_cast<NumType>((base_ - 1) / obase);
      while (division_base <= largest_factor) {
        division_base = static_cast<NumType>(division_base * obase);
        ++group_width;
      }
    }
    DigitVector whole_digits;
    while (!whole.is_zero()) {
      GD_BC_PROFILE_EVENT(Profile::Event::radix_output_divisions, 1);
      whole_digits.push_back(whole.divide(division_base));
    }
    std::string number;
    if (group_width == 1) {
      for (auto it = whole_digits.rbegin(); it != whole_digits.rend(); ++it)
        number += to_string(*it, obase);
    }
    else {
      // division_base < base_ keeps each remainder within a native limb.
      // A group uses fewer than NumType's bit count even in output base two.
      constexpr char const* alphabet = "0123456789ABCDEF";
      std::array<char, std::numeric_limits<NumType>::digits> buffer;
      bool first = true;
      for (auto it = whole_digits.rbegin(); it != whole_digits.rend(); ++it) {
        auto value = *it;
        auto position = buffer.size();
        do {
          buffer[--position] = alphabet[value % obase];
          value = static_cast<NumType>(value / obase);
        } while (value != 0);
        if (!first) {
          while (buffer.size() - position < group_width)
            buffer[--position] = '0';
        }
        number.append(buffer.data() + position, buffer.size() - position);
        first = false;
      }
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
        os << std::string_view(number).substr(p, line_length - line_pos - 1) << "\\\n";
        p += line_length - line_pos - 1;
        line_pos = 0;
      }
    }
    if (p != number.length()) {
      os << std::string_view(number).substr(p);
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
    // Serialize decimal limbs once, then apply the established point, scale
    // and continuation rules. A small reusable buffer handles interior limbs
    // without constructing a formatted stream for each coefficient.
    assert(!is_zero());  // NOLINT
    if (digits_printed >= line_length) {
      os << '\n';
      digits_printed = 0;
    }

    /* Generate string without radix point. Note we reverse through the digits as we want to print
     * big-endian, but numbers are stored little-endian.
     */
    std::string result;
    if (std::locale() == std::locale::classic()) {
      // One coefficient allocation at most; no retained cross-call workspace.
      if (digits_->size() > result.max_size() / base_log10_)
        throw std::length_error("decimal coefficient exceeds string capacity");
      result.reserve(digits_->size() * base_log10_);
      std::array<char, base_log10_> limb;
      auto rit = digits_->rbegin();
      auto const first =
        std::to_chars(limb.data(), limb.data() + limb.size(), static_cast<PrintType>(*rit));
      assert(first.ec == std::errc{});  // NOLINT: every limb is below 10^base_log10_.
      result.append(limb.data(), static_cast<std::size_t>(first.ptr - limb.data()));
      for (++rit; rit != digits_->rend(); ++rit) {
        auto value = *rit;
        // Interior limbs have exactly base_log10_ digits, including all zeros.
        // Reuse the same bounded buffer before appending each complete limb.
        for (auto at = limb.rbegin(); at != limb.rend(); ++at) {
          *at = static_cast<char>('0' + value % 10);
          value /= 10;
        }
        result.append(limb.data(), limb.size());
      }
    }
    else {
      // The historical temporary stream uses the global locale, independently
      // of the destination stream. Preserve custom facets/punctuation there.
      std::ostringstream ss;
      int width = 0;
      for (auto rit = digits_->rbegin(); rit != digits_->rend(); ++rit) {
        ss << std::setfill('0') << std::setw(width) << static_cast<PrintType>(*rit);
        width = static_cast<int>(base_log10_);
      }
      result = ss.str();
    }
    unsigned const break_at = (result.length() <= line_length) ? line_length : (line_length - 1);
    // A long coefficient reserves an extra column before its first chunk.
    // Continue the current number if its sign has already filled that space.
    if (digits_printed >= break_at) {
      os << "\\\n";
      digits_printed = 0;
    }
    auto const whole_end =
      result.end() - static_cast<std::ptrdiff_t>(std::min(std::size_t(scale), result.size()));
    auto it = std::find_if(result.begin(), whole_end, [](char c) { return c != '0'; });
    bool const print = it != whole_end;
    while (it != whole_end) {
      auto count =
        std::min(static_cast<std::size_t>(whole_end - it), std::size_t(break_at - digits_printed));
      if (os.width() != 0) {
        // A caller-supplied field width historically applies to the first
        // emitted character, rather than the complete coefficient chunk.
        os << *it;
        count = 1;
      }
      else {
        os.write(std::to_address(it), static_cast<std::streamsize>(count));
      }
      it += static_cast<std::ptrdiff_t>(count);
      digits_printed += static_cast<unsigned>(count);
      if (digits_printed == break_at) {
        digits_printed = 0;
        os << "\\\n";
      }
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
        os << std::string_view(result).substr(it - result.begin(), break_at - digits_printed)
           << "\\\n";
        // The loop and the scale invariant above bound the chunk by scale.
        scale = static_cast<NumType>(scale - (break_at - digits_printed));
        it += break_at - digits_printed;
        digits_printed = 0;
      }
      if (it != result.end()) {
        os << std::setfill('0') << std::right << std::setw(scale)
           << std::string_view(result).substr(it - result.begin());
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
    // Count logical limb positions, including the first significant limb.
    // The amount expression is not evaluated in ordinary builds.
    GD_BC_PROFILE_EVENT(Profile::Event::length_scan_limbs,
                        std::distance(digits_->rbegin(), rit) + (rit != finish ? 1 : 0));
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

  /** Internal storage-width scan, deliberately separate from public length.
   * Keep the established public function's call graph and narrowing unchanged.
   * Prefix-root dimensions must not narrow enlarged working coefficients.
   */
  [[nodiscard]] auto coefficient_digits() const -> std::size_t
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
    // Count logical limb positions, including the first significant limb.
    // The amount expression is not evaluated in ordinary builds.
    GD_BC_PROFILE_EVENT(Profile::Event::length_scan_limbs,
                        std::distance(digits_->rbegin(), rit) + (rit != finish ? 1 : 0));
    if (rit != finish) {
      length -= base_log10_;
      NumType d = *rit;
      assert(d != 0);  // NOLINT
      while (d != 0) {
        length += 1;
        d /= 10;  // NOLINT - 10 is not a magic number
      }
    }

    return std::max(std::size_t{1}, static_cast<std::size_t>(length));
  }

  /** Copy floor(coefficient * 10^growth / 10^drop) without an oversized owner.
   * Cancel shifts first, copy only surviving limbs when shrinking, and size
   * growing storage once. All dimensions are storage-width values.
   */
  [[nodiscard]] auto rescaled_copy(std::size_t growth, std::size_t drop) const -> BasicDigits
  {
    auto const cancelled = std::min(growth, drop);
    growth -= cancelled;
    drop -= cancelled;
    if (!digits_ || digits_->empty()) {
      return BasicDigits();
    }
    if (growth != 0) {
      auto const whole = growth / base_log10_;
      auto const count = digits_->size();
      if (whole >= std::numeric_limits<std::size_t>::max() - count) {
        throw std::length_error("decimal coefficient growth exceeds storage width");
      }
      BasicDigits result;
      // One spare limb absorbs a possible partial-radix carry without growth.
      result.digits_ = DigitVector(count + whole + 1, 0);
      std::copy(digits_->begin(), digits_->end(), result.digits_->begin() + whole);
      GD_BC_PROFILE_EVENT(Profile::Event::range_limb_bytes, count * sizeof(NumType));
      auto const part = growth % base_log10_;
      if (part != 0) {
        result.mac(static_cast<NumType>(pow10(part)), 0);
      }
      result.tidy();
      return result;
    }
    if (drop != 0) {
      auto const whole = drop / base_log10_;
      if (whole >= digits_->size()) {
        return BasicDigits();
      }
      BasicDigits result(digits_->begin() + whole, digits_->end());
      auto const part = drop % base_log10_;
      if (part != 0) {
        result.divide(static_cast<NumType>(pow10(part)));
      }
      else {
        result.tidy();
      }
      return result;
    }
    return *this;
  }

  // Only negative-exponent division needs this conversion. Keep its storage
  // handling outside the common division and VM dispatch bodies.
#if defined(_MSC_VER)
  __declspec(noinline)
#elif __has_cpp_attribute(gnu::noinline)
  [[gnu::noinline]]
#endif
  void truncate_dividend(std::size_t drop)
  {
    *this = rescaled_copy(0, drop);
    // Comparison requires present storage even if every digit was cut.
    mul_pow10(0);
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
    divide_impl<false>(std::move(v));
  }

  /** Replace the coefficient with its integer remainder by v.
   * The caller establishes that this coefficient is greater than v > 0.
   */
  void modulo(BasicDigits v) { divide_impl<true>(std::move(v)); }

  /** Immutable coefficient snapshot with reusable Algorithm D normalization.
   * Preparation rejects zero. The snapshot remains valid after its source is
   * mutated or destroyed; no decimal scale or VM location is retained.
   */
  class PreparedDivisor;
  class DivisorCache;
  [[nodiscard]] auto prepare_divisor() const -> PreparedDivisor;

  /** Divide by a prepared positive coefficient, including less/equal inputs. */
  void divide(PreparedDivisor const& divisor);
  /** Take the remainder by a prepared positive coefficient. */
  void modulo(PreparedDivisor const& divisor);

private:
  /** Algorithm D shares normalization and quotient estimates between both
   * outputs. Select the result at compile time so quotient-only callers keep
   * their original loop and remainder-only callers allocate no quotient.
   */
  template<bool Remainder>
  void divide_impl(BasicDigits v)
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
      auto const remainder = divide(v.digits_->at(0));
      if constexpr (Remainder) {
        digits_ = DigitVector(std::initializer_list<NumType>{remainder});
        tidy();
      }
      return;
    }

    if (digits_->size() == 2) {
      assert(v.digits_->size() == 2);  // NOLINT
      WideType const u2 = (*digits_)[0] + (base_ * (*digits_)[1]);
      WideType const v2 = (*v.digits_)[0] + (base_ * (*v.digits_)[1]);
      WideType const q = Remainder ? u2 % v2 : u2 / v2;

      digits_ = DigitVector(std::initializer_list<NumType>{static_cast<NumType>(q % base_),
                                                           static_cast<NumType>(q / base_)});
      if constexpr (Remainder) {
        tidy();
      }
      return;
    }

    /* D1. Normalize.  */
    /* This multiplier makes the divisor's highest limb at least base_/2
     * without increasing its limb count.
     */
    auto const n = v.digits_->size();

    NumType d = (static_cast<NumType>(base_) / 2 - 1) / v.digits_->back();
    d += 1;
    if (d != 1) {
      v.mac(d, 0);
    }
    assert(n == v.digits_->size());          // NOLINT
    assert(v.digits_->back() >= base_ / 2);  // NOLINT
    divide_normalized_impl<Remainder>(std::span<NumType const>(v.digits_->begin(), n), d);
  }

  template<bool Remainder>
  void divide_prepared_impl(PreparedDivisor const& divisor);

  /** Run Algorithm D with a read-only, already normalized divisor. The caller
   * supplies unique, tidy dividend storage and establishes dividend > divisor.
   * Only the dividend is scaled here; prepared divisors reuse their scaled limbs.
   */
  template<bool Remainder>
  void divide_normalized_impl(std::span<NumType const> divisor, NumType d)
  {
    auto const n = divisor.size();
    auto const m = digits_->size() - n;
    if (d != 1) {
      mac(d, 0);
    }
    digits_->push_back(0);

    BasicDigits q;
    if constexpr (!Remainder) {
      q.digits_ = DigitVector(m + 1, static_cast<NumType>(0));
    }

    // D3-D6 change limb values but never resize these buffers. Resolve the
    // storage modes once, after normalization/growth and before the final swap.
    // The established extents bound every index in the quotient-digit loop.
    auto dividend = std::span(digits_->begin(), digits_->size());
    std::span<NumType> quotient;
    if constexpr (!Remainder) {
      quotient = std::span(q.digits_->begin(), q.digits_->size());
      assert(quotient.size() == m + 1);  // NOLINT
    }
    assert(n >= 2 && n + m < dividend.size());  // NOLINT

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
      if constexpr (!Remainder) {
        quotient[j] = static_cast<NumType>(q_hat);
      }

    } while (j-- > 0);

    /* D7. The low n dividend limbs hold d times the exact remainder. */
    if constexpr (Remainder) {
      digits_->resize(n);
      if (d != 1) {
        auto const discarded = divide(d);
        assert(discarded == 0);  // NOLINT - normalization is exact.
        static_cast<void>(discarded);
      }
      else {
        tidy();
      }
    }
    else {
      std::swap(q.digits_, digits_);
    }
  }

public:
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

#if defined(_MSC_VER)
  __declspec(noinline)
#elif __has_cpp_attribute(gnu::noinline)
  [[gnu::noinline]]
#endif
  static auto multiply_runs(std::size_t lhs_count, std::size_t rhs_count, WideType lhs,
                            WideType rhs, WideType lhs_top, WideType rhs_top) -> BasicDigits
  {
    auto const last = lhs_count + rhs_count;
    auto const interior = lhs * rhs;
    auto const left_edge = lhs_top * rhs;
    auto const right_edge = lhs * rhs_top;
    auto const top = lhs_top * rhs_top;
    BasicDigits result;
    result.copy_on_write();
    result.digits_->resize(last + 2);

    // A diagonal crosses a triangular (or trapezoidal) number of equal
    // interior products and at most one product from each leading limb.
    // Split products at the radix before multiplying by the overlap count.
    WideType carry = 0;
    for (std::size_t index = 0; index < last; ++index) {
      auto const count =
        static_cast<WideType>(std::min({index + 1, lhs_count, rhs_count, last - index - 1}));
      auto low = static_cast<WideType>(count * (interior % base_) + carry);
      auto high = static_cast<WideType>(count * (interior / base_));
      if (index >= lhs_count) {
        low += left_edge % base_;
        high += static_cast<WideType>(left_edge / base_);
      }
      if (index >= rhs_count) {
        low += right_edge % base_;
        high += static_cast<WideType>(right_edge / base_);
      }
      (*result.digits_)[index] = static_cast<NumType>(low % base_);
      carry = high + low / base_;
    }
    // Keep the leading product split as well. The carry can exceed one limb
    // in the middle of the result, even though the final carry cannot.
    auto const low = top % base_ + carry;
    (*result.digits_)[last] = static_cast<NumType>(low % base_);
    auto const final_carry = top / base_ + low / base_;
    assert(final_carry < base_);  // NOLINT
    (*result.digits_)[last + 1] = static_cast<NumType>(final_carry);
    return result;
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

    /* Have to do the full thing... */
    BasicDigits result;
    result.copy_on_write();
    auto const result_size = (lhs_end - lhs_begin) + (rhs_end - rhs_begin) + 2;
    result.digits_->resize(result_size, 0);
    if (Details::LimbSchoolbook<Traits>::eligible(lhs_end - lhs_begin, rhs_end - rhs_begin)) {
      Details::LimbSchoolbook<Traits>::multiply(
        std::span<NumType const>(lhs_begin, lhs_end), std::span<NumType const>(rhs_begin, rhs_end),
        std::span<NumType>(result.digits_->begin(), result.digits_->size()));
      return result;
    }
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
    // Keep run detection in the recursive path. Short schoolbook products
    // need no shape checks, and irregular recursive operands fall through.
    constexpr std::size_t run_threshold = 32;
    auto const shorter = std::min(lhs_size, rhs_size);
    // With at most shorter + 1 terms on a diagonal, the low parts and
    // incoming carry together occupy less than twice that many bases.
    if (shorter >= run_threshold &&
        shorter + 1 <= std::numeric_limits<WideType>::max() / base_ / 2 && lhs_begin[0] != 0 &&
        rhs_begin[0] != 0 && lhs_begin[0] == lhs_begin[1] && rhs_begin[0] == rhs_begin[1] &&
        std::all_of(lhs_begin + 2, lhs_end - 1,
                    [digit = *lhs_begin](auto d) { return d == digit; }) &&
        std::all_of(rhs_begin + 2, rhs_end - 1,
                    [digit = *rhs_begin](auto d) { return d == digit; })) {
      return multiply_runs(lhs_size - 1, rhs_size - 1, *lhs_begin, *rhs_begin, lhs_end[-1],
                           rhs_end[-1]);
    }
    auto const point = static_cast<std::size_t>(multiply_split_point_);
    using Kernel = LimbProduct<Traits>;
    // Allocate output and scratch once. Recursive products only borrow spans;
    // no owner escapes and no generic decimal rescaling is used to recombine.
    BasicDigits result;
    result.copy_on_write();
    result.digits_->resize(lhs_size + rhs_size, NumType{0});
#if defined(GD_BC_EXPERIMENT_NTT)
    if constexpr (std::is_same_v<Traits, NumberTraits32>) {
      auto const left = std::span<NumType const>(lhs_begin, lhs_end);
      if (lhs_size == rhs_size && std::to_address(lhs_begin) == std::to_address(rhs_begin) &&
          Details::LimbNtt::eligible(left)) {
        Details::LimbNtt::square(left, {result.digits_->begin(), result.digits_->end()});
        result.tidy();
        return result;
      }
    }
#endif
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
class BasicDigits<Traits>::PreparedDivisor
{
public:
  [[nodiscard]] auto coefficient_limbs() const noexcept -> std::size_t
  {
    return original_.coefficient_limbs();
  }

private:
  friend class BasicDigits<Traits>;
  friend class DivisorCache;

  explicit PreparedDivisor(BasicDigits const& source) : original_(source)
  {
    if (original_.is_zero()) {
      throw std::domain_error("Cannot prepare a zero divisor");
    }
    // Trim only when necessary: a canonical source with factor one needs no
    // writable copy. Both retained coefficients otherwise share its snapshot.
    auto const& storage = original_.digits_;
    if (storage->back() == 0) {
      original_.copy_on_write();
      original_.tidy();
    }
    normalized_ = original_;
    if (storage->size() >= 2) {
      factor_ = (static_cast<NumType>(base_) / 2 - 1) / storage->back() + 1;
      if (factor_ != 1) {
        normalized_.mac(factor_, 0);
      }
      assert(normalized_.coefficient_limbs() == original_.coefficient_limbs());  // NOLINT
    }
  }

  BasicDigits original_;
  BasicDigits normalized_;
  NumType factor_ = 1;
};

template<typename Traits>
auto BasicDigits<Traits>::prepare_divisor() const -> PreparedDivisor
{
  return PreparedDivisor(*this);
}

template<typename Traits>
void BasicDigits<Traits>::divide(PreparedDivisor const& divisor)
{
  divide_prepared_impl<false>(divisor);
}

template<typename Traits>
void BasicDigits<Traits>::modulo(PreparedDivisor const& divisor)
{
  divide_prepared_impl<true>(divisor);
}

template<typename Traits>
template<bool Remainder>
void BasicDigits<Traits>::divide_prepared_impl(PreparedDivisor const& divisor)
{
  copy_on_write();
  tidy();
  auto const relation = compare(divisor.original_, 0);
  if (relation == ComparisonResult::less_than) {
    if constexpr (!Remainder) {
      reset();
    }
    return;
  }
  if (relation == ComparisonResult::equality) {
    *this = BasicDigits(static_cast<NumType>(Remainder ? 0 : 1));
    return;
  }
  if (divisor.coefficient_limbs() == 1 || digits_->size() == 2) {
    // Keep the generic scalar and double-width paths for short operands.
    divide_impl<Remainder>(divisor.original_);
    return;
  }
  auto const& normalized = divisor.normalized_.digits_;
  divide_normalized_impl<Remainder>(
    std::span<NumType const>(normalized->begin(), normalized->size()), divisor.factor_);
}

/** One-entry admission cache for immutable prepared coefficients.
 * A first observation holds only weak identity. A second computes an admission
 * fingerprint; a third stable observation prepares the current value and retains its unshifted
 * snapshot; cached hits therefore require exact ownership and decimal-padding identity. The
 * fingerprint never authorizes reuse of a previously prepared value. Experimental VM
 * dispatch opts into admission; the default interpreter uses ordinary division.
 */
template<typename Traits>
class BasicDigits<Traits>::DivisorCache
{
public:
  static constexpr std::size_t max_retained_limb_bytes = 64 * 1024;

  DivisorCache() = default;
  DivisorCache(DivisorCache const&) = delete;
  auto operator=(DivisorCache const&) -> DivisorCache& = delete;

  /** Return a prepared value after repeated stable observations, otherwise
   * nullptr for the ordinary fallback. The returned pointer lasts until the
   * next lookup, clear, or destruction of this cache. No strong source owner is kept
   * on a first observation or a miss. First observations compute no admission
   * fingerprint; numeric zero preflight may scan the source. Promotion after
   * three stable observations may allocate or throw.
   * The source coefficient must remain unchanged throughout the lookup.
   */
  [[nodiscard]] auto lookup(BasicDigits const& source, NumType padding = 0)
    -> PreparedDivisor const*
  {
    GD_BC_PROFILE_EVENT(Profile::Event::divisor_cache_lookups, 1);
    auto const capacity = source.digits_.capacity();
    constexpr auto limit = max_retained_limb_bytes / sizeof(NumType);
    // Compare in the scale field's width before accepting its bounded extent.
    auto const whole = padding / base_log10_;
    // Conservative preflight leaves room for three retained coefficient views
    // and possible shift growth. Inspect actual capacities again on promotion.
    if (!source.has_heap_storage() || source.coefficient_limbs() < 2 || capacity > limit / 3 ||
        whole > limit / 3 - capacity || source.is_zero()) {
      clear();
      return nullptr;
    }
    bool const same_owner = source.digits_.shares_owner(observed_);
    bool const same_padding = padding == padding_;
    if (entry_ && same_owner && same_padding) {
      GD_BC_PROFILE_EVENT(Profile::Event::divisor_cache_hits, 1);
      return &entry_->prepared;
    }

    // Release an old immutable snapshot before entering the caller's fallback.
    // Keeping it on a miss could force an otherwise unnecessary COW clone.
    entry_.reset();
    if (!same_owner || !same_padding) {
      observed_ = source.digits_.weak_owner();
      padding_ = padding;
      fingerprint_valid_ = false;
      rejected_ = false;
      return nullptr;
    }
    auto const fingerprint = admission_fingerprint(source);
    if (!fingerprint_valid_ || fingerprint != fingerprint_) {
      fingerprint_ = fingerprint;
      fingerprint_valid_ = true;
      rejected_ = false;
      return nullptr;
    }
    if (rejected_) {
      return nullptr;
    }
    GD_BC_PROFILE_EVENT(Profile::Event::divisor_cache_preparation_attempts, 1);
    Entry candidate(source, padding);
    if (!within_budget(candidate)) {
      rejected_ = true;
      return nullptr;
    }
    entry_.emplace(std::move(candidate));
    GD_BC_PROFILE_EVENT(Profile::Event::divisor_cache_promotions, 1);
    return &entry_->prepared;
  }

  void clear() noexcept
  {
    entry_.reset();
    observed_.reset();
    fingerprint_ = 0;
    fingerprint_valid_ = false;
    padding_ = 0;
    rejected_ = false;
  }

  /** Conservative capacity charge, counting shared coefficient views separately.
   * Fixed cache metadata/control blocks and external owners are not limb bytes.
   */
  [[nodiscard]] auto retained_limb_bytes() const noexcept -> std::size_t
  {
    if (!entry_) {
      return 0;
    }
    return (entry_->source.digits_.capacity() + entry_->prepared.original_.digits_.capacity() +
            entry_->prepared.normalized_.digits_.capacity()) *
           sizeof(NumType);
  }

private:
  struct Entry
  {
    Entry(BasicDigits const& value, NumType padding)
        : source(value), prepared(prepare(value, padding))
    {
    }
    static auto prepare(BasicDigits const& value, NumType padding) -> PreparedDivisor
    {
      auto aligned = value;
      if (padding != 0) {
        aligned.mul_pow10(padding);
      }
      return aligned.prepare_divisor();
    }
    BasicDigits source;
    PreparedDivisor prepared;
  };

  static auto within_budget(Entry const& entry) noexcept -> bool
  {
    auto remaining = max_retained_limb_bytes / sizeof(NumType);
    for (auto capacity :
         {entry.source.digits_.capacity(), entry.prepared.original_.digits_.capacity(),
          entry.prepared.normalized_.digits_.capacity()}) {
      if (capacity > remaining) {
        return false;
      }
      remaining -= capacity;
    }
    return true;
  }

  static auto admission_fingerprint(BasicDigits const& source) noexcept -> std::uint64_t
  {
    GD_BC_PROFILE_EVENT(Profile::Event::divisor_cache_fingerprint_limbs,
                        source.coefficient_limbs());
    // lookup already rejected numeric zero, and its ownership bookkeeping
    // does not mutate the source. A second nonzero scan/optional failure path
    // would duplicate that precondition; retain only the admission hash.
    std::uint64_t hash = 14695981039346656037ULL;
    for (auto limb : source.digits_) {
      hash = (hash ^ static_cast<std::uint64_t>(limb)) * 1099511628211ULL;
    }
    return (hash ^ source.coefficient_limbs()) * 1099511628211ULL;
  }

  typename DigitVector::WeakOwner observed_;
  std::uint64_t fingerprint_ = 0;
  NumType padding_ = 0;
  bool rejected_ = false;
  bool fingerprint_valid_ = false;
  std::optional<Entry> entry_;
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
  using DivisorCache = typename Details::BasicDigits<Traits>::DivisorCache;
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

  /** Storage query for generic copy/move cost policies, not numeric magnitude.
   * A coefficient may retain heap capacity after shrinking to a small value.
   */
  [[nodiscard]] auto has_heap_storage() const noexcept -> bool
  {
    return digits_.has_heap_storage();
  }

  /** Storage ownership query before making a by-value arithmetic operand. */
  [[nodiscard]] auto has_shared_heap_storage() const noexcept -> bool
  {
    return digits_.has_shared_heap_storage();
  }

  /** \brief       Construct a number.
   *  \param s     String representing the number.
   *  \param ibase Input base in range [2, 16]
   *
   * Can only handle positive numbers.
   */
#if defined(__GNUC__) && !defined(__clang__)
  // Isolate runtime-base decoding from GCC's surrounding VM/template inlining.
  // Other compilers retain their generic policy and the same constructor body.
  __attribute__((noinline))
#endif
  BasicNumber(std::string_view s, NumType ibase)
      : digits_(), sign_(Sign::positive), scale_(0)
  {
    GD_BC_PROFILE_EVENT(Profile::Event::literal_conversions, 1);
    assert(ibase >= 2);   // NOLINT
    assert(ibase <= 16);  // NOLINT

    bool const single_literal = s.size() == 1;
    if (single_literal && s.front() >= '0' && s.front() <= '9') {
      // Single digits retain their base-independent value without chunk
      // planning or temporary storage. Hexadecimal letters keep the decoder below.
      digits_.mac(ibase, static_cast<NumType>(s.front() - '0'));
      return;
    }

    if (ibase == 10) {  // NOLINT - decimal limbs match the input radix.
      std::size_t point = std::string_view::npos;
      bool decimal = true;
      for (std::size_t i = 0; i < s.size(); ++i) {
        if (s[i] == '.' && point == std::string_view::npos) {
          point = i;
        }
        else if (s[i] < '0' || s[i] > '9') {
          decimal = false;
          break;
        }
      }
      if (decimal) {
        // Nondecimal forms retain the generic decoder contract. Keep the
        // original fractional scale; no definition-time base is assumed.
        scale_ =
          point == std::string_view::npos ? NumType{0} : static_cast<NumType>(s.size() - point - 1);
        digits_ = Details::BasicDigits<Traits>::from_decimal(s, point);
        return;
      }
    }

    // Large integer text has a measured divide-and-conquer regime. Keep
    // fractional input on the established grouped rescaling path. Cache powers
    // must fit the trait's native exponent constructor; narrower domains fall
    // back without changing their accepted text/value contracts.
    if constexpr (std::numeric_limits<NumType>::max() >= radix_dc_cutoff / 2) {
      if (ibase != 10 && s.size() >= radix_dc_cutoff && s.find('.') == std::string_view::npos) {
        auto const significant = s.find_first_not_of('0');
        if (significant == std::string_view::npos)
          return;
        s.remove_prefix(significant);
        if (s.size() >= radix_dc_cutoff &&
            s.size() - s.size() / 2 <= std::numeric_limits<NumType>::max()) {
          GD_BC_PROFILE_EVENT(Profile::Event::radix_dc_input_calls, 1);
          *this = parse_radix_integer_dc(s, ibase);
          return;
        }
      }
    }

    /* Construct the integer coefficient C by ignoring the radix point and count
     * fractional input digits in scale_. The input value is C / ibase^scale_;
     * decimal storage needs floor(C * 10^scale_ / ibase^scale_).
     */
    bool seen_period = false; /* Have we seen the radix-point. */

    // Keep common short literals on their original scalar path. For longer
    // text, bound a chunk's multiplier and coefficient by both the native limb
    // and widened MAC headroom. This retains generic narrower-trait fallback.
    std::size_t group_size = 1;
    NumType group_multiplier = ibase;
    if (s.size() >= 16) {
      constexpr WideType max_chunk =
        std::min(WideType(std::numeric_limits<NumType>::max()),
                 WideType(std::numeric_limits<WideType>::max() / base_));
      if constexpr (max_chunk >= 16) {
        // Reserve the full legacy decoder range. Splitting the division avoids
        // overflowing the widened type while deriving the power bound.
        WideType const power_bound =
          (max_chunk / 16) * (ibase - 1) + ((max_chunk % 16) * (ibase - 1)) / 16 + 1;
        WideType const last_before_multiply = power_bound / ibase;
        WideType power = 1;
        std::size_t count = 0;
        while (power <= last_before_multiply && count < s.size()) {
          power *= ibase;
          ++count;
        }
        if (count > 1) {
          group_size = count;
          group_multiplier = static_cast<NumType>(power);
        }
      }
    }

    if (group_size == 1) {
      for (auto c : s) {
        if (seen_period) {
          ++scale_;
        }

        auto const digit = Details::radix_character_index(c);
        if (digit != 17) {
          assert((digit < ibase) || (single_literal && digit < 16));  // NOLINT
          digits_.mac(ibase, static_cast<NumType>(digit));
        }
        else if (c == '.') {
          assert(!seen_period);  // NOLINT
          seen_period = true;
        }
        else {
          abort();
        }
      }
    }
    else {
      WideType coefficient = 0;
      std::size_t pending = 0;
      for (auto c : s) {
        if (seen_period)
          ++scale_;
        if (c == '.') {
          assert(!seen_period);  // NOLINT
          seen_period = true;
          continue;
        }
        auto const digit = Details::radix_character_index(c);
        if (digit == 17)
          abort();
        assert((digit < ibase) || (single_literal && digit < 16));  // NOLINT
        coefficient = coefficient * ibase + WideType(digit);
        if (++pending == group_size) {
          digits_.mac(group_multiplier, static_cast<NumType>(coefficient));
          coefficient = 0;
          pending = 0;
        }
      }
      if (pending != 0) {
        NumType power = 1;
        for (std::size_t i = 0; i < pending; ++i)
          power *= ibase;
        digits_.mac(power, static_cast<NumType>(coefficient));
      }
    }

    /* The stored coefficient is C. Integer or zero values need no fractional
     * rescaling; decimal input has already used the direct path above. */
    if (scale_ == 0 || digits_.is_zero()) {
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
#if defined(__GNUC__) && !defined(__clang__)
  // Keep formatting out of GCC's surrounding execution-frame inlining.
  // Other compilers retain generic policy and the identical formatting body.
  __attribute__((noinline))
#endif
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

#if defined(__APPLE__) && defined(__aarch64__) && defined(__clang__)
  // Keep this large hot function's entry alignment stable as decoders grow.
  // Other platforms retain their compiler's normal alignment policy.
  __attribute__((aligned(64)))
#endif
  void divide(BasicNumber rhs, NumType target_scale)
  {
    if (rhs.is_zero()) {
      Details::error(Msg::divide_by_zero);
      return;
    }

    // For nonnegative coefficients A/B, operand scales sa/sb and result
    // scale t, the quotient coefficient is floor(A*10^(t+sb-sa)/B).
    // Cancel powers introduced on both sides rather than padding a small
    // integer divisor to the numerator's fractional scale. Differences avoid
    // overflowing a scale field when the combined exponent would be large.
    if (scale() > rhs.scale()) {
      NumType const difference = scale() - rhs.scale();
      if (target_scale >= difference) {
        digits_.mul_pow10(target_scale - difference);
      }
      else {
        // For integer magnitudes, floor(A/(B*10^k)) equals
        // floor(floor(A/10^k)/B). Drop numerator digits first instead of
        // padding B, retaining scalar division and only surviving storage.
        digits_.truncate_dividend(difference - target_scale);
      }
    }
    else {
      if (scale() < rhs.scale()) {
        digits_.mul_pow10(rhs.scale() - scale());
      }
      digits_.mul_pow10(target_scale);
    }

    // Record logical coefficient dimensions after decimal-exponent reduction and before
    // comparison/normalization. These are request totals, not CPU or RSS costs.
    GD_BC_PROFILE_EVENT(Profile::Event::division_requests, 1);
    GD_BC_PROFILE_EVENT(Profile::Event::division_dividend_limbs, digits_.coefficient_limbs());
    GD_BC_PROFILE_EVENT(Profile::Event::division_divisor_limbs, rhs.digits_.coefficient_limbs());
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

  void modulo(BasicNumber rhs, NumType target_scale) { modulo_impl(std::move(rhs), target_scale); }

  /** Try prepared arithmetic without copying the RHS on a cache miss.
   * False leaves both numeric operands unchanged; the caller owns ordinary
   * fallback and zero diagnostics. Cache observation may change or allocate.
   * On a hit, capture RHS metadata before mutating a possibly aliased LHS.
   */
  [[nodiscard]] auto try_divide_reusing(BasicNumber const& rhs, NumType target_scale,
                                        DivisorCache& cache) -> bool
  {
    return try_divide_reusing_impl<false>(rhs, target_scale, cache);
  }
  [[nodiscard]] auto try_modulo_reusing(BasicNumber const& rhs, NumType target_scale,
                                        DivisorCache& cache) -> bool
  {
    return try_divide_reusing_impl<true>(rhs, target_scale, cache);
  }

  /** By-value compatibility entries retain ordinary arithmetic on a miss. */
  void divide_reusing(BasicNumber rhs, NumType target_scale, DivisorCache& cache)
  {
    if (!try_divide_reusing(rhs, target_scale, cache)) {
      divide(std::move(rhs), target_scale);
    }
  }
  void modulo_reusing(BasicNumber rhs, NumType target_scale, DivisorCache& cache)
  {
    if (!try_modulo_reusing(rhs, target_scale, cache)) {
      modulo(std::move(rhs), target_scale);
    }
  }

  void sqrt(NumType target_scale) { sqrt_impl(target_scale); }

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
  template<bool Remainder>
  auto try_divide_reusing_impl(BasicNumber const& rhs, NumType target_scale, DivisorCache& cache)
    -> bool
  {
    auto const rhs_scale = rhs.scale();
    auto const rhs_sign = rhs.sign_;
#if GD_BC_PROFILE
    auto const rhs_limbs = rhs.digits_.coefficient_limbs();
#endif
    NumType padding = 0;
    NumType result_scale = target_scale;
    if constexpr (Remainder) {
      if (target_scale > std::numeric_limits<NumType>::max() - rhs_scale) {
        // Preserve the ordinary defensive path outside the scale field.
        cache.clear();
        return false;
      }
      result_scale = std::max(scale(), static_cast<NumType>(target_scale + rhs_scale));
      padding = static_cast<NumType>(result_scale - rhs_scale - target_scale);
    }
    else if (scale() > rhs_scale) {
      auto const difference = static_cast<NumType>(scale() - rhs_scale);
      if (target_scale < difference) {
        padding = static_cast<NumType>(difference - target_scale);
      }
    }

    // Observe the unpadded owner. mul_pow10(0) also performs COW, so doing
    // that first would lose identity and could force a clone on every miss.
    auto const* prepared = cache.lookup(rhs.digits_, padding);
    if (prepared == nullptr) {
      // Leave fallback and its zero diagnostic at the caller, before any
      // dividend mutation. lookup may already have scanned the divisor.
      return false;
    }

    if constexpr (Remainder) {
      digits_.mul_pow10(static_cast<NumType>(result_scale - scale()));
    }
    else if (scale() > rhs_scale) {
      auto const difference = static_cast<NumType>(scale() - rhs_scale);
      digits_.mul_pow10(
        static_cast<NumType>(target_scale >= difference ? target_scale - difference : 0));
    }
    else {
      if (scale() < rhs_scale) {
        digits_.mul_pow10(static_cast<NumType>(rhs_scale - scale()));
      }
      // Keep the two numerator shifts separate: their sum can exceed NumType.
      digits_.mul_pow10(target_scale);
    }
    GD_BC_PROFILE_EVENT(Profile::Event::division_requests, 1);
    GD_BC_PROFILE_EVENT(Profile::Event::division_dividend_limbs, digits_.coefficient_limbs());
    // Ordinary division reports unshifted RHS storage before tidying. Modulo
    // always tidies via mul_pow10, even at zero padding; retain that convention.
    GD_BC_PROFILE_EVENT(Profile::Event::division_divisor_limbs,
                        !Remainder && padding == 0 ? rhs_limbs : prepared->coefficient_limbs());
    if constexpr (Remainder) {
      digits_.modulo(*prepared);
    }
    else {
      digits_.divide(*prepared);
      sign_ = sign_ == rhs_sign ? Sign::positive : Sign::negative;
    }
    scale_ = result_scale;
    return true;
  }

  // Native-width modulo entries compile this same generic body outside the VM.
  void modulo_impl(BasicNumber rhs, NumType target_scale)
  {
    if (target_scale <= std::numeric_limits<NumType>::max() - rhs.scale()) {
      if (rhs.is_zero()) {
        Details::error(Msg::divide_by_zero);
        return;
      }
      // With result scale R=max(sa,t+sb), the remainder coefficient is
      // (A*10^(R-sa)) mod (B*10^(R-sb-t)). Both exponents are nonnegative.
      // Its sign is the dividend's sign, independent of the divisor's sign.
      auto const result_scale = std::max(scale(), static_cast<NumType>(target_scale + rhs.scale()));
      digits_.mul_pow10(result_scale - scale());
      rhs.digits_.mul_pow10(result_scale - rhs.scale() - target_scale);
      GD_BC_PROFILE_EVENT(Profile::Event::division_requests, 1);
      GD_BC_PROFILE_EVENT(Profile::Event::division_dividend_limbs, digits_.coefficient_limbs());
      GD_BC_PROFILE_EVENT(Profile::Event::division_divisor_limbs, rhs.digits_.coefficient_limbs());
      auto const comparison = digits_.compare(rhs.digits_, 0);
      if (comparison == Details::ComparisonResult::equality) {
        digits_.reset();
      }
      else if (comparison == Details::ComparisonResult::greater_than) {
        digits_.modulo(std::move(rhs.digits_));
      }
      scale_ = result_scale;
      return;
    }
    // Keep the existing path for a sum outside the trait's scale field;
    // the direct coefficient formula must never use a wrapped exponent.
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

  // Keep the generic algorithm available for every trait. Native-width entry
  // points below call this same implementation from number-sqrt.cc.
  void sqrt_impl(NumType target_scale)
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

    // Guard digits are optional for exact integer-coefficient Newton iteration.
    // If the scale field cannot represent them, work at the requested precision:
    // wrapping would instead lose both fractional digits and the result scale.
    // Retain exact coefficient Newton iteration for small roots. Large
    // roots can grow significant coefficient precision in exact prefixes.
    constexpr auto guards = static_cast<NumType>(base_log10_ + 1);
    NumType const working_scale = target_scale <= std::numeric_limits<NumType>::max() - guards
                                    ? static_cast<NumType>(target_scale + guards)
                                    : target_scale;
    if (sqrt_prefix_if_large(working_scale, target_scale)) {
      return;
    }

    // Determine the magnitude from the original value, before adding working
    // precision. The enlarged coefficient can exceed the narrow length helper's
    // result domain even though its original input and requested scale are valid.
    // This power-of-ten estimate may be below the root; the first step is bounded.
    // Let P be coefficient digits and S the input scale. A power near the
    // root is 10^floor((P-S)/2), including when that exponent is negative.
    // Split the two signs to avoid unsigned underflow or signed narrowing.
    NumType const input_digits = digits_.length();
    NumType const initial_guess = input_digits >= scale_ ? (input_digits - scale_) / 2 : 0;
    NumType const fractional_digits = input_digits < scale_ ? scale_ - input_digits : 0;
    NumType const fractional_shift = fractional_digits / 2 + fractional_digits % 2;

    // Rescale once so each quotient/average is the floor of the corresponding
    // integer-coefficient operation. The nondecreasing-step test below therefore
    // retains the exact floor root, even when no extra guard scale is available.
    digits_.mul_pow10(working_scale - scale());
    scale_ = working_scale;

    BasicNumber c(1);
    c.digits_.mul_pow10(initial_guess);
    // working_scale >= input scale >= fractional_shift. Keep integer growth
    // separate so adding a positive magnitude exponent cannot wrap NumType.
    c.digits_.mul_pow10(working_scale - fractional_shift);
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
      GD_BC_PROFILE_EVENT(Profile::Event::sqrt_iterations, 1);
      GD_BC_PROFILE_EVENT(Profile::Event::sqrt_working_scale_sum, working_scale);
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

  static void grow_coefficient(Details::BasicDigits<Traits>& value, std::size_t places)
  {
    // The decimal storage shift API uses the public trait width. A working
    // coefficient can need a wider offset, so split it without narrowing.
    constexpr auto maximum = std::numeric_limits<NumType>::max();
    while (places != 0) {
      auto const part = static_cast<NumType>(std::min(places, std::size_t(maximum)));
      value.mul_pow10(part);
      places -= part;
    }
  }

  // Unusual traits use this header-defined implementation. Native-width
  // specializations declared below are emitted in number-sqrt.cc, keeping the
  // large prefix kernel out of the VM translation unit.
  auto sqrt_prefix_if_large(NumType working_scale, NumType target_scale) -> bool
  {
    return sqrt_prefix_impl(working_scale, target_scale);
  }

  auto sqrt_prefix_impl(NumType working_scale, NumType target_scale) -> bool
  {
    constexpr std::size_t cutoff = 128;
    constexpr auto maximum = std::numeric_limits<std::size_t>::max();
    auto const limbs = digits_.coefficient_limbs();
    auto const first_shift = std::size_t(working_scale - scale_);
    auto const second_shift = std::size_t(working_scale);
    // A storage-width overflow keeps the established resource/error path.
    if (limbs > maximum / base_log10_ || first_shift > maximum - second_shift) {
      return false;
    }
    auto const extra = first_shift + second_shift;
    auto const upper = limbs * base_log10_;
    if (upper > maximum - extra || upper + extra < 2 * cutoff - 1) {
      return false;
    }
    auto const width = digits_.coefficient_digits();
    if (width > maximum - extra) {
      return false;
    }
    auto const radicand_digits = width + extra;
    auto const root_digits = radicand_digits / 2 + radicand_digits % 2;
    if (root_digits < cutoff) {
      return false;
    }

    // Keep a good existing power-of-ten seed. In the original coefficient
    // domain its square is 10^e, where e is width or width-1. A tiny exact
    // residual means that full-precision Newton already starts with at least
    // half the required root digits. This is selection only, never stopping
    // or an approximation used as the returned result.
    if (width < base_) {
      auto const exponent = width - (width % 2 != std::size_t(scale_) % 2 ? 1 : 0);
      Details::BasicDigits<Traits> reference(1);
      grow_coefficient(reference, exponent);
      auto residual = digits_;
      // sub retains the absolute coefficient and separately reports its sign.
      static_cast<void>(residual.sub(reference, 0));
      bool precise = residual.is_zero();
      if (!precise) {
        auto const error_digits = residual.coefficient_digits();
        precise = exponent > error_digits &&
                  exponent - error_digits >= root_digits / 2 + root_digits % 2 + 1;
      }
      if (precise) {
        GD_BC_PROFILE_EVENT(Profile::Event::sqrt_precise_seed_fallbacks, 1);
        return false;
      }
    }

    // N=A*10^(2W-S) is an exact integer, represented symbolically by A and
    // the checked wide shift. Materialize only the coefficient needed by
    // each Newton request instead of retaining a full shifted radicand.
    BasicNumber current;
    // Plan backwards by ceiling-halves so a nearly complete stage is not
    // followed by another almost identical full-size division. There is at
    // most one stage per storage-index bit; the plan needs no heap allocation.
    std::array<std::size_t, std::numeric_limits<std::size_t>::digits + 1> stages{};
    std::size_t remaining = 0;
    for (auto precision = root_digits;; precision = precision / 2 + precision % 2) {
      assert(remaining < stages.size());  // NOLINT
      stages[remaining++] = precision;
      if (precision <= 8) {
        break;
      }
    }
    std::size_t previous_drop = 0;
    bool initial = true;
    while (remaining != 0) {
      auto const precision = stages[--remaining];
      auto const drop = root_digits - precision;
      // floor(sqrt(floor(N/10^(2q)))) equals floor(sqrt(N)/10^q).
      // Since q<R, 2q is at most the checked radicand width minus one.
      auto const removed = drop + drop;
      if (initial) {
        current = BasicNumber(1);
        grow_coefficient(current.digits_, (radicand_digits - drop - drop) / 2);
        initial = false;
      }
      else {
        grow_coefficient(current.digits_, previous_drop - drop);
      }
      GD_BC_PROFILE_EVENT(Profile::Event::sqrt_precision_stages, 1);
      BasicNumber previous;
      bool first = true;
      bool improving;
      do {
        previous = std::move(current);
        current.digits_ = digits_.rescaled_copy(extra, removed);
        current.scale_ = 0;
        current.sign_ = Sign::positive;
        GD_BC_PROFILE_EVENT(Profile::Event::sqrt_iterations, 1);
        GD_BC_PROFILE_EVENT(Profile::Event::sqrt_working_scale_sum, 0);
        GD_BC_PROFILE_EVENT(Profile::Event::sqrt_root_digits_sum, precision);
        current.divide(previous, 0);
        current.digits_.add(previous.digits_, 0);
        current.digits_.divide(2);
        // Every positive seed has a bounded first Newton step. Thereafter
        // stop on nondecrease, retaining the exact floor and avoiding cycles.
        improving = first ? previous != current : current < previous;
        first = false;
      } while (improving);
      current = std::move(previous);
      if (drop == 0) {
        break;
      }
      previous_drop = drop;
    }
    *this = std::move(current);
    scale_ = working_scale;
    if (scale_ > target_scale) {
      digits_.div_pow10(scale_ - target_scale);
      scale_ = target_scale;
    }
    return true;
  }

  static constexpr std::size_t radix_dc_cutoff = 65536;
  static constexpr std::size_t radix_dc_leaf = 64;
  using RadixPowers = std::map<std::size_t, BasicNumber>;

  static auto parse_radix_integer_range(std::string_view text, NumType radix, RadixPowers& powers)
    -> BasicNumber
  {
    if (text.size() <= radix_dc_leaf) {
      GD_BC_PROFILE_EVENT(Profile::Event::radix_dc_input_leaf_conversions, 1);
      return BasicNumber(text, radix);
    }
    auto const middle = text.size() / 2;
    auto left = parse_radix_integer_range(text.substr(0, middle), radix, powers);
    auto right = parse_radix_integer_range(text.substr(middle), radix, powers);
    // Zero prefixes need no radix power or product, including all-zero subtrees.
    if (left.is_zero())
      return right;
    auto const width = text.size() - middle;
    auto found = powers.find(width);
    if (found == powers.end()) {
      // The entry guard bounds every recursive exponent by NumType::max.
      BasicNumber power(radix);
      power.power(BasicNumber(static_cast<NumType>(width)), 0);
      found = powers.emplace(width, std::move(power)).first;
    }
    left.multiply(found->second, 0);
    left.add(right);
    return left;
  }

  static auto parse_radix_integer_dc(std::string_view text, NumType radix) -> BasicNumber
  {
    // Powers depend on this runtime radix and conversion only. No definition-
    // time input base or hidden cross-call cache is introduced.
    RadixPowers powers;
    return parse_radix_integer_range(text, radix, powers);
  }

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

  // Isolate the value-comparison wrapper from MSVC's surrounding VM inlining
  // choices, complementing the limb comparator's existing call boundary.
  // Other compilers retain their ordinary generic policy.
  [[nodiscard]]
#if defined(_MSC_VER)
  __declspec(noinline)
#endif
  auto compare(BasicNumber const& rhs) const -> Details::ComparisonResult
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

// Keep cached arithmetic outside VM template dispatch, including with IPO.
// Every other trait retains the same generic implementation above.
template<>
#if defined(_MSC_VER)
__declspec(noinline)
#elif __has_cpp_attribute(gnu::noinline)
[[gnu::noinline]]
#endif
auto BasicNumber<NumberTraits16>::try_divide_reusing(BasicNumber const& rhs, NumType target_scale,
                                                     DivisorCache& cache) -> bool;

template<>
#if defined(_MSC_VER)
__declspec(noinline)
#elif __has_cpp_attribute(gnu::noinline)
[[gnu::noinline]]
#endif
auto BasicNumber<NumberTraits16>::try_modulo_reusing(BasicNumber const& rhs, NumType target_scale,
                                                     DivisorCache& cache) -> bool;

template<>
#if defined(_MSC_VER)
__declspec(noinline)
#elif __has_cpp_attribute(gnu::noinline)
[[gnu::noinline]]
#endif
auto BasicNumber<NumberTraits32>::try_divide_reusing(BasicNumber const& rhs, NumType target_scale,
                                                     DivisorCache& cache) -> bool;

template<>
#if defined(_MSC_VER)
__declspec(noinline)
#elif __has_cpp_attribute(gnu::noinline)
[[gnu::noinline]]
#endif
auto BasicNumber<NumberTraits32>::try_modulo_reusing(BasicNumber const& rhs, NumType target_scale,
                                                     DivisorCache& cache) -> bool;

// Isolate the native remainder driver from instruction dispatch. Other traits
// keep the same generic implementation without a required compiler attribute.
template<>
#if defined(_MSC_VER)
__declspec(noinline)
#elif __has_cpp_attribute(gnu::noinline)
[[gnu::noinline]]
#endif
void BasicNumber<NumberTraits16>::modulo(BasicNumber rhs, NumType target_scale);

template<>
#if defined(_MSC_VER)
__declspec(noinline)
#elif __has_cpp_attribute(gnu::noinline)
[[gnu::noinline]]
#endif
void BasicNumber<NumberTraits32>::modulo(BasicNumber rhs, NumType target_scale);

// Isolate the complete native root, including its small-root fallback.
// Other traits retain the identical header-defined generic algorithm.
template<>
#if defined(_MSC_VER)
__declspec(noinline)
#elif __has_cpp_attribute(gnu::noinline)
[[gnu::noinline]]
#endif
void BasicNumber<NumberTraits16>::sqrt(NumType target_scale);

template<>
#if defined(_MSC_VER)
__declspec(noinline)
#elif __has_cpp_attribute(gnu::noinline)
[[gnu::noinline]]
#endif
void BasicNumber<NumberTraits32>::sqrt(NumType target_scale);

// Keep both native pointer-width choices out of callers, including with IPO
// where the compiler supports a noinline attribute. Other traits retain the
// same generic algorithm above without compiler-specific requirements.
template<>
#if defined(_MSC_VER)
__declspec(noinline)
#elif __has_cpp_attribute(gnu::noinline)
[[gnu::noinline]]
#endif
auto BasicNumber<NumberTraits16>::sqrt_prefix_if_large(NumType working_scale, NumType target_scale)
  -> bool;

template<>
#if defined(_MSC_VER)
__declspec(noinline)
#elif __has_cpp_attribute(gnu::noinline)
[[gnu::noinline]]
#endif
auto BasicNumber<NumberTraits32>::sqrt_prefix_if_large(NumType working_scale, NumType target_scale)
  -> bool;

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
