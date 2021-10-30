/** \file   libcpp/include/character-classifiers.hh
 *  \brief  Basic internal functions to classify characters
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef LIBCPP_INCLUDE_CHARACTER_CLASSIFIERS_HH_
#define LIBCPP_INCLUDE_CHARACTER_CLASSIFIERS_HH_

#include <limits>

namespace GD::CPP::Details {

/** \brief  Is \a c classified as a whitespace character? */
constexpr inline auto is_whitespace(char32_t c) -> bool
{
  return c == U' ' || c == U'\t' || c == U'\f' || c == U'\v';
}

/** \brief  Is this a valid UCN character? */
constexpr inline auto is_ucn(char32_t c) -> bool
{
  return c == U'\u0024' || c == U'\u0040' || c == U'\u0060' || (c >= U'\u00a0' && c <= U'\ud7ff') ||
         c >= U'\ue000';
}

/** \brief  Is this a valid Universal Character name identifier?  */
constexpr inline auto is_ucn_identifier(char32_t c) -> bool
{
  return c == U'\u00A8' || c == U'\u00AA' || c == U'\u00AD' || c == U'\u00AF' ||
         (c >= U'\u00B2' && c <= U'\u00B5') || (c >= U'\u00B7' && c <= U'\u00BA') ||
         (c >= U'\u00BC' && c <= U'\u00BE') || (c >= U'\u00C0' && c <= U'\u00D6') ||
         (c >= U'\u00D8' && c <= U'\u00F6') || (c >= U'\u00F8' && c <= U'\u00FF') ||
         (c >= U'\u0100' && c <= U'\u167F') || (c >= U'\u1681' && c <= U'\u180D') ||
         (c >= U'\u180F' && c <= U'\u1FFF') || (c >= U'\u200B' && c <= U'\u200D') ||
         (c >= U'\u202A' && c <= U'\u202E') || (c >= U'\u203F' && c <= U'\u2040') ||
         c == U'\u2054' || (c >= U'\u2060' && c <= U'\u206F') ||
         (c >= U'\u2070' && c <= U'\u218F') || (c >= U'\u2460' && c <= U'\u24FF') ||
         (c >= U'\u2776' && c <= U'\u2793') || (c >= U'\u2C00' && c <= U'\u2DFF') ||
         (c >= U'\u2E80' && c <= U'\u2FFF') || (c >= U'\u3004' && c <= U'\u3007') ||
         (c >= U'\u3021' && c <= U'\u302F') || (c >= U'\u3031' && c <= U'\u303F') ||
         (c >= U'\u3040' && c <= U'\uD7FF') || (c >= U'\uF900' && c <= U'\uFD3D') ||
         (c >= U'\uFD40' && c <= U'\uFDCF') || (c >= U'\uFDF0' && c <= U'\uFE44') ||
         (c >= U'\uFE47' && c <= U'\uFFFD') || (c >= U'\U00010000' && c <= U'\U0001FFFD') ||
         (c >= U'\U00020000' && c <= U'\U0002FFFD') || (c >= U'\U00030000' && c <= U'\U0003FFFD') ||
         (c >= U'\U00040000' && c <= U'\U0004FFFD') || (c >= U'\U00050000' && c <= U'\U0005FFFD') ||
         (c >= U'\U00060000' && c <= U'\U0006FFFD') || (c >= U'\U00070000' && c <= U'\U0007FFFD') ||
         (c >= U'\U00080000' && c <= U'\U0008FFFD') || (c >= U'\U00090000' && c <= U'\U0009FFFD') ||
         (c >= U'\U000A0000' && c <= U'\U000AFFFD') || (c >= U'\U000B0000' && c <= U'\U000BFFFD') ||
         (c >= U'\U000C0000' && c <= U'\U000CFFFD') || (c >= U'\U000D0000' && c <= U'\U000DFFFD') ||
         (c >= U'\U000E0000' && c <= U'\U000EFFFD');
}

/** \brief  Is this a valid Universal Character name initial identifier?  */
constexpr inline auto is_ucn_initial_identifier(char32_t c) -> bool
{
  bool is_id = is_ucn_identifier(c);
  bool disallowed_first = (c >= U'\u0300' && c <= U'\u036F') ||
                          (c >= U'\u1DC0' && c <= U'\u1DFF') ||
                          (c >= U'\u20D0' && c <= U'\u20FF') || (c >= U'\uFE20' && c <= U'\uFE2F');
  return is_id && !disallowed_first;
}

/** \brief Is this a non-digit?  */
constexpr inline auto is_nondigit(char32_t c) -> bool
{
  /* This range check is safe because char32_t is a Unicode encoded set where A-Z & a-z are
   * contiguous.  */
  return c == '_' || (c >= U'A' && c <= U'Z') || (c >= U'a' && c <= u'z');
}

/** \brief Is this a digit?  */
constexpr inline auto is_digit(char32_t c) -> bool
{
  /* This range check is safe because char32_t is a Unicode encoded set where A-Z & a-z are
   * contiguous.  */
  return c >= U'0' && c <= U'9';
}

constexpr inline auto is_octal_digit(char32_t c) -> bool { return (c >= U'0' && c <= U'7'); }

constexpr inline auto is_hex_digit(char32_t c) -> bool
{
  return (c >= U'0' && c <= U'9') || (c >= U'A' && c <= U'F') || (c >= U'a' && c <= U'f');
}

constexpr inline auto hex_digit_value(char32_t c) -> unsigned
{
  constexpr unsigned offset = 10;
  if (c >= U'0' && c <= U'9') {
    return c - U'0';
  }
  if (c >= U'A' && c <= U'F') {
    return c - U'A' + offset;
  }
  if (c >= U'a' && c <= U'f') {
    return c - U'a' + offset;
  }

  return std::numeric_limits<unsigned>::max();
}

constexpr inline auto is_sign(char32_t c) -> bool { return c == U'+' || c == U'-'; }

}  // namespace GD::CPP::Details

#endif  // LIBCPP_INCLUDE_CHARACTER_CLASSIFIERS_HH_
