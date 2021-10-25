/** \file   libcpp/include/token.hh
 *  \brief  Token class
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef CC_LIBCPP_TOKEN_HH_INCLUDED_
#define CC_LIBCPP_TOKEN_HH_INCLUDED_

#include "gd/format.hh"

#include <cstdint>
#include <locale>
#include <map>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "error.hh"
#include "identifier-manager.hh"
#include "location.hh"
#include "ppnumber-manager.hh"

namespace GD::CPP {

/** \brief  Token type */
enum class TokenType {
  end_of_source,   ///< End of all source
  end_of_include,  ///< End of a particular file (main file is included from command line)
  character,       ///< Character
  white_space,     ///< White space
  identifier,      ///< An identifier
  ppnumber,        ///< A PPNumber
};

/** \brief  A token
 *
 * A token is effectively a discriminated union of values.  The \a type says what is in the union.
 * All tokens have a range, which maps to the source code location they come from.
 *
 * Valid token types and the union contents are:
 *
 * | TokenType      | Payload    |  Contents                                                       |
 * | :------------- | :--------- | :---------                                                      |
 * | end_of_source  |            | End of all sources                                              |
 * | end_of_include |            | End of the current source file.                                 |
 * | character      | char32_t   | A character.                                                    |
 * | white_space    |            | White space.                                                    |
 * | identifier     | IdentID    | Identifier.                                                     |
 * | ppnumber       | PPNumberID | PPNumber.                                                       |
 */
class Token  // NOLINT
{
public:
  /** \brief       Construct a token with no payload
   *  \param type  Token type
   *  \param range Source code range for the token.
   */
  Token(TokenType type, Range range);

  /** \brief       Construct a token with a Unicode character payload
   *  \param type  Token type(TokenType::character)
   *  \param range Source code range for the tokne.
   *  \param c     Character the token represents.
   */
  Token(TokenType type, Range range, char32_t c);

  /** \brief       Construct a token with an Identifier
   *  \param type  Token type(TokenType::identifier)
   *  \param range Source code range for the token.
   *  \param id    Identifier the token represents
   */
  Token(TokenType type, Range range, IdentID id);

  /** \brief       Construct a token with a PPNumber
   *  \param type  Token type(TokenType::ppnumber)
   *  \param range Source code range for the token.
   *  \param ppn   PPNumber the token represents
   */
  Token(TokenType type, Range range, PPNumberID ppn);

  /** \brief  Get the token type.  */
  [[nodiscard]] auto type() const noexcept -> TokenType;

  /** \brief  Get the token range.  */
  [[nodiscard]] auto range() const noexcept -> Range;

  /** \brief  Get the character (if type() == TokenType::character).  */
  [[nodiscard]] auto character() const noexcept -> char32_t;

  /** \brief  Get the identifier (if type() == TokenType::identifier).  */
  [[nodiscard]] auto identifier() const noexcept -> IdentID;

  /** \brief  Get the PPNumber (if type() == TokenType::ppnumber).  */
  [[nodiscard]] auto ppnumber() const noexcept -> PPNumberID;

private:
  Range range_;     ///< Range
  TokenType type_;  ///< Token type
  union
  {
    char32_t c_;           ///< Character
    IdentID identifier_;   ///< Identifier
    PPNumberID ppnumber_;  ///< PPNumber
  } payload_;
};

auto operator==(Token const& token, TokenType type) noexcept -> bool;
auto operator==(TokenType type, Token const& token) noexcept -> bool;
auto operator==(Token const& token, char32_t c) noexcept -> bool;
auto operator==(char32_t c, Token const& token) noexcept -> bool;

}  // namespace GD::CPP

/** \brief  Format a token (mostly for debug purposes.
 */
template<>
struct fmt::formatter<GD::CPP::Token>
{
  static constexpr auto parse(format_parse_context& ctx)
  {
    auto it = ctx.begin();  // NOLINT
    for (; it != ctx.end(); ++it) {
      switch (*it) {
      case '}':
        return it;
      default:
        assert_ice(false, "Invalid parse format.");
      }
    }

    if (it != ctx.end()) {
      assert_ice(false, "Invalid parse format - not terminated.");
    }

    return it;
  }

  template<typename FormatContext>
  auto format(GD::CPP::Token const& token, FormatContext& ctx)
  {
    switch (token.type()) {
    case GD::CPP::TokenType::end_of_source:
      return vformat_to(ctx.out(), "<end-of-source>", fmt::make_format_args());
    case GD::CPP::TokenType::end_of_include:
      return vformat_to(ctx.out(), "<end-of-include>", fmt::make_format_args());
    case GD::CPP::TokenType::character:
      return vformat_to(ctx.out(), "{0}",
                        fmt::make_format_args(GD::CPP::to_string(token.character())));
    case GD::CPP::TokenType::white_space:
      return vformat_to(ctx.out(), " ", fmt::make_format_args());
    case GD::CPP::TokenType::identifier:
      return vformat_to(ctx.out(), "IDENTIFIER({0})", fmt::make_format_args(token.identifier()));
    case GD::CPP::TokenType::ppnumber:
      return vformat_to(ctx.out(), "PPNUMBER({0})", fmt::make_format_args(token.identifier()));
    default:
      return vformat_to(ctx.out(), "UNKNOWN", fmt::make_format_args());
    }
  }
};

#endif  // CC_LIBCPP_TOKEN_HH_INCLUDED_
