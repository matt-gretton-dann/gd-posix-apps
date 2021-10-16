/** \file   libcpp/include/token.hh
 *  \brief  Token class
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#ifndef CC_LIBCPP_TOKEN_HH_INCLUDED_
#define CC_LIBCPP_TOKEN_HH_INCLUDED_

#include "gd/format.hh"

#include <cstdint>
#include <map>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "error.hh"
#include "location.hh"

namespace GD::CPP {

/** \brief  Token type */
enum class TokenType {
  end_of_source,   ///< End of all source
  end_of_include,  ///< End of a particular file (main file is included from command line)
  error,           ///< Error (stores an Error)
  character        ///< Character
};

/** \brief  A token
 *
 * A token is effectively a discriminated union of values.  The \a type says what is in the union.
 * All tokens have a range, which maps to the source code location they come from.
 *
 * Use get() to access token loads
 *
 * Valid token types and the union contents are:
 *
 * | TokenType      | Data load |  Contents                                                        |
 * | :------------- | :-------- | :---------                                                       |
 * | end_of_source  |           | End of all sources                                               |
 * | end_of_include |           | End of the current source file.                                  |
 * | error          | Error     | Error                                                            |
 * | character      |           | A character, use the range to get the character represented.     |
 */
class Token
{
public:
  /** \brief       Construct a token with no payload
   *  \param type  Token type
   *  \param range Source code range for the token.
   */
  Token(TokenType type, Range range);

  /** \brief       Construct a token for the Error token type
   *  \param type  Token type (TokenType::error)
   *  \param range Source code range for the token.
   */
  Token(TokenType type, Range range, Error contents);

  /** \brief  Get the token type.  */
  auto type() const noexcept -> TokenType;

  /** \brief  Get the token range.  */
  auto range() const noexcept -> Range;

  /** \brief  Get the payload for the token.
   *  \tparam Payload type
   *  \return Payload
   */
  template<typename FieldT>
  auto get() const -> FieldT const&
  {
    return std::get<FieldT>(contents_);
  }

private:
  TokenType type_;                                ///< Token type
  Range range_;                                   ///< Range
  std::variant<std::nullopt_t, Error> contents_;  ///< Payload
};

auto operator==(Token const& token, TokenType type) noexcept -> bool;
auto operator==(TokenType type, Token const& token) noexcept -> bool;

}  // namespace GD::CPP

/** \brief  Format a token (mostly for debug purposes.
 */
template<>
struct fmt::formatter<GD::CPP::Token>
{
  constexpr auto parse(format_parse_context& ctx)
  {
    auto it = ctx.begin();
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
    case GD::CPP::TokenType::error: {
      auto const& error = token.get<GD::CPP::Error>();
      return vformat_to(ctx.out(), "ERROR({0}, {1})",
                        fmt::make_format_args(error.id(), error.message()));
    }
    case GD::CPP::TokenType::character:
      return vformat_to(ctx.out(), "<Character>", fmt::make_format_args());
    default:
      return vformat_to(ctx.out(), "UNKNOWN", fmt::make_format_args());
    }
  }
};

#endif  // CC_LIBCPP_TOKEN_HH_INCLUDED_
