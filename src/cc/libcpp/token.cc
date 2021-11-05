/** \file   libcpp/token.cc
 *  \brief  Token class
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include "error.hh"
#include "token.hh"

GD::CPP::Token::Token(TokenType type, Range range) : range_(range), type_(type)  // NOLINT
{
  assert_ice(type_ != TokenType::character, "character tokens need a char32_t payload");
  assert_ice(type_ != TokenType::identifier, "identifier tokens need an identifier payload");
  assert_ice(type_ != TokenType::ppnumber, "PPnumber tokens need a PPnumber payload");
  assert_ice(!is_char_literal(), "character literals need a uint32_t payload.");
  assert_ice(!is_string_literal(), "String literals need a (Wide)?StringLiteralID payload.");
}

// NOLINTNEXTLINE
GD::CPP::Token::Token(TokenType type, Range range, char32_t c) : range_(range), type_(type)
{
  assert_ice(type_ == TokenType::character, "Only character tokens take a char32_t payload");
  payload_.c_ = c;  // NOLINT
}

// NOLINTNEXTLINE
GD::CPP::Token::Token(TokenType type, Range range, IdentID id) : range_(range), type_(type)
{
  assert_ice(type_ == TokenType::identifier, "Only identifier tokens take a identifier payload");
  payload_.identifier_ = id;  // NOLINT
}

// NOLINTNEXTLINE
GD::CPP::Token::Token(TokenType type, Range range, PPNumberID ppn) : range_(range), type_(type)
{
  assert_ice(type_ == TokenType::ppnumber, "Only PPNumber tokens take a PPNumber payload");
  payload_.ppnumber_ = ppn;  // NOLINT
}

// NOLINTNEXTLINE
GD::CPP::Token::Token(TokenType type, Range range, std::uint32_t c) : range_(range), type_(type)
{
  assert_ice(is_char_literal(), "Only Character literal tokens take a character literal payload");
  payload_.char_lit_ = c;  // NOLINT
}

// NOLINTNEXTLINE
GD::CPP::Token::Token(TokenType type, Range range, StringLiteralID lit_id)
    : range_(range), type_(type)
{
  assert_ice(type_ == TokenType::string_literal,
             "Only String literal tokens take a narrow string literal payload");
  payload_.str_lit_ = lit_id;  // NOLINT
}

// NOLINTNEXTLINE
GD::CPP::Token::Token(TokenType type, Range range, WideStringLiteralID lit_id)
    : range_(range), type_(type)
{
  assert_ice(is_string_literal() && type_ != TokenType::string_literal,
             "Only Wide string literal tokens take a wide string literal payload");
  payload_.wstr_lit_ = lit_id;  // NOLINT
}

auto GD::CPP::Token::type() const noexcept -> TokenType { return type_; }

auto GD::CPP::Token::is_char_literal() const noexcept -> bool
{
  return type_ == TokenType::char_literal || type_ == TokenType::char16_literal ||
         type_ == TokenType::char32_literal || type_ == TokenType::wchar_literal;
}

auto GD::CPP::Token::is_string_literal() const noexcept -> bool
{
  return type_ == TokenType::string_literal || type_ == TokenType::string8_literal ||
         type_ == TokenType::string16_literal || type_ == TokenType::string32_literal ||
         type_ == TokenType::wstring_literal;
}

auto GD::CPP::Token::range() const noexcept -> Range { return range_; }

auto GD::CPP::Token::character() const noexcept -> char32_t
{
  assert_ice(type() == TokenType::character, "character() can only be called on character Tokens.");
  return payload_.c_;  // NOLINT
}

auto GD::CPP::Token::identifier() const noexcept -> IdentID
{
  assert_ice(type() == TokenType::identifier,
             "identifier() can only be called on identifier Tokens.");
  return payload_.identifier_;  // NOLINT
}

auto GD::CPP::Token::ppnumber() const noexcept -> PPNumberID
{
  assert_ice(type() == TokenType::ppnumber, "ppnumber() can only be called on PPNumber Tokens.");
  return payload_.ppnumber_;  // NOLINT
}

auto GD::CPP::Token::char_literal() const noexcept -> std::uint32_t
{
  assert_ice(is_char_literal(), "char_literal() can only be called on character literals.");
  return payload_.char_lit_;  // NOLINT
}

auto GD::CPP::Token::string_literal() const noexcept -> StringLiteralID
{
  assert_ice(type_ == TokenType::string_literal,
             "string_literal() can only be called on narrow string literals.");
  return payload_.str_lit_;  // NOLINT
}

auto GD::CPP::Token::wide_string_literal() const noexcept -> WideStringLiteralID
{
  assert_ice(is_string_literal() && type_ != TokenType::string_literal,
             "wide_string_literal() can only be called on wide string literals.");
  return payload_.wstr_lit_;  // NOLINT
}

auto GD::CPP::operator==(Token const& token, TokenType type) noexcept -> bool
{
  return token.type() == type;
}

auto GD::CPP::operator==(TokenType type, Token const& token) noexcept -> bool
{
  return token.type() == type;
}

auto GD::CPP::operator==(Token const& token, char32_t c) noexcept -> bool
{
  return token.type() == TokenType::character && token.character() == c;
}

auto GD::CPP::operator==(char32_t c, Token const& token) noexcept -> bool
{
  return token.type() == TokenType::character && token.character() == c;
}
