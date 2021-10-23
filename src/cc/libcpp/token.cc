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
}

// NOLINTNEXTLINE
GD::CPP::Token::Token(TokenType type, Range range, char32_t c) : range_(range), type_(type)
{
  assert_ice(type_ == TokenType::character, "Only character tokens take a char32_t payload");
  payload_.c_ = c;  // NOLINT
}

auto GD::CPP::Token::type() const noexcept -> TokenType { return type_; }

auto GD::CPP::Token::range() const noexcept -> Range { return range_; }

auto GD::CPP::Token::character() const noexcept -> char32_t
{
  assert_ice(type() == TokenType::character, "character() can only be called on character Tokens.");
  return payload_.c_;  // NOLINT
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
