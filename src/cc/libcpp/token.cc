/** \file   libcpp/token.cc
 *  \brief  Token class
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "token.hh"

#include "error.hh"

GD::CPP::Token::Token(TokenType type, Range range)
    : type_(type), range_(range), contents_(std::nullopt)
{
  assert_ice(type != TokenType::error, "TokenType::error needs contents");
}

GD::CPP::Token::Token(TokenType type, Range range, Error contents)
    : type_(type), range_(range), contents_(contents)
{
  assert_ice(type == TokenType::error, "Only TokenType::error takes Error as its contents");
}

auto GD::CPP::Token::type() const noexcept -> TokenType { return type_; }

auto GD::CPP::Token::range() const noexcept -> Range { return range_; }

auto GD::CPP::operator==(Token const& token, TokenType type) noexcept -> bool
{
  return token.type() == type;
}

auto GD::CPP::operator==(TokenType type, Token const& token) noexcept -> bool
{
  return token.type() == type;
}
