/** \file   lexer.cc
 *  \brief  Implementation of GD::Bc::Lexer and derived classes
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "util/utils.hh"

#include "awk-messages.hh"

#include <cstdint>
#include <memory>
#include <optional>
#include <string_view>
#include <unordered_map>

#include "awk.hh"

GD::Awk::Lexer::Lexer(std::unique_ptr<Reader>&& r) : r_(std::move(r)), t_(std::nullopt) {}

auto GD::Awk::Lexer::peek() -> GD::Awk::Token const&
{
  if (!t_.has_value()) {
    lex();
  }

  assert(t_.has_value());  // NOLINT
  return *t_;
}

void GD::Awk::Lexer::chew()
{
  /* If we have nothing to chew we need to find something.  */
  if (!t_.has_value()) {
    lex();
  }

  assert(t_.has_value());  // NOLINT
  if (t_->type() != Token::Type::eof) {
    t_.reset();
  }
}

auto GD::Awk::Lexer::location() const -> GD::Awk::Location const& { return r_->location(); }

void GD::Awk::Lexer::lex()
{
  switch (r_->peek()) {
  case EOF:
    t_.emplace(Token::Type::eof);
    return;
  case '\n': /*
     r_->chew();
     t_.emplace(Token::Type::newline);
     return;*/
  default:
    std::abort();
    ;
    t_.emplace(Token::Type::error, r_->error(Msg::unexpected_token, r_->peek()));
    return;
  }
}
