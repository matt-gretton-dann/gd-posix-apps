/** \file   token.cc
 *  \brief  Implementation of GD::BC::Token
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "util/utils.hh"

#include <assert.h>
#include <iostream>

#include "bc.hh"

GD::Bc::Token::Token(Type type) : value_(type)
{
  assert(type != Type::number);
  assert(type != Type::string);
  assert(type != Type::letter);
}

GD::Bc::Token::Token(Type type, std::string const& s) : value_(s)
{
  assert(type == Type::string || type == Type::number);
  if (type == Type::number) {
    value_ = NumInt{s};
  }
}

GD::Bc::Token::Token(Type type, char l) : value_(l) { assert(type == Type::letter); }

GD::Bc::Token::Type GD::Bc::Token::type() const
{
  return std::visit(
    GD::Overloaded{[](Type t) { return t; }, [](std::string const&) { return Type::string; },
                   [](NumInt const&) { return Type::number; }, [](char) { return Type::letter; },
                   [](auto) {
                     assert(false);
                     return Type::eof;
                   }},
    value_);
}

std::string const& GD::Bc::Token::string() const { return std::get<std::string>(value_); }

std::string const& GD::Bc::Token::number() const { return std::get<NumInt>(value_).num_; }

char GD::Bc::Token::letter() const { return std::get<char>(value_); }

std::ostream& GD::Bc::operator<<(std::ostream& os, Token::Type t)
{
  switch (t) {
  case Token::Type::eof:
    os << "eof";
    break;
  case Token::Type::newline:
    os << "newline";
    break;
  case Token::Type::string:
    os << "string";
    break;
  case Token::Type::letter:
    os << "letter";
    break;
  case Token::Type::number:
    os << "number";
    break;
  case Token::Type::power:
    os << "power";
    break;
  case Token::Type::multiply:
    os << "multiply";
    break;
  case Token::Type::divide:
    os << "divide";
    break;
  case Token::Type::modulo:
    os << "modulo";
    break;
  case Token::Type::add:
    os << "add";
    break;
  case Token::Type::subtract:
    os << "subtract";
    break;
  case Token::Type::assign:
    os << "assign";
    break;
  case Token::Type::add_assign:
    os << "add_assign";
    break;
  case Token::Type::subtract_assign:
    os << "subtract_assign";
    break;
  case Token::Type::multiply_assign:
    os << "multiply_assign";
    break;
  case Token::Type::divide_assign:
    os << "divide_assign";
    break;
  case Token::Type::modulo_assign:
    os << "modulo_assign";
    break;
  case Token::Type::power_assign:
    os << "power_assign";
    break;
  case Token::Type::equals:
    os << "equals";
    break;
  case Token::Type::less_than_equals:
    os << "less_than_equals";
    break;
  case Token::Type::greater_than_equals:
    os << "greater_than_equals";
    break;
  case Token::Type::not_equals:
    os << "not_equals";
    break;
  case Token::Type::less_than:
    os << "less_than";
    break;
  case Token::Type::greater_than:
    os << "greater_than";
    break;
  case Token::Type::increment:
    os << "increment";
    break;
  case Token::Type::decrement:
    os << "decrement";
    break;
  case Token::Type::define:
    os << "define";
    break;
  case Token::Type::break_:
    os << "break";
    break;
  case Token::Type::quit:
    os << "quit";
    break;
  case Token::Type::length:
    os << "length";
    break;
  case Token::Type::return_:
    os << "return";
    break;
  case Token::Type::for_:
    os << "for";
    break;
  case Token::Type::if_:
    os << "if";
    break;
  case Token::Type::while_:
    os << "while";
    break;
  case Token::Type::sqrt:
    os << "sqrt";
    break;
  case Token::Type::scale:
    os << "scale";
    break;
  case Token::Type::ibase:
    os << "ibase";
    break;
  case Token::Type::obase:
    os << "obase";
    break;
  case Token::Type::auto_:
    os << "auto";
    break;
  case Token::Type::lparens:
    os << "lparens";
    break;
  case Token::Type::rparens:
    os << "rparens";
    break;
  case Token::Type::lsquare:
    os << "lsquare";
    break;
  case Token::Type::rsquare:
    os << "rsquare";
    break;
  case Token::Type::lbrace:
    os << "lbrace";
    break;
  case Token::Type::rbrace:
    os << "rbrace";
    break;
  case Token::Type::comma:
    os << "comma";
    break;
  case Token::Type::semicolon:
    os << "semicolon";
    break;
  default:
    assert(false);
  }
  return os;
}

void GD::Bc::Token::Token::debug(std::ostream& os) const
{
  os << "Token::";
  std::visit(GD::Overloaded{[&os](Type t) { os << t; },
                            [&os](NumInt const& n) { os << "number(" << n.num_ << ")"; },
                            [&os](std::string const& s) { os << "string(" << s << ")"; },
                            [&os](char l) { os << "letter(" << l << ")"; }},
             value_);
}