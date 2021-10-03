/** \file   token.cc
 *  \brief  Implementation of GD::BC::Token
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "util/utils.hh"

#include <cassert>
#include <iostream>

#include "bc.hh"

GD::Bc::Letter::Letter(char l) : letter_(encode(l)) {}
GD::Bc::Letter::operator unsigned() const { return letter_; }
auto GD::Bc::Letter::operator==(Letter l) const -> bool { return letter_ == l.letter_; }
auto GD::Bc::Letter::operator==(char l) const -> bool { return letter_ == encode(l); }

auto GD::Bc::operator<<(std::ostream& os, GD::Bc::Letter l) -> std::ostream&
{
  static std::string_view letters = "abcdefghijklmnopqrstuvwxyz";
  os << letters[static_cast<unsigned>(l)];
  return os;
}

auto GD::Bc::operator!=(Letter lhs, Letter rhs) -> bool { return !(lhs == rhs); }
auto GD::Bc::operator!=(Letter lhs, char rhs) -> bool { return !(lhs == rhs); }
auto GD::Bc::operator!=(char lhs, Letter rhs) -> bool { return !(rhs == lhs); }

auto GD::Bc::Letter::encode(char l) -> unsigned
{
  /* Can't assume letters are contiguous code-points.  */
  std::string_view letters = "abcdefghijklmnopqrstuvwxyz";
  auto it = letters.find(l);
  assert(it != std::string_view::npos);  // NOLINT
  return static_cast<unsigned>(it);
}

GD::Bc::Token::Token(Type type) : value_(type)
{
  assert(type != Type::number);  // NOLINT
  assert(type != Type::string);  // NOLINT
  assert(type != Type::letter);  // NOLINT
  assert(type != Type::error);   // NOLINT
}

GD::Bc::Token::Token(Type type, std::string const& s) : value_(s)
{
  assert(type == Type::string || type == Type::number || type == Type::error);  // NOLINT
  if (type == Type::number) {
    value_ = NumInt{s};
  }
  else if (type == Type::error) {
    value_ = ErrorInt{s};
  }
}

GD::Bc::Token::Token([[maybe_unused]] Type type, Letter l) : value_(l)
{
  assert(type == Type::letter);  // NOLINT
}

auto GD::Bc::Token::type() const -> GD::Bc::Token::Type
{
  return std::visit(GD::Overloaded{[](Type t) { return t; },
                                   [](std::string const& /*unused*/) { return Type::string; },
                                   [](NumInt const& /*unused*/) { return Type::number; },
                                   [](Letter /*unused*/) { return Type::letter; },
                                   [](ErrorInt const& /*unused*/) { return Type::error; },
                                   [](auto /*unused*/) {
                                     assert(false);
                                     return Type::eof;
                                   }},
                    value_);
}

auto GD::Bc::Token::string() const -> std::string const& { return std::get<std::string>(value_); }

auto GD::Bc::Token::number() const -> std::string const& { return std::get<NumInt>(value_).get(); }

auto GD::Bc::Token::error() const -> std::string const& { return std::get<ErrorInt>(value_).get(); }

auto GD::Bc::Token::letter() const -> GD::Bc::Letter { return std::get<Letter>(value_); }

auto GD::Bc::operator<<(std::ostream& os, Token::Type t) -> std::ostream&
{
  switch (t) {
  case Token::Type::error:
    os << "error";
    break;
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
  case Token::Type::halt:
    os << "halt";
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
  case Token::Type::abs:
    os << "abs";
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
    abort();
  }
  return os;
}

void GD::Bc::Token::Token::debug(std::ostream& os) const
{
  os << "Token::";
  std::visit(GD::Overloaded{[&os](Type t) { os << t; },
                            [&os](ErrorInt const& e) { os << "error(" << e.get() << ")"; },
                            [&os](NumInt const& n) { os << "number(" << n.get() << ")"; },
                            [&os](std::string const& s) { os << "string(" << s << ")"; },
                            [&os](Letter l) { os << "letter(" << l << ")"; }},
             value_);
}

auto GD::Bc::operator<<(std::ostream& os, GD::Bc::Token const& token) -> std::ostream&
{
  switch (token.type()) {
  case Token::Type::error:
    os << "ERROR(" << token.error() << ")";
    break;
  case Token::Type::eof:
    os << "EOF";
    break;
  case Token::Type::newline:
    os << "NEWLINE";
    break;
  case Token::Type::string:
    os << '"' << token.string() << '"';
    break;
  case Token::Type::letter:
    os << token.letter();
    break;
  case Token::Type::number:
    os << token.number();
    break;
  case Token::Type::power:
    os << '^';
    break;
  case Token::Type::multiply:
    os << '*';
    break;
  case Token::Type::divide:
    os << '/';
    break;
  case Token::Type::modulo:
    os << '%';
    break;
  case Token::Type::add:
    os << '+';
    break;
  case Token::Type::subtract:
    os << '-';
    break;
  case Token::Type::assign:
    os << '=';
    break;
  case Token::Type::add_assign:
    os << "+=";
    break;
  case Token::Type::subtract_assign:
    os << "-=";
    break;
  case Token::Type::multiply_assign:
    os << "*=";
    break;
  case Token::Type::divide_assign:
    os << "/=";
    break;
  case Token::Type::modulo_assign:
    os << "%=";
    break;
  case Token::Type::power_assign:
    os << "^=";
    break;
  case Token::Type::equals:
    os << "==";
    break;
  case Token::Type::less_than_equals:
    os << "<=";
    break;
  case Token::Type::greater_than_equals:
    os << ">=";
    break;
  case Token::Type::not_equals:
    os << "!=";
    break;
  case Token::Type::less_than:
    os << '<';
    break;
  case Token::Type::greater_than:
    os << '>';
    break;
  case Token::Type::increment:
    os << "++";
    break;
  case Token::Type::decrement:
    os << "--";
    break;
  case Token::Type::define:
    os << "define";
    break;
  case Token::Type::break_:
    os << "break";
    break;
  case Token::Type::halt:
    os << "halt";
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
  case Token::Type::abs:
    os << "abs";
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
    os << '(';
    break;
  case Token::Type::rparens:
    os << ')';
    break;
  case Token::Type::lsquare:
    os << '[';
    break;
  case Token::Type::rsquare:
    os << ']';
    break;
  case Token::Type::lbrace:
    os << '{';
    break;
  case Token::Type::rbrace:
    os << '}';
    break;
  case Token::Type::comma:
    os << ',';
    break;
  case Token::Type::semicolon:
    os << ';';
    break;
  default:
    abort();
  }
  return os;
}

auto GD::Bc::Token::is_assign_op() const -> bool
{
  return type() == Type::assign || type() == Type::add_assign || type() == Type::subtract_assign ||
         type() == Type::multiply_assign || type() == Type::divide_assign ||
         type() == Type::modulo_assign || type() == Type::power_assign;
}

auto GD::Bc::Token::is_incr_decr_op() const -> bool
{
  return type() == Type::increment || type() == Type::decrement;
}

auto GD::Bc::Token::is_mul_op() const -> bool
{
  return type() == Type::multiply || type() == Type::divide || type() == Type::modulo;
}

auto GD::Bc::Token::is_add_op() const -> bool
{
  return type() == Type::add || type() == Type::subtract;
}

auto GD::Bc::Token::is_rel_op() const -> bool
{
  return type() == Type::equals || type() == Type::less_than_equals ||
         type() == Type::greater_than_equals || type() == Type::not_equals ||
         type() == Type::less_than || type() == Type::greater_than;
}

auto GD::Bc::operator==(Token const& token, Token::Type type) -> bool
{
  return token.type() == type;
}
auto GD::Bc::operator==(Token::Type type, Token const& token) -> bool { return token == type; }
auto GD::Bc::operator!=(Token const& token, Token::Type type) -> bool { return !(token == type); }
auto GD::Bc::operator!=(Token::Type type, Token const& token) -> bool { return !(token == type); }
