/** \file   token.cc
 *  \brief  Implementation of GD::Awk::Token
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "util/utils.hh"

#include <cassert>
#include <iostream>

#include "awk.hh"

GD::Awk::Token::Token(Type type) : value_(type)
{
  assert(type != Type::number);             // NOLINT
  assert(type != Type::name);               // NOLINT
  assert(type != Type::func_name);          // NOLINT
  assert(type != Type::builtin_func_name);  // NOLINT
  assert(type != Type::ere);                // NOLINT
  assert(type != Type::error);              // NOLINT
}

GD::Awk::Token::Token(Type type, std::string const& s) : value_(Name(s))
{
  assert(type == Type::name || type == Type::func_name || type == Type::ere ||
         type == Type::number || type == Type::error);  // NOLINT
  if (type == Type::number) {
    value_ = Number{s};
  }
  else if (type == Type::func_name) {
    value_ = FuncName{s};
  }
  else if (type == Type::ere) {
    value_ = ERE{s};
  }
  else if (type == Type::error) {
    value_ = Error{s};
  }
}

GD::Awk::Token::Token([[maybe_unused]] Type type, BuiltinFunc func) : value_(func)
{
  assert(type == Type::builtin_func_name);
}

auto GD::Awk::Token::type() const -> Token::Type
{
  return std::visit(GD::Overloaded{[](Type t) { return t; },
                                   [](Name const& /*unused*/) { return Type::name; },
                                   [](std::string const& /*unused*/) { return Type::string; },
                                   [](FuncName const& /*unused*/) { return Type::func_name; },
                                   [](BuiltinFunc /*unused*/) { return Type::builtin_func_name; },
                                   [](Number const& /*unused*/) { return Type::number; },
                                   [](ERE const& /*unused*/) { return Type::ere; },
                                   [](Error const& /*unused*/) { return Type::error; }},
                    value_);
}

auto GD::Awk::Token::name() const -> std::string const& { return std::get<Name>(value_).get(); }

auto GD::Awk::Token::string() const -> std::string const& { return std::get<std::string>(value_); }

auto GD::Awk::Token::func_name() const -> std::string const&
{
  return std::get<FuncName>(value_).get();
}

auto GD::Awk::Token::builtin_func_name() const -> BuiltinFunc
{
  return std::get<BuiltinFunc>(value_);
}

auto GD::Awk::Token::ere() const -> std::string const& { return std::get<ERE>(value_).get(); }

auto GD::Awk::Token::number() const -> std::string const& { return std::get<Number>(value_).get(); }

auto GD::Awk::Token::error() const -> std::string const& { return std::get<Error>(value_).get(); }

auto GD::Awk::operator<<(std::ostream& os, Token::Type t) -> std::ostream&
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
  case Token::Type::name:
    os << "name";
    break;
  case Token::Type::string:
    os << "string";
    break;
  case Token::Type::func_name:
    os << "func_name";
    break;
  case Token::Type::builtin_func_name:
    os << "builtin_func_name";
    break;
  case Token::Type::number:
    os << "number";
    break;
  case Token::Type::ere:
    os << "ere";
    break;
  case Token::Type::begin:
    os << "begin";
    break;
  case Token::Type::break_:
    os << "break";
    break;
  case Token::Type::continue_:
    os << "continue";
    break;
  case Token::Type::delete_:
    os << "delete";
    break;
  case Token::Type::do_:
    os << "do";
    break;
  case Token::Type::else_:
    os << "else";
    break;
  case Token::Type::end:
    os << "end";
    break;
  case Token::Type::exit:
    os << "exit";
    break;
  case Token::Type::for_:
    os << "for";
    break;
  case Token::Type::function:
    os << "function";
    break;
  case Token::Type::getline:
    os << "getline";
    break;
  case Token::Type::if_:
    os << "if";
    break;
  case Token::Type::in:
    os << "in";
    break;
  case Token::Type::next:
    os << "next";
    break;
  case Token::Type::print:
    os << "print";
    break;
  case Token::Type::printf:
    os << "printf";
    break;
  case Token::Type::return_:
    os << "return";
    break;
  case Token::Type::while_:
    os << "while";
    break;
  case Token::Type::add_assign:
    os << "add_assign";
    break;
  case Token::Type::sub_assign:
    os << "sub_assign";
    break;
  case Token::Type::mul_assign:
    os << "mul_assign";
    break;
  case Token::Type::div_assign:
    os << "div_assign";
    break;
  case Token::Type::mod_assign:
    os << "mod_assign";
    break;
  case Token::Type::pow_assign:
    os << "pow_assign";
    break;
  case Token::Type::or_:
    os << "or";
    break;
  case Token::Type::and_:
    os << "and";
    break;
  case Token::Type::no_match:
    os << "no_match";
    break;
  case Token::Type::eq:
    os << "eq";
    break;
  case Token::Type::le:
    os << "le";
    break;
  case Token::Type::ge:
    os << "ge";
    break;
  case Token::Type::ne:
    os << "ne";
    break;
  case Token::Type::incr:
    os << "incr";
    break;
  case Token::Type::decr:
    os << "decr";
    break;
  case Token::Type::append:
    os << "append";
    break;
  case Token::Type::lbrace:
    os << "lbrace";
    break;
  case Token::Type::rbrace:
    os << "rbrace";
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
  case Token::Type::comma:
    os << "comma";
    break;
  case Token::Type::semicolon:
    os << "semicolon";
    break;
  case Token::Type::add:
    os << "add";
    break;
  case Token::Type::subtract:
    os << "subtract";
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
  case Token::Type::power:
    os << "power";
    break;
  case Token::Type::not_:
    os << "not";
    break;
  case Token::Type::greater_than:
    os << "greater_than";
    break;
  case Token::Type::less_than:
    os << "less_than";
    break;
  case Token::Type::pipe:
    os << "pipe";
    break;
  case Token::Type::query:
    os << "query";
    break;
  case Token::Type::colon:
    os << "colon";
    break;
  case Token::Type::tilde:
    os << "tilde";
    break;
  case Token::Type::dollar:
    os << "dollar";
    break;
  case Token::Type::assign:
    os << "assign";
    break;
  }

  return os;
}

auto GD::Awk::operator<<(std::ostream& os, Token::BuiltinFunc bf) -> std::ostream&
{
  switch (bf) {
  case Token::BuiltinFunc::atan2:
    os << "atan2";
    break;
  case Token::BuiltinFunc::close:
    os << "close";
    break;
  case Token::BuiltinFunc::cos:
    os << "cos";
    break;
  case Token::BuiltinFunc::exp:
    os << "exp";
    break;
  case Token::BuiltinFunc::gsub:
    os << "gsub";
    break;
  case Token::BuiltinFunc::index:
    os << "index";
    break;
  case Token::BuiltinFunc::int_:
    os << "int";
    break;
  case Token::BuiltinFunc::length:
    os << "length";
    break;
  case Token::BuiltinFunc::log:
    os << "log";
    break;
  case Token::BuiltinFunc::match:
    os << "match";
    break;
  case Token::BuiltinFunc::rand:
    os << "rand";
    break;
  case Token::BuiltinFunc::sin:
    os << "sin";
    break;
  case Token::BuiltinFunc::split:
    os << "split";
    break;
  case Token::BuiltinFunc::sprintf:
    os << "sprintf";
    break;
  case Token::BuiltinFunc::sqrt:
    os << "sqrt";
    break;
  case Token::BuiltinFunc::srand:
    os << "srand";
    break;
  case Token::BuiltinFunc::sub:
    os << "sub";
    break;
  case Token::BuiltinFunc::substr:
    os << "substr";
    break;
  case Token::BuiltinFunc::system:
    os << "system";
    break;
  case Token::BuiltinFunc::tolower:
    os << "tolower";
    break;
  case Token::BuiltinFunc::toupper:
    os << "toupper";
    break;
  }
  return os;
}

void GD::Awk::Token::Token::debug(std::ostream& os) const
{
  os << "Token::";
  std::visit(GD::Overloaded{[&os](Type t) { os << t; },
                            [&os](Error const& e) { os << "error(" << e.get() << ")"; },
                            [&os](Number const& n) { os << "number(" << n.get() << ")"; },
                            [&os](FuncName const& n) { os << "func_name(" << n.get() << ")"; },
                            [&os](BuiltinFunc const& n) { os << "builtin_func_name(" << n << ")"; },
                            [&os](ERE const& n) { os << "ere(" << n.get() << ")"; },
                            [&os](Name const& n) { os << "name(" << n.get() << ")"; },
                            [&os](std::string const& n) { os << "string(" << n << ")"; }},
             value_);
}

auto GD::Awk::operator<<(std::ostream& os, Token const& token) -> std::ostream&
{
  switch (token.type()) {
  case Token::Type::error:
    os << "ERROR(" << token.error() << ")";
    break;
  case Token::Type::eof:
    os << "EOF";
    break;
  case Token::Type::newline:
    os << "# NEWLINE\n";
    break;
  case Token::Type::name:
    os << token.name();
    break;
  case Token::Type::func_name:
    os << token.func_name() << "(";
    break;
  case Token::Type::builtin_func_name:
    os << token.builtin_func_name();
    break;
  case Token::Type::string:
    os << "\"" << token.string() << "\"";
    break;
  case Token::Type::number:
    os << token.number();
    break;
  case Token::Type::ere:
    os << "/" << token.ere() << "/";
    break;
  case Token::Type::begin:
    os << "BEGIN";
    break;
  case Token::Type::break_:
    os << "break";
    break;
  case Token::Type::continue_:
    os << "continue";
    break;
  case Token::Type::delete_:
    os << "delete";
    break;
  case Token::Type::do_:
    os << "do";
    break;
  case Token::Type::else_:
    os << "else";
    break;
  case Token::Type::end:
    os << "END";
    break;
  case Token::Type::exit:
    os << "exit";
    break;
  case Token::Type::for_:
    os << "for";
    break;
  case Token::Type::function:
    os << "function";
    break;
  case Token::Type::getline:
    os << "getline";
    break;
  case Token::Type::if_:
    os << "if";
    break;
  case Token::Type::in:
    os << "in";
    break;
  case Token::Type::next:
    os << "next";
    break;
  case Token::Type::print:
    os << "print";
    break;
  case Token::Type::printf:
    os << "printf";
    break;
  case Token::Type::return_:
    os << "return";
    break;
  case Token::Type::while_:
    os << "while";
    break;
  case Token::Type::add_assign:
    os << "+=";
    break;
  case Token::Type::sub_assign:
    os << "-=";
    break;
  case Token::Type::mul_assign:
    os << "*=";
    break;
  case Token::Type::div_assign:
    os << "/=";
    break;
  case Token::Type::mod_assign:
    os << "%=";
    break;
  case Token::Type::pow_assign:
    os << "^=";
    break;
  case Token::Type::or_:
    os << "||";
    break;
  case Token::Type::and_:
    os << "&&";
    break;
  case Token::Type::no_match:
    os << "!~";
    break;
  case Token::Type::eq:
    os << "==";
    break;
  case Token::Type::le:
    os << "<=";
    break;
  case Token::Type::ge:
    os << ">=";
    break;
  case Token::Type::ne:
    os << "!=";
    break;
  case Token::Type::incr:
    os << "++";
    break;
  case Token::Type::decr:
    os << "--";
    break;
  case Token::Type::append:
    os << ">>";
    break;
  case Token::Type::lbrace:
    os << "{";
    break;
  case Token::Type::rbrace:
    os << "}";
    break;
  case Token::Type::lparens:
    os << "(";
    break;
  case Token::Type::rparens:
    os << ")";
    break;
  case Token::Type::lsquare:
    os << "[";
    break;
  case Token::Type::rsquare:
    os << "]";
    break;
  case Token::Type::comma:
    os << ",";
    break;
  case Token::Type::semicolon:
    os << ";";
    break;
  case Token::Type::add:
    os << "+";
    break;
  case Token::Type::subtract:
    os << "-";
    break;
  case Token::Type::multiply:
    os << "*";
    break;
  case Token::Type::divide:
    os << "/";
    break;
  case Token::Type::modulo:
    os << "%";
    break;
  case Token::Type::power:
    os << "^";
    break;
  case Token::Type::not_:
    os << "!";
    break;
  case Token::Type::greater_than:
    os << ">";
    break;
  case Token::Type::less_than:
    os << "<";
    break;
  case Token::Type::pipe:
    os << "|";
    break;
  case Token::Type::query:
    os << "?";
    break;
  case Token::Type::colon:
    os << ":";
    break;
  case Token::Type::tilde:
    os << "~";
    break;
  case Token::Type::dollar:
    os << "$";
    break;
  case Token::Type::assign:
    os << "=";
    break;
  }
  return os;
}

auto GD::Awk::operator==(Token const& token, Token::Type type) -> bool
{
  return token.type() == type;
}

auto GD::Awk::operator==(Token const& token, Token::BuiltinFunc builtin_func) -> bool
{
  return token.type() == Token::Type::builtin_func_name &&
         token.builtin_func_name() == builtin_func;
}

auto GD::Awk::operator==(Token::Type type, Token const& token) -> bool { return token == type; }

auto GD::Awk::operator!=(Token const& token, Token::Type type) -> bool { return !(token == type); }

auto GD::Awk::operator!=(Token::Type type, Token const& token) -> bool { return !(token == type); }

auto GD::Awk::operator!=(Token const& token, Token::BuiltinFunc builtin_func) -> bool
{
  return !(token == builtin_func);
}
