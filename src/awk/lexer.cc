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
  // Sometimes we create a second pending token - usually when we have an error and the correction
  // for that error.
  if (!t_.has_value() && t2_.has_value()) {
    t_ = t2_;
    t2_.reset();
  }

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

void GD::Awk::Lexer::lex_comment()
{
  assert(r_->peek() == '#');

  // Comments continue to the end of a line (or file).  However, we need to handle new-lines which
  // may have been escaped.
  bool seen_escape{false};
  while (true) {
    switch (r_->peek()) {
    case EOF:
      return;
    case '\n':
      if (!seen_escape) {
        return;
      }
      seen_escape = false;
      break;
    case '\\':
      seen_escape = true;
      break;
    default:
      seen_escape = false;
      break;
    }

    r_->chew();
  }
}

auto GD::Awk::Lexer::lex_octal_escape() -> char
{
  assert(r_->peek() >= '0' && r_->peek() < '8');

  unsigned c{0};
  unsigned len{0};
  std::string representation;
  while (r_->peek() >= '0' && r_->peek() < '8' && len < 3) {
    ++len;
    c <<= 3;
    c |= r_->peek() - '0';
    representation += static_cast<char>(r_->peek());
    r_->chew();
  }

  if (c > std::numeric_limits<unsigned char>::max()) {
    t2_.emplace(Token::Type::error, r_->error(Msg::octal_escape_too_big, representation));
  }
  if (c == 0) {
    t2_.emplace(Token::Type::error, r_->error(Msg::octal_escape_0_undefined, representation));
  }

  return static_cast<char>(c);
}

void GD::Awk::Lexer::lex_string()
{
  assert(r_->peek() == '"');
  r_->chew();

  std::string str;
  bool seen_escape{false};

  while (true) {
    switch (r_->peek()) {
    case EOF:
      t_.emplace(Token::Type::string, str);
      t2_.emplace(Token::Type::error, r_->error(Msg::unexpected_eof_in_string));
      return;
    case '\n':
      if (!seen_escape) {
        // Don't chew the newline.
        t_.emplace(Token::Type::string, str);
        t2_.emplace(Token::Type::error, r_->error(Msg::unexpected_nl_in_string));
        return;
      }

      seen_escape = false;
      r_->chew();
      break;
    case '"':
      r_->chew();
      if (!seen_escape) {
        t_.emplace(Token::Type::string, str);
        return;
      }

      seen_escape = false;
      str += '"';
      break;
    case '/':
      seen_escape = false;
      r_->chew();
      str += '/';
      break;
    case '\\':
      if (seen_escape) {
        str += '\\';
        seen_escape = false;
      }
      else {
        seen_escape = true;
      }

      r_->chew();
      break;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      if (seen_escape) {
        seen_escape = false;
        str += lex_octal_escape();
        break;
      }

      str += static_cast<char>(r_->peek());
      r_->chew();
      break;
    case 'a':
      str += seen_escape ? '\a' : 'a';
      seen_escape = false;
      r_->chew();
      break;
    case 'b':
      str += seen_escape ? '\b' : 'b';
      seen_escape = false;
      r_->chew();
      break;
    case 'f':
      str += seen_escape ? '\f' : 'f';
      seen_escape = false;
      r_->chew();
      break;
    case 'n':
      str += seen_escape ? '\n' : 'n';
      seen_escape = false;
      r_->chew();
      break;
    case 'r':
      str += seen_escape ? '\r' : 'r';
      seen_escape = false;
      r_->chew();
      break;
    case 't':
      str += seen_escape ? '\t' : 't';
      seen_escape = false;
      r_->chew();
      break;
    case 'v':
      str += seen_escape ? '\v' : 'v';
      seen_escape = false;
      r_->chew();
      break;
    default:
      if (seen_escape) {
        // We put an error in to the queue.  But we continue to parse the string as normal.
        seen_escape = false;
        t2_.emplace(Token::Type::error, r_->error(Msg::bad_escape_sequence, r_->peek()));
      }

      str += static_cast<char>(r_->peek());
      r_->chew();
      break;
    }
  }
}

void GD::Awk::Lexer::lex_word()
{
  static const std::unordered_map<std::string, Token::Type> token_map{
    {"BEGIN", Token::Type::begin},
    {"break", Token::Type::break_},
    {"continue", Token::Type::continue_},
    {"delete", Token::Type::delete_},
    {"do", Token::Type::do_},
    {"else", Token::Type::else_},
    {"END", Token::Type::end},
    {"exit", Token::Type::exit},
    {"for", Token::Type::for_},
    {"function", Token::Type::function},
    {"getline", Token::Type::getline},
    {"if", Token::Type::if_},
    {"in", Token::Type::in},
    {"next", Token::Type::next},
    {"print", Token::Type::print},
    {"printf", Token::Type::printf},
    {"return", Token::Type::return_},
    {"while", Token::Type::while_},
  };

  static const std::unordered_map<std::string, Token::BuiltinFunc> builtin_map{
    {"atan2", Token::BuiltinFunc::atan2},     {"close", Token::BuiltinFunc::close},
    {"cos", Token::BuiltinFunc::cos},         {"exp", Token::BuiltinFunc::exp},
    {"gsub", Token::BuiltinFunc::gsub},       {"index", Token::BuiltinFunc::index},
    {"int", Token::BuiltinFunc::int_},        {"length", Token::BuiltinFunc::length},
    {"log", Token::BuiltinFunc::log},         {"match", Token::BuiltinFunc::match},
    {"rand", Token::BuiltinFunc::rand},       {"sin", Token::BuiltinFunc::sin},
    {"split", Token::BuiltinFunc::split},     {"sprintf", Token::BuiltinFunc::sprintf},
    {"sqrt", Token::BuiltinFunc::sqrt},       {"srand", Token::BuiltinFunc::srand},
    {"sub", Token::BuiltinFunc::sub},         {"substr", Token::BuiltinFunc::substr},
    {"system", Token::BuiltinFunc::system},   {"tolower", Token::BuiltinFunc::tolower},
    {"toupper", Token::BuiltinFunc::toupper},
  };

  std::string word;
  bool cont{true};
  bool maybe_func_name{false};

  while (cont) {
    switch (r_->peek()) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
    case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'E':
    case 'F':
    case 'G':
    case 'H':
    case 'I':
    case 'J':
    case 'K':
    case 'L':
    case 'M':
    case 'N':
    case 'O':
    case 'P':
    case 'Q':
    case 'R':
    case 'S':
    case 'T':
    case 'U':
    case 'V':
    case 'W':
    case 'X':
    case 'Y':
    case 'Z':
    case 'a':
    case 'b':
    case 'c':
    case 'd':
    case 'e':
    case 'f':
    case 'g':
    case 'h':
    case 'i':
    case 'j':
    case 'k':
    case 'l':
    case 'm':
    case 'n':
    case 'o':
    case 'p':
    case 'q':
    case 'r':
    case 's':
    case 't':
    case 'u':
    case 'v':
    case 'w':
    case 'x':
    case 'y':
    case 'z':
    case '_':
      word += static_cast<char>(r_->peek());
      r_->chew();
      break;
    case '(':
      maybe_func_name = true;
      cont = false;
      break;
    default:
      cont = false;
      break;
    }
  }

  auto it{token_map.find(word)};
  if (it != token_map.end()) {
    t_.emplace(it->second);
    return;
  }

  auto builtin_it{builtin_map.find(word)};
  if (builtin_it != builtin_map.end()) {
    t_.emplace(Token::Type::builtin_func_name, builtin_it->second);
    return;
  }

  if (maybe_func_name) {
    r_->chew();
    t_.emplace(Token::Type::func_name, word);
    return;
  }

  t_.emplace(Token::Type::name, word);
}

void GD::Awk::Lexer::lex()
{
  while (true) {
    switch (r_->peek()) {
    case EOF:
      t_.emplace(Token::Type::eof);
      return;
    case '\n':
      r_->chew();
      t_.emplace(Token::Type::newline);
      return;
    case ' ':
      r_->chew();  // We just chew ' '.
      break;
    case '#':
      lex_comment();
      break;
    case '"':
      lex_string();
      return;
    case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'E':
    case 'F':
    case 'G':
    case 'H':
    case 'I':
    case 'J':
    case 'K':
    case 'L':
    case 'M':
    case 'N':
    case 'O':
    case 'P':
    case 'Q':
    case 'R':
    case 'S':
    case 'T':
    case 'U':
    case 'V':
    case 'W':
    case 'X':
    case 'Y':
    case 'Z':
    case 'a':
    case 'b':
    case 'c':
    case 'd':
    case 'e':
    case 'f':
    case 'g':
    case 'h':
    case 'i':
    case 'j':
    case 'k':
    case 'l':
    case 'm':
    case 'n':
    case 'o':
    case 'p':
    case 'q':
    case 'r':
    case 's':
    case 't':
    case 'u':
    case 'v':
    case 'w':
    case 'x':
    case 'y':
    case 'z':
    case '_':
      lex_word();
      return;
    default:
      t_.emplace(Token::Type::error, r_->error(Msg::unexpected_token, r_->peek()));
      r_->chew();
      return;
    }
  }
}
