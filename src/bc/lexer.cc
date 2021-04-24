/** \file   lexer.cc
 *  \brief  Implementation of GD::Bc::Lexer and derived classes
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "util/utils.hh"

#include "bc-messages.hh"

#include <memory>
#include <optional>
#include <stdint.h>

#include "bc.hh"
#include <string_view>

GD::Bc::Lexer::Lexer(std::unique_ptr<Reader>&& r) : r_(std::move(r)), t_(std::nullopt) {}

GD::Bc::Token const& GD::Bc::Lexer::peek()
{
  if (!t_.has_value()) {
    lex();
  }

  assert(t_.has_value());
  return *t_;
}

void GD::Bc::Lexer::chew()
{
  /* If we have nothing to chew we need to find something.  */
  if (!t_.has_value()) {
    lex();
  }

  if (t_->type() != Token::Type::eof) {
    t_.reset();
  }
}

void GD::Bc::Lexer::lex_string()
{
  assert(r_->peek() == '"');
  r_->chew();

  std::string value;

  while (true) {
    switch (r_->peek()) {
    case '"':
      r_->chew();
      t_.emplace(Token::Type::string, value);
      return;
    case EOF:
      r_->error(Msg::unexpected_eof_string);
      break;
    default:
      value += r_->peek();
      r_->chew();
      break;
    }
  }
}

void GD::Bc::Lexer::lex_number()
{
  std::string value;
  bool seen_period = false;
  bool cont = true;

  while (cont) {
    switch (r_->peek()) {
    case 'F':
    case 'E':
    case 'D':
    case 'C':
    case 'B':
    case 'A':
    case '9':
    case '8':
    case '7':
    case '6':
    case '5':
    case '4':
    case '3':
    case '2':
    case '1':
    case '0':
      value += r_->peek();
      r_->chew();
      break;
    case '.':
      if (!seen_period) {
        value += r_->peek();
        r_->chew();
        seen_period = true;
      }
      else {
        cont = false;
      }
      break;
    case '\\':
      r_->chew();
      if (r_->peek() != '\n') {
        r_->error(Msg::unexpected_token, '\\');
        cont = false;
      }

      r_->chew();
      break;
    default:
      cont = false;
      break;
    }
  }

  assert(!value.empty());
  t_.emplace(Token::Type::number, value);
}

void GD::Bc::Lexer::lex_letter_or_keyword()
{
  static const std::unordered_map<std::string, Token::Type> token_map{
    {"auto", Token::Type::auto_},    {"break", Token::Type::break_},
    {"define", Token::Type::define}, {"ibase", Token::Type::ibase},
    {"if", Token::Type::if_},        {"for", Token::Type::for_},
    {"length", Token::Type::length}, {"obase", Token::Type::obase},
    {"quit", Token::Type::quit},     {"return", Token::Type::return_},
    {"scale", Token::Type::scale},   {"sqrt", Token::Type::sqrt},
    {"while", Token::Type::while_},
  };
  std::string value;
  bool cont = true;

  while (cont) {
    switch (r_->peek()) {
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
      value += r_->peek();
      r_->chew();
      break;
    default:
      cont = false;
      break;
    }
  }

  assert(!value.empty());
  if (value.size() == 1) {
    t_.emplace(Token::Type::letter, value[0]);
    return;
  }

  auto it = token_map.find(value);
  if (it != token_map.end()) {
    t_.emplace(it->second);
    return;
  }

  r_->error(Msg::unexpected_token, value);
}

void GD::Bc::Lexer::lex_symbol(Token::Type plain, char next1, Token::Type tok1)
{
  r_->chew();
  if (r_->peek() == next1) {
    r_->chew();
    t_.emplace(tok1);
    return;
  }

  t_.emplace(plain);
}

void GD::Bc::Lexer::lex_symbol(Token::Type plain, char next1, Token::Type tok1, char next2,
                               Token::Type tok2)
{
  r_->chew();
  if (r_->peek() == next1) {
    r_->chew();
    t_.emplace(tok1);
    return;
  }

  if (r_->peek() == next2) {
    r_->chew();
    t_.emplace(tok2);
    return;
  }

  t_.emplace(plain);
}

void GD::Bc::Lexer::lex_not_equals()
{
  assert(r_->peek() == '!');
  r_->chew();
  if (r_->peek() == '=') {
    t_.emplace(Token::Type::not_equals);
    return;
  }

  r_->error(Msg::unexpected_token, r_->peek());
}

void GD::Bc::Lexer::lex_comment()
{
  enum class CommentState { normal, pending, done };
  CommentState state = CommentState::normal;

  while (state != CommentState::done) {
    switch (r_->peek()) {
    case '*':
      state = CommentState::pending;
      break;
    case '/':
      if (state == CommentState::pending) {
        state = CommentState::done;
      }
      break;
    case EOF:
      error(Msg::unexpected_eof_comment);
      break;
    default:
      break;
    }
    r_->chew();
  }
}

void GD::Bc::Lexer::lex()
{
  /* Loop forever - this enables us to treat \ followed by \n and comments as just another go
     round the loop.  */
  while (true) {
    switch (r_->peek()) {
    case EOF:
      t_.emplace(Token::Type::eof);
      return;
    case '"':
      lex_string();
      return;
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
    case '.':
      lex_number();
      return;
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
      lex_letter_or_keyword();
      return;
    case '=':
      lex_symbol(Token::Type::assign, '=', Token::Type::equals);
      return;
    case '+':
      lex_symbol(Token::Type::add, '=', Token::Type::add_assign, '+', Token::Type::increment);

      return;
    case '-':
      lex_symbol(Token::Type::subtract, '=', Token::Type::subtract_assign, '-',
                 Token::Type::decrement);
      return;
    case '*':
      lex_symbol(Token::Type::multiply, '=', Token::Type::multiply_assign);
      return;
    case '/': {
      r_->chew();
      if (r_->peek() == '=') {
        t_.emplace(Token::Type::divide_assign);
        return;
      }

      if (r_->peek() != '*') {
        t_.emplace(Token::Type::divide);
        return;
      }

      assert(r_->peek() == '*');
      r_->chew();
      lex_comment();
      break;
    }
    case '%':
      lex_symbol(Token::Type::modulo, '=', Token::Type::modulo_assign);
      return;
    case '^':
      lex_symbol(Token::Type::power, '=', Token::Type::power_assign);
      return;
    case '<':
      lex_symbol(Token::Type::less_than, '=', Token::Type::less_than_equals);
      return;
    case '>':
      lex_symbol(Token::Type::greater_than, '=', Token::Type::greater_than_equals);
      return;
    case '!':
      lex_not_equals();
      return;
    case '\n':
      r_->chew();
      t_.emplace(Token::Type::newline);
      return;
    case '\\':
      r_->chew();
      if (r_->peek() != '\n') {
        r_->error(Msg::unexpected_token, "\\");
      }
      break;
    default:
      r_->error(Msg::unexpected_token, r_->peek());
    }
  }
}