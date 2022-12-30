/** \file   lexer.cc
 *  \brief  Implementation of GD::Bc::Lexer and derived classes
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "util/utils.hh"

#include "awk-messages.hh"

#include <charconv>
#include <clocale>
#include <cstdint>
#include <memory>
#include <string_view>
#include <unordered_map>

#include "awk.hh"

GD::Awk::Lexer::Lexer(std::unique_ptr<Reader>&& r) : r_(std::move(r)), t_(std::nullopt) {}

auto GD::Awk::Lexer::peek(bool enable_divide) -> GD::Awk::Token const&
{
  // Sometimes we create a second pending token - usually when we have an error and the correction
  // for that error.
  if (!t_.has_value() && t2_.has_value()) {
    t_ = t2_;
    t2_.reset();
  }

  if (!t_.has_value()) {
    lex(enable_divide);
  }

  assert(t_.has_value());  // NOLINT
  return *t_;
}

auto GD::Awk::Lexer::peek_cmdline_string() -> GD::Awk::Token const&
{
  // Sometimes we create a second pending token - usually when we have an error and the correction
  // for that error.
  if (!t_.has_value() && t2_.has_value()) {
    t_ = t2_;
    t2_.reset();
  }

  if (!t_.has_value()) {
    lex_string_or_ere(true);
  }

  assert(t_.has_value());  // NOLINT
  return *t_;
}

void GD::Awk::Lexer::chew(bool enable_divide)
{
  /* If we have nothing to chew we need to find something.  */
  if (!t_.has_value()) {
    lex(enable_divide);
  }

  assert(t_.has_value());  // NOLINT
  if (t_->type() != Token::Type::eof) {
    t_.reset();
  }
}

auto GD::Awk::Lexer::location() const -> GD::Awk::Location const& { return r_->location(); }

void GD::Awk::Lexer::lex_and()
{
  assert(r_->peek() == '&');
  r_->chew();

  if (r_->peek() == '&') {
    t_.emplace(Token::Type::and_);
    r_->chew();
  }
  else {
    t_.emplace(Token::Type::error, r_->error(Msg::unexpected_token, static_cast<char>('&')));
  }
}

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

void GD::Awk::Lexer::lex_number()
{
  assert(r_->peek() >= '0' && r_->peek() <= '9');

  std::string num;
  bool hex{false};
  bool floating{false};
  bool cont{true};
  std::size_t exponent_pos{std::string::npos};

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
      num += static_cast<char>(r_->peek());
      r_->chew();
      break;
    case '.':
      if (floating) {
        cont = false;
        break;
      }
      num += '.';
      r_->chew();
      floating = true;
      break;
    case 'a':
    case 'A':
    case 'b':
    case 'B':
    case 'c':
    case 'C':
    case 'd':
    case 'D':
    case 'f':
    case 'F':
      if (hex) {
        num += static_cast<char>(r_->peek());
        r_->chew();
      }
      else {
        cont = false;
      }
      break;
    case 'e':
    case 'E':
      if (!hex) {
        if (exponent_pos != std::string::npos) {
          cont = false;
          break;
        }
        floating = true;
        exponent_pos = num.size();
      }
      num += static_cast<char>(r_->peek());
      r_->chew();
      break;
    case 'p':
    case 'P':
      if (!hex || exponent_pos != std::string::npos) {
        cont = false;
        break;
      }

      floating = true;
      exponent_pos = num.size();
      num += static_cast<char>(r_->peek());
      r_->chew();
      break;
    case 'x':
    case 'X':
      if (!hex && num.length() == 1 && num[0] == '0') {
        num.clear();
        r_->chew();
        hex = true;
      }
      else {
        cont = false;
      }
      break;
    case '+':
    case '-':
      if (!floating || exponent_pos != num.size() + 1) {
        cont = false;
        break;
      }

      num += static_cast<char>(r_->peek());
      r_->chew();
      break;
    default:
      cont = false;
      break;
    }
  }

  std::from_chars_result fcr{};
  if (floating) {
    double number{0.0};
    if (hex) {
      num.insert(num.begin(), {'0', 'x'});
    };
    char const* old_locale{std::setlocale(LC_NUMERIC, "C")};  // NOLINT(concurrency-mt-unsafe)
    errno = 0;
    try {
      std::size_t pos{0};
      number = std::stod(num, &pos);
      fcr.ptr = num.data() + pos;  // NOLINT(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    }
    catch (std::invalid_argument const& e) {
      fcr.ec = std::errc::invalid_argument;
    }
    catch (std::out_of_range const& e) {
      fcr.ec = std::errc::result_out_of_range;
    }
    (void)std::setlocale(LC_NUMERIC, old_locale);  // NOLINT(concurrency-mt-unsafe)
    t_.emplace(Token::Type::floating, number);
  }
  else {
    std::uint64_t number{0};
    int const base = hex ? 16 : 10;  // NOLINT
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    fcr = std::from_chars(num.data(), num.data() + num.size(), number, base);
    t_.emplace(Token::Type::integer, number);
  }

  if (fcr.ec == std::errc::invalid_argument) {
    t2_.emplace(Token::Type::error, r_->error(Msg::invalid_number, num));
  }
  else if (fcr.ec == std::errc::result_out_of_range) {
    t2_.emplace(Token::Type::error, r_->error(Msg::number_out_of_range, num));
  }
  else {
    assert(fcr.ptr == num.data() + num.size());  // NOLINT
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

void GD::Awk::Lexer::lex_string_or_ere(bool from_cmdline)
{
  // We are called in three circumstances:
  //  From the command line - the string ends at EOF, need to treat a final \ as non-escaping
  //  As a string.  First token is ".
  //  As an ERE.  First token is /.
  assert(from_cmdline || r_->peek() == '"' || r_->peek() == '/');
  bool const is_string = from_cmdline || r_->peek() == '"';
  r_->chew();

  Token::Type const type{is_string ? Token::Type::string : Token::Type::ere};
  std::string str;
  bool seen_escape{false};

  while (true) {
    switch (r_->peek()) {
    case EOF:
      if (seen_escape && from_cmdline) {
        str += '\\';
      }
      t_.emplace(type, str);
      if (!from_cmdline) {
        t2_.emplace(Token::Type::error, r_->error(Msg::unexpected_eof_in_string));
      }
      return;
    case '\n':
      if (!seen_escape) {
        // Don't chew the newline.
        t_.emplace(type, str);
        t2_.emplace(Token::Type::error, r_->error(Msg::unexpected_nl_in_string));
        return;
      }

      seen_escape = false;
      r_->chew();
      break;
    case '"':
      r_->chew();
      if (is_string && !from_cmdline && !seen_escape) {
        t_.emplace(type, str);
        return;
      }

      seen_escape = false;
      str += '"';
      break;
    case '/':
      r_->chew();
      if (!is_string && !seen_escape) {
        t_.emplace(type, str);
        return;
      }

      seen_escape = false;
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

void GD::Awk::Lexer::lex_symbol(Token::Type plain, char next1, Token::Type tok1)
{
  r_->chew();
  if (r_->peek() == next1) {
    r_->chew();
    t_.emplace(tok1);
    return;
  }

  t_.emplace(plain);
}

void GD::Awk::Lexer::lex_symbol(Token::Type plain, char next1, Token::Type tok1, char next2,
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
    t_.emplace(Token::Type::func_name, word);
    return;
  }

  t_.emplace(Token::Type::name, word);
}

void GD::Awk::Lexer::lex(bool allow_divide)
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
      lex_string_or_ere(false);
      return;
    case '/':
      if (allow_divide) {
        lex_symbol(Token::Type::divide, '=', Token::Type::div_assign);
      }
      else {
        lex_string_or_ere(false);
      }
      return;
    case '.':
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
      lex_number();
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
    case '&':
      lex_and();
      return;
    case '{':
      t_.emplace(Token::Type::lbrace);
      r_->chew();
      return;
    case '}':
      t_.emplace(Token::Type::rbrace);
      r_->chew();
      return;
    case '(':
      t_.emplace(Token::Type::lparens);
      r_->chew();
      return;
    case ')':
      t_.emplace(Token::Type::rparens);
      r_->chew();
      return;
    case '[':
      t_.emplace(Token::Type::lsquare);
      r_->chew();
      return;
    case ']':
      t_.emplace(Token::Type::rsquare);
      r_->chew();
      return;
    case ',':
      t_.emplace(Token::Type::comma);
      r_->chew();
      return;
    case ';':
      t_.emplace(Token::Type::semicolon);
      r_->chew();
      return;
    case '+':
      lex_symbol(Token::Type::add, '=', Token::Type::add_assign, '+', Token::Type::incr);
      return;
    case '-':
      lex_symbol(Token::Type::subtract, '=', Token::Type::sub_assign, '-', Token::Type::decr);
      return;
    case '*':
      lex_symbol(Token::Type::multiply, '=', Token::Type::mul_assign);
      return;
    case '%':
      lex_symbol(Token::Type::modulo, '=', Token::Type::mod_assign);
      return;
    case '^':
      lex_symbol(Token::Type::power, '=', Token::Type::pow_assign);
      return;
    case '!':
      lex_symbol(Token::Type::not_, '=', Token::Type::ne, '~', Token::Type::no_match);
      return;
    case '>':
      lex_symbol(Token::Type::greater_than, '=', Token::Type::ge, '>', Token::Type::append);
      return;
    case '<':
      lex_symbol(Token::Type::less_than, '=', Token::Type::le);
      return;
    case '|':
      lex_symbol(Token::Type::pipe, '|', Token::Type::or_);
      return;
    case '?':
      t_.emplace(Token::Type::query);
      r_->chew();
      return;
    case ':':
      t_.emplace(Token::Type::colon);
      r_->chew();
      return;
    case '~':
      t_.emplace(Token::Type::tilde);
      r_->chew();
      return;
    case '$':
      t_.emplace(Token::Type::dollar);
      r_->chew();
      return;
    case '=':
      lex_symbol(Token::Type::assign, '=', Token::Type::eq);
      return;
    default:
      t_.emplace(Token::Type::error,
                 r_->error(Msg::unexpected_token, static_cast<char>(r_->peek())));
      r_->chew();
      return;
    }
  }
}
