/** \file   expr.cc
 *  \brief  Main program for expr
 *  \author Copyright 2020, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/span.hh"

#include "util/utils.hh"

#include "expr-messages.hh"

#include <algorithm>
#include <cassert>
#include <clocale>
#include <cstdint>
#include <functional>
#include <iostream>
#include <ostream>
#include <regex>
#include <string>
#include <string_view>
#include <vector>

using Msg = GD::Expr::Msg;

namespace {
/** \brief  Exit codes. */
enum class ExitCode { success, zero_or_null_result, invalid_expr, error };

[[noreturn]] void exit(ExitCode code)
{
  std::exit(static_cast<int>(code));  // NOLINT(concurrency-mt-unsafe)
}

/** \brief       Report an error and exit with exit code 3.
 *  \param  msg  Message ID
 *  \param  args Arguments for the message.
 */
template<typename... Ts>
[[noreturn]] void error(Msg msg, Ts... args)
{
  std::cerr << GD::program_name() << ": "
            << GD::Expr::Messages::get().format(GD::Expr::Set::expr, msg, args...) << '\n'
            << GD::Expr::Messages::get().format(GD::Expr::Set::expr, Msg::usage, GD::program_name())
            << '\n';
  exit(ExitCode::error);  // NOLINT(concurrency-mt-unsafe)
}

/** \brief       Report an invalid_expr and exit with exit code 2.
 *  \param  msg  Message ID
 *  \param  args Arguments for the message.
 */
template<typename... Ts>
[[noreturn]] void invalid_expr(Msg msg, Ts... args)
{
  std::cerr << GD::program_name() << ": "
            << GD::Expr::Messages::get().format(GD::Expr::Set::expr, msg, args...) << '\n';
  exit(ExitCode::invalid_expr);  // NOLINT(concurrency-mt-unsafe)
}

/** \brief       Report a warning
 *  \param  msg  Message ID
 *  \param  args Arguments for the message.
 */
template<typename... Ts>
void warn(Msg msg, Ts... args)
{
  std::cerr << GD::program_name() << ": "
            << GD::Expr::Messages::get().format(GD::Expr::Set::expr, msg, args...) << '\n';
}

/** \brief  Class that represents a token.  */
class Token
{
public:
  /** \brief Token Type.  */
  enum class Type {
    number,             /**< Number.  */
    string,             /**< String.  */
    lparens,            /**< (.  */
    rparens,            /**< ).  */
    logical_or,         /**< |.  */
    logical_and,        /**< &.  */
    equal,              /**< =.  */
    greater_than,       /**< >.  */
    greater_than_equal, /**< >=.  */
    less_than,          /**< <.  */
    less_than_equal,    /**< <=.  */
    not_equal,          /**< !=.  */
    add,                /**< +.  */
    subtract,           /**< -.  */
    multiply,           /**< *.  */
    divide,             /**< /.  */
    modulo,             /**< %.  */
    match,              /**< :.  */
  };

  /** \brief      Construct a token.
   *  \param type Type of token - should not be Type::number
   *  \param str  String representation
   */
  Token(Type type, std::string_view str)
      : type_(type), number_(0), str_(str), action_(get_action(type))
  {
    assert(type_ != Type::number);  // NOLINT
  }

  /** \brief        Construct a number token.
   *  \param type   Should be Type::number
   *  \param number Number to represent.
   *  \param str    String representation of number
   *
   * Because of the match operator we need to be able to keep the original string representation
   * of the token to use as appropriate, and not just the numbers canonical representation.
   */
  Token(Type type, int32_t number, std::string_view str)
      : type_(type), number_(number), str_(str), action_(get_action(type))
  {
    assert(type_ == Type::number);  // NOLINT
  }

  /** \brief        Construct a number token.
   *  \param type   Should be Type::number
   *  \param number Number to represent.
   *
   * This generates a number token with the number represented in the canonical manner.
   */
  Token(Type type, int32_t number) : Token(type, number, std::to_string(number)) {}

  /** \brief  Get the token type.  */
  [[nodiscard]] auto type() const noexcept -> Type { return type_; }

  /** \brief  Get the number stored in this token.  */
  [[nodiscard]] auto number() const noexcept -> int32_t
  {
    assert(type_ == Type::number);  // NOLINT
    return number_;
  }

  /** \brief  Get the string for the token.  */
  [[nodiscard]] auto string() const noexcept -> std::string const& { return str_; }

  /** \brief  Get the precedence of this token.  Lower numbers mean a higher precedence.  */
  [[nodiscard]] auto precedence() const noexcept -> unsigned { return action_.precedence_; }

  /** \brief  Apply this token to the two tokens passed in.  */
  [[nodiscard]] auto apply(Token const& lhs, Token const& rhs) const -> Token
  {
    assert(action_.apply_ != nullptr);  // NOLINT
    return action_.apply_(lhs, rhs);
  }

  /** \brief  Is this token 0 or an empty string?  */
  [[nodiscard]] auto null_or_zero() const noexcept -> bool
  {
    if (type_ == Token::Type::number) {
      return number_ == 0;
    }

    return str_.empty();
  }

  /** \brief  Get the maximum precedence.  */
  static constexpr auto max_precedence() noexcept -> unsigned { return max_precedence_; }

private:
  static constexpr unsigned max_precedence_ = 6;

  /** \brief Internal structure describing actions.  */
  struct Action
  {
    unsigned precedence_;                                    /**< Precedence of operator.  */
    std::function<Token(Token const&, Token const&)> apply_; /**< Application function. */
  };

  /** \brief      Get action object for a token.
   *  \param type Type of token
   *  \return     Reference to action object.
   *
   * Action object has application lifetime.
   */
  static auto get_action(Type type) -> Action const&;

  Type type_;       /**< Token type.  */
  int32_t number_;  /**< Number.  */
  std::string str_; /**< String.  */
  Action action_;   /**< Precedence and action for this token.  */
};

using Tokens = std::vector<Token>;
using TokenIt = std::vector<Token>::const_iterator;

/** \brief        Tokenise something that looks like a number.
 *  \param  token String to tokenise.
 *  \return       Token - either a number or a string.
 *
 * We accept numbers in the range [-INT32_MAX, INT32_MAX] which is the minimum range mandated by
 * POSIX for command line options.  We produce a warning on parsing overflow.
 *
 * We do this by hand because there actually isn't a sensible C/C++ parse function that guarantees
 * an int32_t result.
 */
auto tokenise_number(std::string_view token) -> Token
{
  int32_t number = 0;
  int32_t sign = 1;
  constexpr int input_base = 10;
  std::string_view::iterator p = token.begin();
  if (*p == '-') {
    sign = -1;
    ++p;
  }

  while (p != token.end() && *p >= '0' && *p <= '0' + input_base - 1) {
    int32_t new_number = number * input_base + (*p - '0');
    if (new_number / input_base != number) {
      warn(Msg::number_parse_overflow, token);
      break;
    }
    number = new_number;
    ++p;
  }

  return p == token.end() ? Token(Token::Type::number, number * sign, token)
                          : Token(Token::Type::string, token);
}

/** \brief  Tokenise a string into a token.
 *  \param  token Token string
 *  \return       Token.
 *
 * Because of the rules about tokenisation for `expr` we never fail to tokenise - everything falls
 * back to being a Token::Type::string.
 *
 * We manually iterate through up to the first three characters to identify what type of token we
 * have.
 */
auto tokenise(std::string_view token) -> Token
{
  Token::Type type = Token::Type::string; /* What we think the token type is currently.  */
  std::string_view::iterator it = token.begin();
  if (it == token.end()) {
    return Token(type, token);
  }

  /* Examine the first character and make a guess about the token type.  */
  switch (*it) {
  case '(':
    type = Token::Type::lparens;
    break;
  case ')':
    type = Token::Type::rparens;
    break;
  case '|':
    type = Token::Type::logical_or;
    break;
  case '&':
    type = Token::Type::logical_and;
    break;
  case '=':
    type = Token::Type::equal;
    break;
  case '>':
    type = Token::Type::greater_than;
    break;
  case '<':
    type = Token::Type::less_than;
    break;
  case '!':
    type = Token::Type::not_equal;
    break;
  case '+':
    type = Token::Type::add;
    break;
  case '-':
    type = Token::Type::subtract;
    break;
  case '*':
    type = Token::Type::multiply;
    break;
  case '/':
    type = Token::Type::divide;
    break;
  case '%':
    type = Token::Type::modulo;
    break;
  case ':':
    type = Token::Type::match;
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
  case '9': /* Possibly a number.  */
    return tokenise_number(token);
  default: /* Definitely not a special token. */
    return Token(type, token);
  }

  /* Now parse second character.  */
  ++it;
  if (type == Token::Type::not_equal) {
    if (it == token.end() || *it != '=') {
      /* ! is the only character that means something in a two character sequence but not a 1
         character one.  */
      type = Token::Type::string;
    }
  }
  else {
    if (it == token.end()) {
      return Token(type, token);
    }
    switch (*it) {
    case '=':
      if (type == Token::Type::greater_than) {
        type = Token::Type::greater_than_equal;
      }
      else if (type == Token::Type::less_than) {
        type = Token::Type::less_than_equal;
      }
      else {
        type = Token::Type::string;
      }
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
      if (type == Token::Type::subtract) {
        return tokenise_number(token);
      }
      else {
        type = Token::Type::string;
      }
      break;
    default:
      type = Token::Type::string;
      break;
    }
  }

  if (type == Token::Type::string) {
    return Token(type, token);
  }

  /* Now for the third character: If the token is more than 2 characters long its definitely a
     string at this point.  */
  ++it;
  if (it != token.end()) {
    type = Token::Type::string;
  }

  return Token(type, token);
}

/** \brief       Tokenise the command line arguments.
 *  \param  args Arguments.
 *  \return      Vector of tokens.
 */
auto tokenise(GD::Span::span<char*>::iterator begin, GD::Span::span<char*>::iterator end) -> Tokens
{
  Tokens result;
  result.reserve(std::distance(begin, end));

  auto out = std::back_inserter(result);

  std::transform(begin, end, out, [](auto arg) { return tokenise(arg); });
  return result;
}

auto do_match(Token const& lhs, Token const& rhs) -> Token
{
  std::string res = rhs.string();
  if (!res.empty() && res[0] != '^') {
    res = '^' + res;
  }
  std::regex re(res, std::regex_constants::basic);
  std::smatch m;
  bool matched = std::regex_search(lhs.string(), m, re);
  if (re.mark_count() > 0) {
    std::string s = matched ? m.str(1) : "";
    return tokenise(s.c_str());
  }
  auto len = m.length();
  if (len > INT32_MAX) {
    warn(Msg::match_too_long, lhs.string(), rhs.string(), len);
  }
  return Token(Token::Type::number, matched ? static_cast<int32_t>(m.length(0)) : 0);
}

auto do_multiply(Token const& lhs, Token const& rhs) noexcept -> Token
{
  if (lhs.type() != Token::Type::number) {
    invalid_expr(Msg::expected_number_lhs, '*', lhs.string());
  }
  if (rhs.type() != Token::Type::number) {
    invalid_expr(Msg::expected_number_rhs, '*', lhs.string());
  }

  if ((INT32_MAX / abs(lhs.number())) < abs(rhs.number())) {
    warn(Msg::overflow, '*', lhs.number(), rhs.number());
  }

  return Token(Token::Type::number, lhs.number() * rhs.number());
}

auto do_divide(Token const& lhs, Token const& rhs) -> Token
{
  if (lhs.type() != Token::Type::number) {
    invalid_expr(Msg::expected_number_lhs, '/', lhs.string());
  }
  if (rhs.type() != Token::Type::number) {
    invalid_expr(Msg::expected_number_rhs, '/', lhs.string());
  }

  return Token(Token::Type::number, lhs.number() / rhs.number());
}

auto do_modulo(Token const& lhs, Token const& rhs) -> Token
{
  if (lhs.type() != Token::Type::number) {
    invalid_expr(Msg::expected_number_lhs, '%', lhs.string());
  }
  if (rhs.type() != Token::Type::number) {
    invalid_expr(Msg::expected_number_rhs, '%', lhs.string());
  }

  return Token(Token::Type::number, lhs.number() % rhs.number());
}

auto do_add(Token const& lhs, Token const& rhs) -> Token
{
  if (lhs.type() != Token::Type::number) {
    invalid_expr(Msg::expected_number_lhs, '+', lhs.string());
  }
  if (rhs.type() != Token::Type::number) {
    invalid_expr(Msg::expected_number_rhs, '+', lhs.string());
  }

  if (lhs.number() > 0 && rhs.number() > 0 && (INT32_MAX - lhs.number()) < rhs.number()) {
    warn(Msg::overflow, '+', lhs.number(), rhs.number());
  }
  if (lhs.number() < 0 && rhs.number() < 0 && (INT32_MAX + lhs.number()) < -rhs.number()) {
    warn(Msg::overflow, '+', lhs.number(), rhs.number());
  }

  return Token(Token::Type::number, lhs.number() + rhs.number());
}

auto do_subtract(Token const& lhs, Token const& rhs) -> Token
{
  if (lhs.type() != Token::Type::number) {
    invalid_expr(Msg::expected_number_lhs, '-', lhs.string());
  }
  if (rhs.type() != Token::Type::number) {
    invalid_expr(Msg::expected_number_rhs, '-', lhs.string());
  }

  if (lhs.number() > 0 && rhs.number() < 0 && (INT32_MAX - lhs.number()) < -rhs.number()) {
    warn(Msg::overflow, '-', lhs.number(), rhs.number());
  }
  if (lhs.number() < 0 && rhs.number() > 0 && (INT32_MAX + lhs.number()) < rhs.number()) {
    warn(Msg::overflow, '-', lhs.number(), rhs.number());
  }

  return Token(Token::Type::number, lhs.number() - rhs.number());
}

template<typename Fn>
auto do_comparison(Token const& lhs, Token const& rhs, Fn comparitor) -> Token
{
  int comparison = 0;
  if (lhs.type() == Token::Type::number && rhs.type() == Token::Type::number) {
    if (lhs.number() > rhs.number()) {
      comparison = 1;
    }
    else if (lhs.number() < rhs.number()) {
      comparison = -1;
    }
    else {
      assert(lhs.number() == rhs.number());  // NOLINT
      comparison = 0;
    }
  }
  else {
    comparison = ::strcoll(lhs.string().data(), rhs.string().data());
  }
  return Token(Token::Type::number, comparitor(comparison) ? 1 : 0);
}

auto do_and(Token const& lhs, Token const& rhs) -> Token
{
  if (!lhs.null_or_zero() && !rhs.null_or_zero()) {
    return lhs;
  }
  return Token(Token::Type::number, 0);
}

auto do_or(Token const& lhs, Token const& rhs) -> Token
{
  if (!lhs.null_or_zero()) {
    return lhs;
  }
  if (!rhs.string().empty()) {
    return rhs;
  }

  return Token(Token::Type::number, 0);
}

auto parse(TokenIt& begin, TokenIt const& end) -> Token;

auto parse_primary(TokenIt& begin, TokenIt const& end) -> Token
{
  if (begin == end) {
    invalid_expr(Msg::expected_token);
  }

  return *(begin++);
}

auto parse_parens(TokenIt& begin, TokenIt const& end) -> Token
{
  if (begin != end) {
    if (begin->type() == Token::Type::lparens) {
      ++begin;
      auto result = parse(begin, end);
      if (begin == end) {
        invalid_expr(Msg::expected_rparens_at_end);
      }
      if (begin->type() != Token::Type::rparens) {
        invalid_expr(Msg::expected_rparens, begin->string());
      }
      ++begin;
      return result;
    }
  }

  return parse_primary(begin, end);
}

auto Token::get_action(Type type) -> Token::Action const&
{
  static std::array<Action, 18> const actions = {
    Action{max_precedence_ + 1, nullptr},  // number,
    Action{max_precedence_ + 1, nullptr},  // string,
    Action{max_precedence_ + 1, nullptr},  // lparens,
    Action{max_precedence_ + 1, nullptr},  // rparens,
    Action{max_precedence_ + 0, do_or},    // logical_or,
    Action{max_precedence_ - 1, do_and},   // logical_and,
    Action{max_precedence_ - 2,
           [](Token const& l, Token const& r) {
             return do_comparison(l, r, [](int cmp) { return cmp == 0; });
           }},  // equal
    Action{max_precedence_ - 2,
           [](Token const& l, Token const& r) {
             return do_comparison(l, r, [](int cmp) { return cmp > 0; });
           }},  // greater_than,
    Action{max_precedence_ - 2,
           [](Token const& l, Token const& r) {
             return do_comparison(l, r, [](int cmp) { return cmp >= 0; });
           }},  // greater_than_equal,
    Action{max_precedence_ - 2,
           [](Token const& l, Token const& r) {
             return do_comparison(l, r, [](int cmp) { return cmp < 0; });
           }},  // less_than,
    Action{max_precedence_ - 2,
           [](Token const& l, Token const& r) {
             return do_comparison(l, r, [](int cmp) { return cmp <= 0; });
           }},  // less_than_equal,
    Action{max_precedence_ - 2,
           [](Token const& l, Token const& r) {
             return do_comparison(l, r, [](int cmp) { return cmp != 0; });
           }},                                 // not_equal,
    Action{max_precedence_ - 3, do_add},       // add,
    Action{max_precedence_ - 3, do_subtract},  // subtract,
    Action{max_precedence_ - 4, do_multiply},  // multiply,
    Action{max_precedence_ - 4, do_divide},    // divide,
    Action{max_precedence_ - 4, do_modulo},    // modulo,
    Action{max_precedence_ - 5, do_match},     // match
  };

  return actions.at(static_cast<size_t>(type));
}

auto parse_expr(TokenIt& begin, TokenIt const& end, unsigned int precedence) -> Token
{
  if (precedence == 0) {
    return parse_parens(begin, end);
  }

  Token lhs = parse_expr(begin, end, precedence - 1);
  while (begin != end && begin->precedence() == precedence) {
    auto op = begin++;
    Token rhs = parse_expr(begin, end, precedence - 1);
    lhs = op->apply(lhs, rhs);
  }
  return lhs;
}

auto parse(TokenIt& begin, TokenIt const& end) -> Token
{
  return parse_expr(begin, end, Token::max_precedence());
}
}  // namespace

auto main(int argc, char** argv) -> int
{
  try {
    std::setlocale(LC_ALL, "");  // NOLINT(concurrency-mt-unsafe)
    GD::Span::span<char*> args(argv, argc);
    auto it = args.begin();
    GD::program_name(*it++);

    if (it != args.end() && std::strcmp(*it, "--") == 0) {
      ++it;
    }

    if (it == args.end()) {
      error(Msg::missing_operand);
    }

    auto tokens = tokenise(it, args.end());
    auto begin = tokens.cbegin();
    auto result = parse(begin, tokens.end());
    if (begin != tokens.end()) {
      invalid_expr(Msg::extra_tokens_at_end, begin->string());
    }

    std::cout << result.string() << '\n';
    // NOLINTNEXTLINE(concurrency-mt-unsafe)
    exit(result.null_or_zero() ? ExitCode::zero_or_null_result : ExitCode::success);
  }
  catch (std::exception& e) {
    error(Msg::uncaught_std_exception, e.what());
  }
  catch (...) {
    error(Msg::uncaught_exception);
  }
}
