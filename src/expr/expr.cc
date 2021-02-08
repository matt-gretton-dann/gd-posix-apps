#include "expr-messages.hh"
#include "util/utils.hh"

#include <assert.h>
#include <functional>
#include <iostream>
#include <locale.h>
#include <ostream>
#include <regex>
#include <stdint.h>
#include <string>
#include <string_view>
#include <vector>

using Msg = GD::Expr::Msg;

namespace {
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
  ::exit(3);
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
  ::exit(2);
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
  Token(Type type, std::string_view str) : type_(type), number_(0), str_(str)
  {
    assert(type_ != Type::number);
  }

  /** \brief        Construct a number token.
   *  \param type   Should be Type::number
   *  \param number Number to represent.
   *  \param str    String representation of number
   *
   * Because of the match operator we need to be able to keep the original string representation
   * of the token to use as appropriate, and not just the numbers canonical representation.
   */
  Token(Type type, int32_t number, std::string_view str) : type_(type), number_(number), str_(str)
  {
    assert(type_ == Type::number);
  }

  /** \brief        Construct a number token.
   *  \param type   Should be Type::number
   *  \param number Number to represent.
   *
   * This generates a number token with the number represented in the canonical manner.
   */
  Token(Type type, int32_t number) : Token(type, number, std::to_string(number)) {}

  /** \brief  Get the token type.  */
  Type type() const noexcept { return type_; }

  /** \brief  Get the number stored in this token.  */
  int32_t number() const noexcept
  {
    assert(type_ == Type::number);
    return number_;
  }

  /** \brief  Get the string for the token.  */
  std::string const& string() const noexcept { return str_; }

  /** \brief  Get the precedence of this token.  Lower numbers mean a higher precedence.  */
  unsigned precedence() const noexcept { return actions_[static_cast<size_t>(type_)].precedence_; }

  /** \brief  Apply this token to the two tokens passed in.  */
  Token apply(Token const& lhs, Token const& rhs) const noexcept
  {
    assert(actions_[static_cast<size_t>(type_)].apply_ != nullptr);
    return actions_[static_cast<size_t>(type_)].apply_(lhs, rhs);
  }

  /** \brief  Is this token 0 or an empty string?  */
  bool null_or_zero() const noexcept
  {
    if (type_ == Token::Type::number) {
      return number_ == 0;
    }

    return str_.empty();
  }

private:
  /** \brief Internal structure describing actions.  */
  struct Actions
  {
    unsigned precedence_;                                    /**< Precedence of operator.  */
    std::function<Token(Token const&, Token const&)> apply_; /**< Application function. */
  };

  Type type_;                /**< Token type.  */
  int32_t number_;           /**< Number.  */
  std::string str_;          /**< String.  */
  static Actions actions_[]; /**< Array (indexed by type of Token). */
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
Token tokenise_number(char const* token)
{
  int32_t number = 0;
  int32_t sign = 1;
  char const* p = token;
  if (*p == '-') {
    sign = -1;
    ++p;
  }

  while (*p >= '0' && *p <= '9') {
    int32_t new_number = number * 10 + (*p - '0');
    if (new_number / 10 != number) {
      warn(Msg::number_parse_overflow, token);
      break;
    }
    number = new_number;
    ++p;
  }

  return *p == '\0' ? Token(Token::Type::number, number * sign, token)
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
Token tokenise(char const* token)
{
  Token::Type type = Token::Type::string; /* What we think the token type is currently.  */

  /* Examine the first character and make a guess about the token type.  */
  switch (*token) {
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
  if (type == Token::Type::not_equal) {
    if (token[1] != '=') {
      /* ! is the only character that means something in a two character sequence but not a 1
         character one.  */
      type = Token::Type::string;
    }
  }
  else {
    switch (token[1]) {
    case '\0':
      return Token(type, token);
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
  if (token[2] != '\0') {
    type = Token::Type::string;
  }

  return Token(type, token);
}

/** \brief       Tokenise the command line arguments.
 *  \param  argc Argument count.
 *  \param  argv Argument vector.
 *  \return      Vector of tokens.
 */
Tokens tokenise(int argc, char** argv)
{
  assert(argc >= 0);
  assert(argv != nullptr);

  Tokens result;
  result.reserve(argc);

  while (argc > 0) {
    assert(*argv != nullptr);
    result.emplace_back(tokenise(*argv));
    ++argv;
    --argc;
  }

  return result;
}

Token do_match(Token const& lhs, Token const& rhs)
{
  std::string res = rhs.string();
  if (res.size() > 0 && res[0] != '^') {
    res = '^' + res;
  }
  std::regex re(res, std::regex_constants::basic);
  std::smatch m;
  bool matched = std::regex_search(lhs.string(), m, re);
  if (re.mark_count() > 0) {
    std::string s = matched ? m.str(1) : "";
    return tokenise(s.c_str());
  }
  else {
    auto len = m.length();
    if (len > INT32_MAX) {
      warn(Msg::match_too_long, lhs.string(), rhs.string(), len);
    }
    return Token(Token::Type::number, matched ? static_cast<int32_t>(m.length(0)) : 0);
  }
}

Token do_multiply(Token const& lhs, Token const& rhs)
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

Token do_divide(Token const& lhs, Token const& rhs)
{
  if (lhs.type() != Token::Type::number) {
    invalid_expr(Msg::expected_number_lhs, '/', lhs.string());
  }
  if (rhs.type() != Token::Type::number) {
    invalid_expr(Msg::expected_number_rhs, '/', lhs.string());
  }

  return Token(Token::Type::number, lhs.number() / rhs.number());
}

Token do_modulo(Token const& lhs, Token const& rhs)
{
  if (lhs.type() != Token::Type::number) {
    invalid_expr(Msg::expected_number_lhs, '%', lhs.string());
  }
  if (rhs.type() != Token::Type::number) {
    invalid_expr(Msg::expected_number_rhs, '%', lhs.string());
  }

  return Token(Token::Type::number, lhs.number() % rhs.number());
}

Token do_add(Token const& lhs, Token const& rhs)
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

Token do_subtract(Token const& lhs, Token const& rhs)
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
Token do_comparison(Token const& lhs, Token const& rhs, Fn comparitor)
{
  int comparison;
  if (lhs.type() == Token::Type::number && rhs.type() == Token::Type::number) {
    if (lhs.number() > rhs.number()) {
      comparison = 1;
    }
    else if (lhs.number() < rhs.number()) {
      comparison = -1;
    }
    else {
      assert(lhs.number() == rhs.number());
      comparison = 0;
    }
  }
  else {
    comparison = ::strcoll(lhs.string().data(), rhs.string().data());
  }
  return Token(Token::Type::number, comparitor(comparison) ? 1 : 0);
}

Token do_and(Token const& lhs, Token const& rhs)
{
  if (!lhs.null_or_zero() && !rhs.null_or_zero()) {
    return lhs;
  }
  else {
    return Token(Token::Type::number, 0);
  }
}

Token do_or(Token const& lhs, Token const& rhs)
{
  if (!lhs.null_or_zero()) {
    return lhs;
  }
  else if (!rhs.string().empty()) {
    return rhs;
  }
  else {
    return Token(Token::Type::number, 0);
  }
}

Token parse(TokenIt& begin, TokenIt end);

Token parse_primary(TokenIt& begin, TokenIt end)
{
  if (begin == end) {
    invalid_expr(Msg::expected_token);
  }

  return *(begin++);
}

Token parse_parens(TokenIt& begin, TokenIt end)
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

Token::Actions Token::actions_[] = {
  {100, nullptr},  // number,
  {100, nullptr},  // string,
  {100, nullptr},  // lparens,
  {100, nullptr},  // rparens,
  {6, do_or},      // logical_or,
  {5, do_and},     // logical_and,
  {4,
   [](Token const& l, Token const& r) {
     return do_comparison(l, r, [](int cmp) { return cmp == 0; });
   }},  // equal
  {4,
   [](Token const& l, Token const& r) {
     return do_comparison(l, r, [](int cmp) { return cmp > 0; });
   }},  // greater_than,
  {4,
   [](Token const& l, Token const& r) {
     return do_comparison(l, r, [](int cmp) { return cmp >= 0; });
   }},  // greater_than_equal,
  {4,
   [](Token const& l, Token const& r) {
     return do_comparison(l, r, [](int cmp) { return cmp < 0; });
   }},  // less_than,
  {4,
   [](Token const& l, Token const& r) {
     return do_comparison(l, r, [](int cmp) { return cmp <= 0; });
   }},  // less_than_equal,
  {4,
   [](Token const& l, Token const& r) {
     return do_comparison(l, r, [](int cmp) { return cmp != 0; });
   }},               // not_equal,
  {3, do_add},       // add,
  {3, do_subtract},  // subtract,
  {2, do_multiply},  // multiply,
  {2, do_divide},    // divide,
  {2, do_modulo},    // modulo,
  {1, do_match},     // match
};

Token parse_expr(TokenIt& begin, TokenIt end, unsigned int precedence)
{
  if (precedence == 0) {
    return parse_parens(begin, end);
  }

  Token lhs = parse_expr(begin, end, precedence - 1);
  while (begin != end && begin->precedence() == precedence) {
    TokenIt op = begin++;
    Token rhs = parse_expr(begin, end, precedence - 1);
    lhs = op->apply(lhs, rhs);
  }
  return lhs;
}

Token parse(TokenIt& begin, TokenIt end) { return parse_expr(begin, end, 6); }
}  // namespace

int main(int argc, char** argv)
{
  ::setlocale(LC_ALL, "");
  GD::program_name(argv[0]);

  --argc;
  ++argv;

  if (argc > 0 && argv[0][0] == '-' && argv[0][1] == '-' && argv[0][2] == '\0') {
    ++argv;
    --argc;
  }

  if (argc == 0) {
    error(Msg::missing_operand);
  }

  auto tokens = tokenise(argc, argv);
  TokenIt begin = tokens.begin();
  auto result = parse(begin, tokens.end());
  if (begin != tokens.end()) {
    invalid_expr(Msg::extra_tokens_at_end, begin->string());
  }

  std::cout << result.string() << '\n';

  if (result.null_or_zero()) {
    return 1;
  }
  else {
    return 0;
  }
}
