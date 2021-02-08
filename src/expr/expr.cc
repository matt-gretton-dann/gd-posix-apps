#include "message-data.hh"
#include "util/utils.hh"

#include <assert.h>
#include <iostream>
#include <locale.h>
#include <ostream>
#include <stdint.h>
#include <string>
#include <string_view>
#include <vector>

using Msg = GD::Expr::Msg;

namespace {
template<typename... Ts>
[[noreturn]] void error(Msg msg, Ts... args)
{
  std::cerr << GD::program_name() << ": "
            << GD::Expr::Messages::get().format(GD::Expr::Set::expr, msg, args...) << '\n'
            << GD::Expr::Messages::get().format(GD::Expr::Set::expr, Msg::usage, GD::program_name())
            << '\n';
  ::exit(3);
}

template<typename... Ts>
[[noreturn]] void invalid_expr(Msg msg, Ts... args)
{
  std::cerr << GD::program_name() << ": "
            << GD::Expr::Messages::get().format(GD::Expr::Set::expr, msg, args...) << '\n';
  ::exit(2);
}

template<typename... Ts>
void warn(Msg msg, Ts... args)
{
  std::cerr << GD::program_name() << ": "
            << GD::Expr::Messages::get().format(GD::Expr::Set::expr, msg, args...) << '\n';
}

class Token
{
public:
  enum class Type {
    number,
    string,
    lparens,
    rparens,
    logical_or,
    logical_and,
    equal,
    greater_than,
    greater_than_equal,
    less_than,
    less_than_equal,
    not_equal,
    add,
    subtract,
    multiply,
    divide,
    modulo,
    match
  };

  Token(Type type, std::string_view str) : type_(type), number_(0), str_(str)
  {
    assert(type_ != Type::number);
  }

  /** \brief  Construct a number token.
   *
   * Because of the match operator we need to be able to keep the original string representation
   * of the token to use as appropriate.
   */
  Token(Type type, int32_t number, std::string_view str) : type_(type), number_(number), str_(str)
  {
    assert(type_ == Type::number);
  }

  Token(Type type, int32_t number) : Token(type, number, std::to_string(number)) {}

  Type type() const noexcept { return type_; }
  int32_t number() const noexcept
  {
    assert(type_ == Type::number);
    return number_;
  }
  std::string const& string() const noexcept { return str_; }

private:
  Type type_;
  int32_t number_;
  std::string str_;
};

using Tokens = std::vector<Token>;
using TokenIt = std::vector<Token>::const_iterator;

#if 0
std::ostream& operator<<(std::ostream& os, Token const& token)
{
  switch (token.type()) {
  case Token::Type::number:
    return os << "number(" << token.number() << ")";
  case Token::Type::string:
    return os << "string(" << token.string() << ")";
  case Token::Type::lparens:
    return os << "lparens";
  case Token::Type::rparens:
    return os << "rparens";
  case Token::Type::logical_or:
    return os << "logical_or";
  case Token::Type::logical_and:
    return os << "logical_and";
  case Token::Type::equal:
    return os << "equal";
  case Token::Type::greater_than:
    return os << "greater_than";
  case Token::Type::greater_than_equal:
    return os << "greater_than_equal";
  case Token::Type::less_than:
    return os << "less_than";
  case Token::Type::less_than_equal:
    return os << "less_than_equal";
  case Token::Type::not_equal:
    return os << "not_equal";
  case Token::Type::add:
    return os << "add";
  case Token::Type::subtract:
    return os << "subtract";
  case Token::Type::multiply:
    return os << "multiply";
  case Token::Type::divide:
    return os << "divide";
  case Token::Type::modulo:
    return os << "modulo";
  case Token::Type::match:
    return os << "match";
  default:
    return os << "unrecognised";
  }
}
#endif

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

Token do_match(Token const&, Token const&) { assert(false); }

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

bool null_or_zero(Token const& token)
{
  if (token.type() == Token::Type::number) {
    return token.number() == 0;
  }

  return token.string().empty();
}

Token do_and(Token const& lhs, Token const& rhs)
{
  if (!null_or_zero(lhs) && !null_or_zero(rhs)) {
    return lhs;
  }
  else {
    return Token(Token::Type::number, 0);
  }
}

Token do_or(Token const& lhs, Token const& rhs)
{
  if (!null_or_zero(lhs)) {
    return lhs;
  }
  else if (!rhs.string().empty()) {
    return rhs;
  }
  else {
    return Token(Token::Type::number, 0);
  }
}

Token parse_match(TokenIt& begin, TokenIt end)
{
  Token lhs = parse_parens(begin, end);
  while (begin != end && begin->type() == Token::Type::match) {
    ++begin;
    Token rhs = parse_parens(begin, end);
    lhs = do_match(lhs, rhs);
  }
  return lhs;
}

Token parse_multiplicative(TokenIt& begin, TokenIt end)
{
  Token lhs = parse_match(begin, end);
  while (begin != end &&
         (begin->type() == Token::Type::multiply || begin->type() == Token::Type::divide ||
          begin->type() == Token::Type::modulo)) {
    Token::Type type = begin->type();
    ++begin;
    Token rhs = parse_match(begin, end);
    switch (type) {
    case Token::Type::multiply:
      lhs = do_multiply(lhs, rhs);
      break;
    case Token::Type::divide:
      lhs = do_divide(lhs, rhs);
      break;
    case Token::Type::modulo:
      lhs = do_modulo(lhs, rhs);
      break;
    default:
      assert(false);
    }
  }
  return lhs;
}

Token parse_additive(TokenIt& begin, TokenIt end)
{
  Token lhs = parse_multiplicative(begin, end);
  while (begin != end &&
         (begin->type() == Token::Type::add || begin->type() == Token::Type::subtract)) {
    Token::Type type = begin->type();
    ++begin;
    Token rhs = parse_multiplicative(begin, end);
    switch (type) {
    case Token::Type::add:
      lhs = do_add(lhs, rhs);
      break;
    case Token::Type::subtract:
      lhs = do_subtract(lhs, rhs);
      break;
    default:
      assert(false);
    }
  }
  return lhs;
}

Token parse_comparison(TokenIt& begin, TokenIt end)
{
  Token lhs = parse_additive(begin, end);
  while (begin != end &&
         (begin->type() == Token::Type::equal || begin->type() == Token::Type::not_equal ||
          begin->type() == Token::Type::less_than ||
          begin->type() == Token::Type::less_than_equal ||
          begin->type() == Token::Type::greater_than ||
          begin->type() == Token::Type::greater_than_equal)) {
    Token::Type type = begin->type();
    ++begin;
    Token rhs = parse_additive(begin, end);
    switch (type) {
    case Token::Type::equal:
      lhs = do_comparison(lhs, rhs, [](int relation) { return relation == 0; });
      break;
    case Token::Type::not_equal:
      lhs = do_comparison(lhs, rhs, [](int relation) { return relation != 0; });
      break;
    case Token::Type::less_than:
      lhs = do_comparison(lhs, rhs, [](int relation) { return relation < 0; });
      break;
    case Token::Type::less_than_equal:
      lhs = do_comparison(lhs, rhs, [](int relation) { return relation <= 0; });
      break;
      break;
    case Token::Type::greater_than:
      lhs = do_comparison(lhs, rhs, [](int relation) { return relation > 0; });
      break;
    case Token::Type::greater_than_equal:
      lhs = do_comparison(lhs, rhs, [](int relation) { return relation >= 0; });
      break;
    default:
      assert(false);
    }
  }

  return lhs;
}

Token parse_and(TokenIt& begin, TokenIt end)
{
  Token lhs = parse_comparison(begin, end);
  while (begin != end && begin->type() == Token::Type::logical_and) {
    ++begin;
    Token rhs = parse_comparison(begin, end);
    lhs = do_and(lhs, rhs);
  }
  return lhs;
}

Token parse_or(TokenIt& begin, TokenIt end)
{
  Token lhs = parse_and(begin, end);
  while (begin != end && begin->type() == Token::Type::logical_or) {
    ++begin;
    Token rhs = parse_and(begin, end);
    lhs = do_or(lhs, rhs);
  }
  return lhs;
}

Token parse(TokenIt& begin, TokenIt end) { return parse_or(begin, end); }
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

  if (null_or_zero(result)) {
    return 1;
  }
  else {
    return 0;
  }
}
