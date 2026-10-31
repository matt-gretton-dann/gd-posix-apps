/** \file test-token-format.cc
 * Assert token diagnostics and classification without relying on test logging.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include <sstream>
#include <string>
#include <string_view>

#include "bc.hh"

namespace {
using Token = GD::Bc::Token;
using Type = Token::Type;
enum Class : unsigned {
  plain = 0,
  assign = 1,
  increment = 2,
  multiply = 4,
  add = 8,
  relation = 16
};

void check_token(Token const& token, Type type, std::string_view name, std::string_view spelling,
                 std::string_view debug, unsigned classes)
{
  INFO("token type = " << name);
  REQUIRE(token.type() == type);
  REQUIRE(token == type);
  REQUIRE(type == token);
  REQUIRE_FALSE(token != type);
  REQUIRE_FALSE(type != token);
  auto const other = type == Type::eof ? Type::newline : Type::eof;
  REQUIRE_FALSE(token == other);
  REQUIRE_FALSE(other == token);
  REQUIRE(token != other);
  REQUIRE(other != token);
  REQUIRE(token.is_assign_op() == ((classes & assign) != 0));
  REQUIRE(token.is_incr_decr_op() == ((classes & increment) != 0));
  REQUIRE(token.is_mul_op() == ((classes & multiply) != 0));
  REQUIRE(token.is_add_op() == ((classes & add) != 0));
  REQUIRE(token.is_rel_op() == ((classes & relation) != 0));
  std::ostringstream type_text;
  type_text << type;
  REQUIRE(type_text.str() == name);
  std::ostringstream text;
  text << token;
  REQUIRE(text.str() == spelling);
  std::ostringstream diagnostic;
  token.debug(diagnostic);
  REQUIRE(diagnostic.str() == std::string("Token::") + std::string(debug));
  REQUIRE(fmt::vformat("{}", fmt::make_format_args(token)) == spelling);
  REQUIRE(fmt::vformat("{:}", fmt::make_format_args(token)) == spelling);
  REQUIRE_THROWS_AS(fmt::vformat("{:x}", fmt::make_format_args(token)), fmt::format_error);
}
}  // namespace

TEST_CASE("Token spelling and operator classes are explicit", "[bc][token][format]")
{
  struct Case
  {
    Type type;
    std::string_view name;
    std::string_view spelling;
    unsigned classes;
  };
  Case const cases[] = {
    {Type::eof, "eof", "EOF", plain},
    {Type::newline, "newline", "NEWLINE", plain},
    {Type::power, "power", "^", plain},
    {Type::multiply, "multiply", "*", multiply},
    {Type::divide, "divide", "/", multiply},
    {Type::modulo, "modulo", "%", multiply},
    {Type::add, "add", "+", add},
    {Type::subtract, "subtract", "-", add},
    {Type::assign, "assign", "=", assign},
    {Type::add_assign, "add_assign", "+=", assign},
    {Type::subtract_assign, "subtract_assign", "-=", assign},
    {Type::multiply_assign, "multiply_assign", "*=", assign},
    {Type::divide_assign, "divide_assign", "/=", assign},
    {Type::modulo_assign, "modulo_assign", "%=", assign},
    {Type::power_assign, "power_assign", "^=", assign},
    {Type::equals, "equals", "==", relation},
    {Type::less_than_equals, "less_than_equals", "<=", relation},
    {Type::greater_than_equals, "greater_than_equals", ">=", relation},
    {Type::not_equals, "not_equals", "!=", relation},
    {Type::less_than, "less_than", "<", relation},
    {Type::greater_than, "greater_than", ">", relation},
    {Type::increment, "increment", "++", increment},
    {Type::decrement, "decrement", "--", increment},
    {Type::define, "define", "define", plain},
    {Type::break_, "break", "break", plain},
    {Type::halt, "halt", "halt", plain},
    {Type::quit, "quit", "quit", plain},
    {Type::length, "length", "length", plain},
    {Type::return_, "return", "return", plain},
    {Type::for_, "for", "for", plain},
    {Type::if_, "if", "if", plain},
    {Type::while_, "while", "while", plain},
    {Type::abs, "abs", "abs", plain},
    {Type::sqrt, "sqrt", "sqrt", plain},
    {Type::scale, "scale", "scale", plain},
    {Type::ibase, "ibase", "ibase", plain},
    {Type::obase, "obase", "obase", plain},
    {Type::auto_, "auto", "auto", plain},
    {Type::lparens, "lparens", "(", plain},
    {Type::rparens, "rparens", ")", plain},
    {Type::lsquare, "lsquare", "[", plain},
    {Type::rsquare, "rsquare", "]", plain},
    {Type::lbrace, "lbrace", "{", plain},
    {Type::rbrace, "rbrace", "}", plain},
    {Type::comma, "comma", ",", plain},
    {Type::semicolon, "semicolon", ";", plain},
  };
  for (auto const& test : cases) {
    check_token(Token(test.type), test.type, test.name, test.spelling, test.name, test.classes);
  }
}

TEST_CASE("Token payload diagnostics preserve their values", "[bc][token][format]")
{
  check_token(Token(Type::number, "12.30"), Type::number, "number", "12.30", "number(12.30)",
              plain);
  check_token(Token(Type::string, "hello"), Type::string, "string", "\"hello\"", "string(hello)",
              plain);
  check_token(Token(Type::string, ""), Type::string, "string", "\"\"", "string()", plain);
  check_token(Token(Type::error, "bad input"), Type::error, "error", "ERROR(bad input)",
              "error(bad input)", plain);
  check_token(Token(Type::letter, GD::Bc::Letter('z')), Type::letter, "letter", "z", "letter(z)",
              plain);
  REQUIRE(Token(Type::number, "12.30").number() == "12.30");
  REQUIRE(Token(Type::string, "hello").string() == "hello");
  REQUIRE(Token(Type::error, "bad input").error() == "bad input");
  REQUIRE(Token(Type::letter, GD::Bc::Letter('z')).letter() == 'z');
}

TEST_CASE("Letter formatting accepts only an empty specification", "[bc][token][format]")
{
  std::string_view const letters = "abcdefghijklmnopqrstuvwxyz";
  for (std::size_t i = 0; i < letters.size(); ++i) {
    GD::Bc::Letter const letter(letters[i]);
    REQUIRE(static_cast<unsigned>(letter) == i);
    REQUIRE(letter == letters[i]);
    REQUIRE_FALSE(letter != letters[i]);
    REQUIRE_FALSE(letters[i] != letter);
    REQUIRE(letter == GD::Bc::Letter(letters[i]));
    REQUIRE_FALSE(letter != GD::Bc::Letter(letters[i]));
    auto const other = letters[(i + 1) % letters.size()];
    REQUIRE(letter != other);
    REQUIRE(other != letter);
    REQUIRE(letter != GD::Bc::Letter(other));
    REQUIRE(fmt::vformat("{}", fmt::make_format_args(letter)) == std::string(1, letters[i]));
    REQUIRE(fmt::vformat("{:}", fmt::make_format_args(letter)) == std::string(1, letters[i]));
    REQUIRE_THROWS_AS(fmt::vformat("{:x}", fmt::make_format_args(letter)), fmt::format_error);
  }
}
