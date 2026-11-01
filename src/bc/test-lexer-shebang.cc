/** \file test-lexer-shebang.cc
 * Initial-character shebang regressions.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <new>
#include <catch2/catch.hpp>

#include <memory>
#include <string_view>

#include "bc.hh"

TEST_CASE("Shebang is only recognized at input start", "[bc][lexer][shebang]")
{
  using Type = GD::Bc::Token::Type;
  std::string_view const input = GENERATE("#!ignored", "#!ignored\n12");
  GD::Bc::Lexer lexer(std::make_unique<GD::Bc::StringReader>(input));
  if (!GD::Bc::extensions_enabled()) {
    REQUIRE(lexer.peek().type() == Type::error);
    return;
  }
  if (input.back() == '2') {
    REQUIRE(lexer.peek().type() == Type::newline);
    lexer.chew();
    REQUIRE(lexer.peek().type() == Type::number);
    REQUIRE(lexer.peek().number() == "12");
    lexer.chew();
  }
  REQUIRE(lexer.peek().type() == Type::eof);
}

TEST_CASE("Noninitial hash characters are errors", "[bc][lexer][shebang]")
{
  using Type = GD::Bc::Token::Type;
  std::string_view const prefix = GENERATE("", " ", "\n", "1\n", "a;");
  std::string const input = std::string(prefix) + "#!ignored\n";
  GD::Bc::Lexer lexer(std::make_unique<GD::Bc::StringReader>(input));
  if (prefix.empty() && GD::Bc::extensions_enabled()) {
    REQUIRE(lexer.peek().type() == Type::newline);
    return;
  }
  // Consume known prefix tokens without relying on the shebang implementation.
  if (prefix == "1\n" || prefix == "a;") {
    REQUIRE(lexer.peek().type() == (prefix.front() == '1' ? Type::number : Type::letter));
    lexer.chew();
  }
  if (prefix == "\n" || prefix == "1\n" || prefix == "a;") {
    REQUIRE(lexer.peek().type() == (prefix.back() == '\n' ? Type::newline : Type::semicolon));
    lexer.chew();
  }
  REQUIRE(lexer.peek().type() == Type::error);
}
