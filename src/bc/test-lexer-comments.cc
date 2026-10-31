/** \file test-lexer-comments.cc
 * Comment delimiter and EOF regressions.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include <memory>
#include <string_view>

#include "bc.hh"

TEST_CASE("Comment delimiters preserve the following token", "[bc][lexer][comments]")
{
  std::string_view const text =
    GENERATE("/**/12", "/*ordinary*/12", "/*\nmultiline\n*/12", "/****/12", "/* / */12",
             "/* *x/ still inside */12", "/**//**/12", "/* **x***y*/12");
  INFO("input = " << text);
  GD::Bc::Lexer lexer(std::make_unique<GD::Bc::StringReader>(text));
  REQUIRE(lexer.peek().type() == GD::Bc::Token::Type::number);
  REQUIRE(lexer.peek().number() == "12");
  lexer.chew();
  REQUIRE(lexer.peek().type() == GD::Bc::Token::Type::eof);
}

TEST_CASE("Unterminated comments retain their diagnostic", "[bc][lexer][comments]")
{
  std::string_view const text = GENERATE("/*", "/*ordinary", "/* *", "/* *x/", "/*\n**x/");
  INFO("input = " << text);
  GD::Bc::Lexer lexer(std::make_unique<GD::Bc::StringReader>(text));
  REQUIRE(lexer.peek().type() == GD::Bc::Token::Type::error);
  REQUIRE(lexer.peek().error().find("end of file") != std::string::npos);
}
