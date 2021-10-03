/** \file   test-lexer.cc
 *  \brief  Tests for bc's Lexer
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include <catch2/catch.hpp>

#include <algorithm>
#include <memory>
#include <string>
#include <string_view>

#include "bc.hh"

TEST_CASE("GD::Bc::Lexer - Symbol Tokenizing", "[bc][lexer]")
{
  auto [input, expected] = GENERATE(table<std::string_view, GD::Bc::Token::Type>({
    {"*", GD::Bc::Token::Type::multiply},
    {"/", GD::Bc::Token::Type::divide},
    {"%", GD::Bc::Token::Type::modulo},
    {"^", GD::Bc::Token::Type::power},
    {"=", GD::Bc::Token::Type::assign},
    {"+=", GD::Bc::Token::Type::add_assign},
    {"-=", GD::Bc::Token::Type::subtract_assign},
    {"*=", GD::Bc::Token::Type::multiply_assign},
    {"/=", GD::Bc::Token::Type::divide_assign},
    {"%=", GD::Bc::Token::Type::modulo_assign},
    {"^=", GD::Bc::Token::Type::power_assign},
    {"==", GD::Bc::Token::Type::equals},
    {"<=", GD::Bc::Token::Type::less_than_equals},
    {">=", GD::Bc::Token::Type::greater_than_equals},
    {"!=", GD::Bc::Token::Type::not_equals},
    {"<", GD::Bc::Token::Type::less_than},
    {">", GD::Bc::Token::Type::greater_than},
    {"++", GD::Bc::Token::Type::increment},
    {"--", GD::Bc::Token::Type::decrement},
    {"define", GD::Bc::Token::Type::define},
    {"break", GD::Bc::Token::Type::break_},
    {"quit", GD::Bc::Token::Type::quit},
    {"length", GD::Bc::Token::Type::length},
    {"return", GD::Bc::Token::Type::return_},
    {"for", GD::Bc::Token::Type::for_},
    {"if", GD::Bc::Token::Type::if_},
    {"while", GD::Bc::Token::Type::while_},
    {"sqrt", GD::Bc::Token::Type::sqrt},
    {"scale", GD::Bc::Token::Type::scale},
    {"ibase", GD::Bc::Token::Type::ibase},
    {"obase", GD::Bc::Token::Type::obase},
    {"auto", GD::Bc::Token::Type::auto_},
    {";", GD::Bc::Token::Type::semicolon},
    {"[", GD::Bc::Token::Type::lsquare},
    {"]", GD::Bc::Token::Type::rsquare},
    {"(", GD::Bc::Token::Type::lparens},
    {")", GD::Bc::Token::Type::rparens},
    {"{", GD::Bc::Token::Type::lbrace},
    {"}", GD::Bc::Token::Type::rbrace},
    {",", GD::Bc::Token::Type::comma},
    {"+", GD::Bc::Token::Type::add},
    {"-", GD::Bc::Token::Type::subtract},
    {"\n", GD::Bc::Token::Type::newline},
  }));
  auto lexer = GD::Bc::Lexer(std::make_unique<GD::Bc::StringReader>(input));
  INFO("Parsing " << input);
  auto t1 = lexer.peek();
  REQUIRE(t1.type() == expected);
  lexer.chew();
  auto t2 = lexer.peek();
  REQUIRE(t2.type() == GD::Bc::Token::Type::eof);
}

TEST_CASE("GD::Bc::Lexer - Token extensions", "[bc][lexer][extensions]")
{
  auto [input, expected] = GENERATE(table<std::string_view, GD::Bc::Token::Type>({
    {"halt", GD::Bc::Token::Type::halt},
    {"abs", GD::Bc::Token::Type::abs},
  }));
  auto lexer = GD::Bc::Lexer(std::make_unique<GD::Bc::StringReader>(input));
  INFO("Parsing " << input);
  auto t1 = lexer.peek();
  if (GD::Bc::extensions_enabled()) {
    REQUIRE(t1.type() == expected);
    lexer.chew();
    auto t2 = lexer.peek();
    REQUIRE(t2.type() == GD::Bc::Token::Type::eof);
  }
  else {
    REQUIRE(t1.type() != expected);
  }
}

TEST_CASE("GD::Bc::Lexer - Number", "[bc][lexer]")
{
  // NOLINTNEXTLINE
  std::string_view number = GENERATE("1234567890", "ABCDEF1", ".1", "2.0", "3.", "1\\\n2");
  auto lexer = GD::Bc::Lexer(std::make_unique<GD::Bc::StringReader>(number));
  auto t1 = lexer.peek();
  REQUIRE(t1.type() == GD::Bc::Token::Type::number);
  std::string result(number);
  // Remove escaped new-lines
  result.erase(
    std::remove_if(result.begin(), result.end(), [](char c) { return c == '\\' || c == '\n'; }),
    result.end());
  REQUIRE(t1.number() == result);
  lexer.chew();
  REQUIRE(lexer.peek().type() == GD::Bc::Token::Type::eof);
}

TEST_CASE("GD::Bc::Lexer - Number Extensions", "[bc][lexer][extensions]")
{
  std::string_view number = GENERATE("1\\\n    2");  // NOLINT
  auto lexer = GD::Bc::Lexer(std::make_unique<GD::Bc::StringReader>(number));
  auto t1 = lexer.peek();
  if (GD::Bc::extensions_enabled()) {
    // As an extension we remove spaces are escaped newlines.
    std::string result(number);
    result.erase(
      std::remove_if(result.begin(), result.end(),
                     [](char c) { return c == '\\' || c == '\n' || c == ' ' || c == '\t'; }),
      result.end());
    REQUIRE(t1.type() == GD::Bc::Token::Type::number);
    REQUIRE(t1.number() == result);
    lexer.chew();
    REQUIRE(lexer.peek().type() == GD::Bc::Token::Type::eof);
  }
  else {
    REQUIRE(t1.type() == GD::Bc::Token::Type::number);
    lexer.chew();
    REQUIRE(lexer.peek().type() != GD::Bc::Token::Type::eof);
  }
}

TEST_CASE("GD::Bc::Lexer - Letter", "[bc][lexer]")
{
  auto letter = GENERATE('a', 'z');
  std::string s(1, letter);
  auto lexer = GD::Bc::Lexer(std::make_unique<GD::Bc::StringReader>(s));
  auto t1 = lexer.peek();
  REQUIRE(t1.type() == GD::Bc::Token::Type::letter);
  REQUIRE(t1.letter() == letter);
  lexer.chew();
  REQUIRE(lexer.peek().type() == GD::Bc::Token::Type::eof);
}

TEST_CASE("GD::Bc::Lexer - String", "[bc][lexer]")
{
  std::string_view string = GENERATE("Hello, world!", "A\\\nB");  // NOLINT
  std::string s = std::string("\"") + std::string(string) + std::string("\"");
  auto lexer = GD::Bc::Lexer(std::make_unique<GD::Bc::StringReader>(s));
  auto t1 = lexer.peek();
  REQUIRE(t1.type() == GD::Bc::Token::Type::string);
  REQUIRE(t1.string() == string);
  lexer.chew();
  REQUIRE(lexer.peek().type() == GD::Bc::Token::Type::eof);
}

TEST_CASE("GD::Bc::Lexer - Two numbers", "[bc][lexer]")
{
  std::string input("123.456.789");
  auto lexer = GD::Bc::Lexer(std::make_unique<GD::Bc::StringReader>(input));
  auto t1 = lexer.peek();
  REQUIRE(t1.type() == GD::Bc::Token::Type::number);
  REQUIRE(t1.number() == "123.456");
  lexer.chew();
  auto t2 = lexer.peek();
  REQUIRE(t2.type() == GD::Bc::Token::Type::number);
  REQUIRE(t2.number() == ".789");
  lexer.chew();
  REQUIRE(lexer.peek().type() == GD::Bc::Token::Type::eof);
}

TEST_CASE("GD::Bc::Lexer - number on multiple lines", "[bc][lexer]")
{
  std::string input("123\\\n456");
  auto lexer = GD::Bc::Lexer(std::make_unique<GD::Bc::StringReader>(input));
  auto t1 = lexer.peek();
  REQUIRE(t1.type() == GD::Bc::Token::Type::number);
  REQUIRE(t1.number() == "123456");
  lexer.chew();
  REQUIRE(lexer.peek().type() == GD::Bc::Token::Type::eof);
}

TEST_CASE("GD::Bc::Lexer - line merging", "[bc][lexer]")
{
  std::string input("a\\\nb");
  auto lexer = GD::Bc::Lexer(std::make_unique<GD::Bc::StringReader>(input));
  auto t1 = lexer.peek();
  REQUIRE(t1.type() == GD::Bc::Token::Type::letter);
  REQUIRE(t1.letter() == 'a');
  lexer.chew();
  auto t2 = lexer.peek();
  REQUIRE(t2.type() == GD::Bc::Token::Type::letter);
  REQUIRE(t2.letter() == 'b');
  lexer.chew();
  REQUIRE(lexer.peek().type() == GD::Bc::Token::Type::eof);
}

TEST_CASE("GD::Bc::Lexer - =- is an error", "[bc][lexer]")
{
  std::string input("=-");
  auto lexer = GD::Bc::Lexer(std::make_unique<GD::Bc::StringReader>(input));
  auto t1 = lexer.peek();
  REQUIRE(t1.type() == GD::Bc::Token::Type::error);
}
