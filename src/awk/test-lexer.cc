/** \file   test-lexer.cc
 *  \brief  Tests for awk's Lexer
 *  \author Copyright 2022, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include <catch2/catch.hpp>

#include <algorithm>
#include <memory>
#include <string>
#include <string_view>

#include "awk.hh"

TEST_CASE("GD::Awk::Lexer - Word Tokenizing", "[awk][lexer]")
{
  auto [input, expected] = GENERATE(table<std::string_view, GD::Awk::Token::Type>({
    {"BEGIN", GD::Awk::Token::Type::begin},
    {"break", GD::Awk::Token::Type::break_},
    {"continue", GD::Awk::Token::Type::continue_},
    {"delete", GD::Awk::Token::Type::delete_},
    {"do", GD::Awk::Token::Type::do_},
    {"else", GD::Awk::Token::Type::else_},
    {"END", GD::Awk::Token::Type::end},
    {"exit", GD::Awk::Token::Type::exit},
    {"for", GD::Awk::Token::Type::for_},
    {"function", GD::Awk::Token::Type::function},
    {"getline", GD::Awk::Token::Type::getline},
    {"if", GD::Awk::Token::Type::if_},
    {"in", GD::Awk::Token::Type::in},
    {"next", GD::Awk::Token::Type::next},
    {"print", GD::Awk::Token::Type::print},
    {"printf", GD::Awk::Token::Type::printf},
    {"return", GD::Awk::Token::Type::return_},
    {"while", GD::Awk::Token::Type::while_},
  }));
  auto lexer = GD::Awk::Lexer(std::make_unique<GD::Awk::StringReader>(input));
  INFO("Parsing " << input);
  auto t1 = lexer.peek();
  REQUIRE(t1.type() == expected);
  lexer.chew();
  auto t2 = lexer.peek();
  REQUIRE(t2.type() == GD::Awk::Token::Type::eof);
}

TEST_CASE("GD::Awk::Lexer - Builtin func Tokenizing", "[awk][lexer]")
{
  auto [input, expected] = GENERATE(table<std::string_view, GD::Awk::Token::BuiltinFunc>({
    {"atan2", GD::Awk::Token::BuiltinFunc::atan2},
    {"close", GD::Awk::Token::BuiltinFunc::close},
    {"cos", GD::Awk::Token::BuiltinFunc::cos},
    {"exp", GD::Awk::Token::BuiltinFunc::exp},
    {"gsub", GD::Awk::Token::BuiltinFunc::gsub},
    {"index", GD::Awk::Token::BuiltinFunc::index},
    {"int", GD::Awk::Token::BuiltinFunc::int_},
    {"length", GD::Awk::Token::BuiltinFunc::length},
    {"log", GD::Awk::Token::BuiltinFunc::log},
    {"match", GD::Awk::Token::BuiltinFunc::match},
    {"rand", GD::Awk::Token::BuiltinFunc::rand},
    {"sin", GD::Awk::Token::BuiltinFunc::sin},
    {"split", GD::Awk::Token::BuiltinFunc::split},
    {"sprintf", GD::Awk::Token::BuiltinFunc::sprintf},
    {"sqrt", GD::Awk::Token::BuiltinFunc::sqrt},
    {"srand", GD::Awk::Token::BuiltinFunc::srand},
    {"sub", GD::Awk::Token::BuiltinFunc::sub},
    {"substr", GD::Awk::Token::BuiltinFunc::substr},
    {"system", GD::Awk::Token::BuiltinFunc::system},
    {"tolower", GD::Awk::Token::BuiltinFunc::tolower},
    {"toupper", GD::Awk::Token::BuiltinFunc::toupper},
  }));
  auto lexer = GD::Awk::Lexer(std::make_unique<GD::Awk::StringReader>(input));
  INFO("Parsing " << input);
  auto t1 = lexer.peek();
  REQUIRE(t1.type() == GD::Awk::Token::Type::builtin_func_name);
  REQUIRE(t1.builtin_func_name() == expected);
  lexer.chew();
  auto t2 = lexer.peek();
  REQUIRE(t2.type() == GD::Awk::Token::Type::eof);
}

TEST_CASE("GD::Awk::Lexer - Name", "[awk][lexer]")
{
  std::string_view const input{"fred george(\nherbert # ignatious\njo # kerry\\\nlonger\nmary\n"};
  auto lexer{GD::Awk::Lexer(std::make_unique<GD::Awk::StringReader>(input))};
  INFO("Parsing " << input);
  auto t1{lexer.peek()};
  REQUIRE(t1.type() == GD::Awk::Token::Type::name);
  REQUIRE(t1.name() == "fred");
  lexer.chew();
  auto t2{lexer.peek()};
  REQUIRE(t2.type() == GD::Awk::Token::Type::func_name);
  REQUIRE(t2.func_name() == "george");
  lexer.chew();
  auto t3{lexer.peek()};
  REQUIRE(t3.type() == GD::Awk::Token::Type::newline);
  lexer.chew();
  auto t4{lexer.peek()};
  REQUIRE(t4.type() == GD::Awk::Token::Type::name);
  REQUIRE(t4.name() == "herbert");
  lexer.chew();
  auto t5{lexer.peek()};
  REQUIRE(t5.type() == GD::Awk::Token::Type::newline);
  lexer.chew();
  auto t6{lexer.peek()};
  REQUIRE(t6.type() == GD::Awk::Token::Type::name);
  REQUIRE(t6.name() == "jo");
  lexer.chew();
  auto t7{lexer.peek()};
  REQUIRE(t7.type() == GD::Awk::Token::Type::newline);
  lexer.chew();
  auto t8{lexer.peek()};
  REQUIRE(t8.type() == GD::Awk::Token::Type::name);
  REQUIRE(t8.name() == "mary");
  lexer.chew();
  auto t9{lexer.peek()};
  REQUIRE(t9.type() == GD::Awk::Token::Type::newline);
  lexer.chew();
  auto t10{lexer.peek()};
  REQUIRE(t10.type() == GD::Awk::Token::Type::eof);
}
