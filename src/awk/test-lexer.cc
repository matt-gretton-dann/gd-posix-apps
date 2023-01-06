/** \file   test-lexer.cc
 *  \brief  Tests for awk's Lexer
 *  \author Copyright 2022, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include <catch2/catch.hpp>

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
  auto t1 = lexer.peek(false);
  REQUIRE(t1.type() == expected);
  lexer.chew(false);
  auto t2 = lexer.peek(false);
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
  auto t1 = lexer.peek(false);
  REQUIRE(t1.type() == GD::Awk::Token::Type::builtin_func_name);
  REQUIRE(t1.builtin_func_name() == expected);
  lexer.chew(false);
  auto t2 = lexer.peek(false);
  REQUIRE(t2.type() == GD::Awk::Token::Type::eof);
}

TEST_CASE("GD::Awk::Lexer - Name", "[awk][lexer]")
{
  std::string_view const input{"fred george(\nherbert ( # ignatious\njo # kerry\\\nlonger\nmary\n"};
  auto lexer{GD::Awk::Lexer(std::make_unique<GD::Awk::StringReader>(input))};
  INFO("Parsing " << input);
  auto t1{lexer.peek(false)};
  REQUIRE(t1.type() == GD::Awk::Token::Type::name);
  REQUIRE(t1.name() == "fred");
  lexer.chew(false);
  auto t2{lexer.peek(false)};
  REQUIRE(t2.type() == GD::Awk::Token::Type::func_name);
  REQUIRE(t2.func_name() == "george");
  lexer.chew(false);
  auto t3{lexer.peek(false)};
  REQUIRE(t3.type() == GD::Awk::Token::Type::lparens);
  lexer.chew(false);
  auto t4{lexer.peek(false)};
  REQUIRE(t4.type() == GD::Awk::Token::Type::newline);
  lexer.chew(false);
  auto t5{lexer.peek(false)};
  REQUIRE(t5.type() == GD::Awk::Token::Type::name);
  REQUIRE(t5.name() == "herbert");
  lexer.chew(false);
  auto t6{lexer.peek(false)};
  REQUIRE(t6.type() == GD::Awk::Token::Type::lparens);
  lexer.chew(false);
  auto t7{lexer.peek(false)};
  REQUIRE(t7.type() == GD::Awk::Token::Type::newline);
  lexer.chew(false);
  auto t8{lexer.peek(false)};
  REQUIRE(t8.type() == GD::Awk::Token::Type::name);
  REQUIRE(t8.name() == "jo");
  lexer.chew(false);
  auto t9{lexer.peek(false)};
  REQUIRE(t9.type() == GD::Awk::Token::Type::newline);
  lexer.chew(false);
  auto t10{lexer.peek(false)};
  REQUIRE(t10.type() == GD::Awk::Token::Type::name);
  REQUIRE(t10.name() == "mary");
  lexer.chew(false);
  auto t11{lexer.peek(false)};
  REQUIRE(t11.type() == GD::Awk::Token::Type::newline);
  lexer.chew(false);
  auto t12{lexer.peek(false)};
  REQUIRE(t12.type() == GD::Awk::Token::Type::eof);
}

TEST_CASE("GD::Awk::Lexer - Strings", "[awk][lexer]")
{
  auto [input, expected] = GENERATE(
    table<std::string_view, std::string_view>({{"\"a string\"", "a string"},
                                               {"\"\\/\"", "/"},
                                               {"\"a\\\nb\"", "ab"},
                                               {"\"\\\"\"", "\""},
                                               {"\"/\"", "/"},
                                               {"\"\\\\\"", "\\"},
                                               {"\"\\040\"", "\040"},
                                               {"\"\\0401\"", "\0401"},
                                               {"\"\\a\\b\\f\\n\\r\\t\\v\"", "\a\b\f\n\r\t\v"},
                                               {"\"abfnrtv\"", "abfnrtv"}}));
  auto lexer = GD::Awk::Lexer(std::make_unique<GD::Awk::StringReader>(input));
  INFO("Parsing " << input);
  auto t1 = lexer.peek(false);
  REQUIRE(t1.type() == GD::Awk::Token::Type::string);
  REQUIRE(t1.string() == expected);
  lexer.chew(false);
  auto t2 = lexer.peek(false);
  REQUIRE(t2.type() == GD::Awk::Token::Type::eof);
}

TEST_CASE("GD::Awk::Lexer - Strings errors", "[awk][lexer]")
{
  auto [input, expected] = GENERATE(table<std::string_view, std::string_view>(
    {{"\"a", "a"}, {"\"c\\Q\"", "cQ"}, {"\"e\\777\"", "e\377"}}));
  auto lexer = GD::Awk::Lexer(std::make_unique<GD::Awk::StringReader>(input));
  INFO("Parsing " << input);
  auto t1{lexer.peek(false)};
  REQUIRE(t1.type() == GD::Awk::Token::Type::string);
  REQUIRE(t1.string() == expected);
  lexer.chew(false);
  auto t2{lexer.peek(false)};
  REQUIRE(t2.type() == GD::Awk::Token::Type::error);
  lexer.chew(false);
  auto t3{lexer.peek(false)};
  REQUIRE(t3.type() == GD::Awk::Token::Type::eof);
}

TEST_CASE("GD::Awk::Lexer - Strings errors - newline", "[awk][lexer]")
{
  std::string_view const input{"\"b\n"};
  auto lexer = GD::Awk::Lexer(std::make_unique<GD::Awk::StringReader>(input));
  INFO("Parsing " << input);
  auto t1 = lexer.peek(false);
  REQUIRE(t1.type() == GD::Awk::Token::Type::string);
  REQUIRE(t1.string() == "b");
  lexer.chew(false);
  auto t2{lexer.peek(false)};
  REQUIRE(t2.type() == GD::Awk::Token::Type::error);
  lexer.chew(false);
  auto t3{lexer.peek(false)};
  REQUIRE(t3.type() == GD::Awk::Token::Type::newline);
  lexer.chew(false);
  auto t4{lexer.peek(false)};
  REQUIRE(t4.type() == GD::Awk::Token::Type::eof);
}

TEST_CASE("GD::Awk::Lexer - Strings errors - nul-byte", "[awk][lexer]")
{
  std::string_view const input{R"("d\000")"};
  auto lexer = GD::Awk::Lexer(std::make_unique<GD::Awk::StringReader>(input));
  INFO("Parsing " << input);
  auto t1 = lexer.peek(false);
  REQUIRE(t1.type() == GD::Awk::Token::Type::string);
  REQUIRE(t1.string() == std::string{'d', '\000'});
  lexer.chew(false);
  auto t2{lexer.peek(false)};
  REQUIRE(t2.type() == GD::Awk::Token::Type::error);
  lexer.chew(false);
  auto t3{lexer.peek(false)};
  REQUIRE(t3.type() == GD::Awk::Token::Type::eof);
}

TEST_CASE("GD::Awk::Lexer - ERE", "[awk][lexer]")
{
  auto [input, expected] = GENERATE(
    table<std::string_view, std::string_view>({{"/a string/", "a string"},
                                               {"/\\//", "/"},
                                               {"/a\\\nb/", "ab"},
                                               {"/\\\"/", "\""},
                                               {"/\"/", "\""},
                                               {"/\\\\/", "\\\\"},
                                               {"/\\$/", "\\$"},
                                               {"/\\040/", "\040"},
                                               {"/\\0401/", "\0401"},
                                               {"/\\a\\b\\f\\n\\r\\t\\v/", "\a\b\f\n\r\t\v"},
                                               {"/abfnrtv/", "abfnrtv"}}));
  auto lexer = GD::Awk::Lexer(std::make_unique<GD::Awk::StringReader>(input));
  INFO("Parsing " << input);
  auto t1 = lexer.peek(false);
  REQUIRE(t1.type() == GD::Awk::Token::Type::ere);
  REQUIRE(t1.ere() == expected);
  lexer.chew(false);
  auto t2 = lexer.peek(false);
  REQUIRE(t2.type() == GD::Awk::Token::Type::eof);
}

TEST_CASE("GD::Awk::Lexer - integers", "[awk][lexer]")
{
  auto [input, expected] = GENERATE(table<std::string_view, GD::Awk::Integer::underlying_type>({
    {"012", 12},
    {"0x12", 0x12},
  }));
  auto lexer = GD::Awk::Lexer(std::make_unique<GD::Awk::StringReader>(input));
  INFO("Parsing " << input);
  auto t1 = lexer.peek(false);
  REQUIRE(t1.type() == GD::Awk::Token::Type::integer);
  REQUIRE(t1.integer().get() == expected);
  lexer.chew(false);
  auto t2 = lexer.peek(false);
  REQUIRE(t2.type() == GD::Awk::Token::Type::eof);
}

TEST_CASE("GD::Awk::Lexer - floating", "[awk][lexer]")
{
  auto [input, expected] = GENERATE(table<std::string_view, double>({
    {"0.0", 0.0},
    {"0x1p0", 1.0},
  }));
  auto lexer = GD::Awk::Lexer(std::make_unique<GD::Awk::StringReader>(input));
  INFO("Parsing " << input);
  auto t1 = lexer.peek(false);
  REQUIRE(t1.type() == GD::Awk::Token::Type::floating);
  REQUIRE(t1.floating() == expected);
  lexer.chew(false);
  auto t2 = lexer.peek(false);
  REQUIRE(t2.type() == GD::Awk::Token::Type::eof);
}

TEST_CASE("GD::Awk::Lexer - symbols", "[awk][lexer]")
{
  auto [input, expected] = GENERATE(table<std::string_view, GD::Awk::Token::Type>({
    {"+=", GD::Awk::Token::Type::add_assign}, {"-=", GD::Awk::Token::Type::sub_assign},
    {"*=", GD::Awk::Token::Type::mul_assign}, {"/=", GD::Awk::Token::Type::div_assign},
    {"%=", GD::Awk::Token::Type::mod_assign}, {"^=", GD::Awk::Token::Type::pow_assign},
    {"||", GD::Awk::Token::Type::or_},        {"&&", GD::Awk::Token::Type::and_},
    {"!~", GD::Awk::Token::Type::no_match},   {"==", GD::Awk::Token::Type::eq},
    {"<=", GD::Awk::Token::Type::le},         {">=", GD::Awk::Token::Type::ge},
    {"!=", GD::Awk::Token::Type::ne},         {"++", GD::Awk::Token::Type::incr},
    {"--", GD::Awk::Token::Type::decr},       {">>", GD::Awk::Token::Type::append},
    {"{", GD::Awk::Token::Type::lbrace},      {"}", GD::Awk::Token::Type::rbrace},
    {"(", GD::Awk::Token::Type::lparens},     {")", GD::Awk::Token::Type::rparens},
    {"[", GD::Awk::Token::Type::lsquare},     {"]", GD::Awk::Token::Type::rsquare},
    {",", GD::Awk::Token::Type::comma},       {";", GD::Awk::Token::Type::semicolon},
    {"\n", GD::Awk::Token::Type::newline},    {"+", GD::Awk::Token::Type::add},
    {"-", GD::Awk::Token::Type::subtract},    {"*", GD::Awk::Token::Type::multiply},
    {"/", GD::Awk::Token::Type::divide},      {">", GD::Awk::Token::Type::greater_than},
    {"<", GD::Awk::Token::Type::less_than},   {"|", GD::Awk::Token::Type::pipe},
    {"?", GD::Awk::Token::Type::query},       {":", GD::Awk::Token::Type::colon},
    {"~", GD::Awk::Token::Type::tilde},       {"$", GD::Awk::Token::Type::dollar},
    {"=", GD::Awk::Token::Type::assign},
  }));
  auto lexer = GD::Awk::Lexer(std::make_unique<GD::Awk::StringReader>(input));
  INFO("Parsing " << input);
  auto t1 = lexer.peek(true);
  REQUIRE(t1.type() == expected);
  lexer.chew(true);
  auto t2 = lexer.peek(true);
  REQUIRE(t2.type() == GD::Awk::Token::Type::eof);
}
