/** \file   libcpp/test-trigraph-parser.cc
 *  \brief  Tests for GD::CPP::TrigraphTParser
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include <catch2/catch.hpp>

#include <sstream>

#include "error.hh"
#include "file-store.hh"
#include "identifier-manager.hh"
#include "location.hh"
#include "preprocessor-tokenizer.hh"

TEST_CASE("GD::CPP::PreprocessorTokenizer - Whitespace", "[cpp][preprocessor-tokenizer]")
{
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::PPNumberManager ppn_manager;
  GD::CPP::IdentifierManager id_manager;
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);
  auto tokenizer =
    GD::CPP::PreprocessorTokenizer(file_store, error_manager, id_manager, ppn_manager);

  auto fname = std::string("Test");
  auto input = std::string("a \t \f \v b");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::identifier);
  REQUIRE(id_manager.display_name(tokenizer.peek().identifier()) == "a");
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::white_space);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::identifier);
  REQUIRE(id_manager.display_name(tokenizer.peek().identifier()) == "b");
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_include);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_source);
}

TEST_CASE("GD::CPP::PreprocessorTokenizer - multi-line comment", "[cpp][preprocessor-tokenizer]")
{
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::PPNumberManager ppn_manager;
  GD::CPP::IdentifierManager id_manager;
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);
  auto tokenizer =
    GD::CPP::PreprocessorTokenizer(file_store, error_manager, id_manager, ppn_manager);

  auto fname = std::string("Test");
  auto input = std::string("a/**/b");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::identifier);
  REQUIRE(id_manager.display_name(tokenizer.peek().identifier()) == "a");
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::white_space);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::identifier);
  REQUIRE(id_manager.display_name(tokenizer.peek().identifier()) == "b");
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_include);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_source);
  REQUIRE(os.str().empty());
}

TEST_CASE("GD::CPP::PreprocessorTokenizer - line comment", "[cpp][preprocessor-tokenizer]")
{
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::PPNumberManager ppn_manager;
  GD::CPP::IdentifierManager id_manager;
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);
  auto tokenizer =
    GD::CPP::PreprocessorTokenizer(file_store, error_manager, id_manager, ppn_manager);

  auto fname = std::string("Test");
  auto input = std::string("a// b\nc");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::identifier);
  REQUIRE(id_manager.display_name(tokenizer.peek().identifier()) == "a");
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::white_space);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == U'\n');
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::identifier);
  REQUIRE(id_manager.display_name(tokenizer.peek().identifier()) == "c");
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_include);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_source);
  REQUIRE(os.str().empty());
}

TEST_CASE("GD::CPP::PreprocessorTokenizer - unterminated multi-line comment",
          "[cpp][preprocessor-tokenizer]")
{
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::PPNumberManager ppn_manager;
  GD::CPP::IdentifierManager id_manager;
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);
  auto tokenizer =
    GD::CPP::PreprocessorTokenizer(file_store, error_manager, id_manager, ppn_manager);

  auto fname = std::string("Test");
  auto input = std::string("a/**b");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::identifier);
  REQUIRE(id_manager.display_name(tokenizer.peek().identifier()) == "a");
  tokenizer.chew();
  REQUIRE(os.str().empty());
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::white_space);
  tokenizer.chew();
  auto errs{os.str()};
  REQUIRE(!errs.empty());
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_include);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_source);
  REQUIRE(errs == os.str());
}

TEST_CASE("GD::CPP::PreprocessorTokenizer - unterminated line comment",
          "[cpp][preprocessor-tokenizer]")
{
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::PPNumberManager ppn_manager;
  GD::CPP::IdentifierManager id_manager;
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);
  auto tokenizer =
    GD::CPP::PreprocessorTokenizer(file_store, error_manager, id_manager, ppn_manager);

  auto fname = std::string("Test");
  auto input = std::string("a// bc");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::identifier);
  REQUIRE(id_manager.display_name(tokenizer.peek().identifier()) == "a");
  tokenizer.chew();
  REQUIRE(os.str().empty());
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::white_space);
  tokenizer.chew();
  auto errs{os.str()};
  REQUIRE(!errs.empty());
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_include);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_source);
  REQUIRE(errs == os.str());
}

TEST_CASE("GD::CPP::PreprocessorTokenizer - identifier big literal",
          "[cpp][preprocessor-tokenizer]")
{
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::PPNumberManager ppn_manager;
  GD::CPP::IdentifierManager id_manager;
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);
  auto tokenizer =
    GD::CPP::PreprocessorTokenizer(file_store, error_manager, id_manager, ppn_manager);

  auto fname = std::string("Test");
  auto input = std::string("a\xF0\x9F\x98\x81z1");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  REQUIRE(os.str().empty());
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::identifier);
  auto errs{os.str()};
  REQUIRE(!errs.empty());
  REQUIRE(id_manager.display_name(tokenizer.peek().identifier()) == "a\\U0001f601z1");
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_include);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_source);
  REQUIRE(os.str() == errs);
}

TEST_CASE("GD::CPP::PreprocessorTokenizer - identifier UCN", "[cpp][preprocessor-tokenizer]")
{
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::PPNumberManager ppn_manager;
  GD::CPP::IdentifierManager id_manager;
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);
  auto tokenizer =
    GD::CPP::PreprocessorTokenizer(file_store, error_manager, id_manager, ppn_manager);

  auto fname = std::string("Test");
  auto input = std::string("a\\U0001f601z");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::identifier);
  REQUIRE(id_manager.display_name(tokenizer.peek().identifier()) == "a\\U0001f601z");
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_include);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_source);
  REQUIRE(os.str().empty());
}

TEST_CASE("GD::CPP::PreprocessorTokenizer - identifier UCN not enough hex",
          "[cpp][preprocessor-tokenizer]")
{
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::PPNumberManager ppn_manager;
  GD::CPP::IdentifierManager id_manager;
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);
  auto tokenizer =
    GD::CPP::PreprocessorTokenizer(file_store, error_manager, id_manager, ppn_manager);

  auto fname = std::string("Test");
  auto input = std::string("a\\U0001f60z");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  REQUIRE(os.str().empty());
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::identifier);
  auto errs{os.str()};
  REQUIRE(!errs.empty());
  REQUIRE(id_manager.display_name(tokenizer.peek().identifier()) == "a\\ufffdz");
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_include);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_source);
  REQUIRE(errs == os.str());
}

TEST_CASE("GD::CPP::PreprocessorTokenizer - starts UCN", "[cpp][preprocessor-tokenizer]")
{
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::PPNumberManager ppn_manager;
  GD::CPP::IdentifierManager id_manager;
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);
  auto tokenizer =
    GD::CPP::PreprocessorTokenizer(file_store, error_manager, id_manager, ppn_manager);

  auto fname = std::string("Test");
  auto input = std::string("\\U0001f601z");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::identifier);
  REQUIRE(id_manager.display_name(tokenizer.peek().identifier()) == "\\U0001f601z");
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_include);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_source);
  REQUIRE(os.str().empty());
}

TEST_CASE("GD::CPP::PreprocessorTokenizer - starts \\ not UCN", "[cpp][preprocessor-tokenizer]")
{
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::PPNumberManager ppn_manager;
  GD::CPP::IdentifierManager id_manager;
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);
  auto tokenizer =
    GD::CPP::PreprocessorTokenizer(file_store, error_manager, id_manager, ppn_manager);

  auto fname = std::string("Test");
  auto input = std::string("\\z");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  REQUIRE(tokenizer.peek() == U'\\');
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::identifier);
  REQUIRE(id_manager.display_name(tokenizer.peek().identifier()) == "z");
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_include);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_source);
  REQUIRE(os.str().empty());
}

TEST_CASE("GD::CPP::PreprocessorTokenizer - PPNumber simple", "[cpp][preprocessor-tokenizer]")
{
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::PPNumberManager ppn_manager;
  GD::CPP::IdentifierManager id_manager;
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);
  auto tokenizer =
    GD::CPP::PreprocessorTokenizer(file_store, error_manager, id_manager, ppn_manager);

  auto fname = std::string("Test");
  auto input = std::string("1234");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::ppnumber);
  REQUIRE(ppn_manager.display_name(tokenizer.peek().ppnumber()) == "1234");
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_include);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_source);
  REQUIRE(os.str().empty());
}

TEST_CASE("GD::CPP::PreprocessorTokenizer - PPNumber starts with .",
          "[cpp][preprocessor-tokenizer]")
{
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::PPNumberManager ppn_manager;
  GD::CPP::IdentifierManager id_manager;
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);
  auto tokenizer =
    GD::CPP::PreprocessorTokenizer(file_store, error_manager, id_manager, ppn_manager);

  auto fname = std::string("Test");
  auto input = std::string(".1234.");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::ppnumber);
  REQUIRE(ppn_manager.display_name(tokenizer.peek().ppnumber()) == ".1234.");
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_include);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_source);
  REQUIRE(os.str().empty());
}

TEST_CASE("GD::CPP::PreprocessorTokenizer - PPNumber signs", "[cpp][preprocessor-tokenizer]")
{
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::PPNumberManager ppn_manager;
  GD::CPP::IdentifierManager id_manager;
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);
  auto tokenizer =
    GD::CPP::PreprocessorTokenizer(file_store, error_manager, id_manager, ppn_manager);

  auto fname = std::string("Test");
  auto input = std::string("1234E+5e+6P+7p+8A+9");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::ppnumber);
  REQUIRE(ppn_manager.display_name(tokenizer.peek().ppnumber()) == "1234E+5e+6P+7p+8A");
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == U'+');
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::ppnumber);
  REQUIRE(ppn_manager.display_name(tokenizer.peek().ppnumber()) == "9");
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_include);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_source);
  REQUIRE(os.str().empty());
}

TEST_CASE("GD::CPP::PreprocessorTokenizer - Character literals", "[cpp][character-tokenizer]")
{
  auto [prefix, type] =
    GENERATE(table<std::string, GD::CPP::TokenType>({{"", GD::CPP::TokenType::char_literal},
                                                     {"U", GD::CPP::TokenType::char32_literal},
                                                     {"u", GD::CPP::TokenType::char16_literal}}));
  auto [in, out] = GENERATE(table<std::string, std::uint32_t>(
    {{"a", U'a'}, {"\\?", U'?'}, {"\\'", U'\''}, {"\\\\", U'\\'}}));

  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::PPNumberManager ppn_manager;
  GD::CPP::IdentifierManager id_manager;
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);
  auto tokenizer =
    GD::CPP::PreprocessorTokenizer(file_store, error_manager, id_manager, ppn_manager);

  auto fname = std::string("Test");
  std::string input = prefix + "'" + in + "'";
  INFO("Input = " << input);
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  REQUIRE(tokenizer.peek() == type);
  REQUIRE(tokenizer.peek().is_char_literal());
  REQUIRE(tokenizer.peek().char_literal() == out);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_include);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_source);
  REQUIRE(os.str().empty());
}
