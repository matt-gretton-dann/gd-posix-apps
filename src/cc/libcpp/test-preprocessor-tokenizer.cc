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

class TokenizerMaker
{
public:
  TokenizerMaker()
      : error_manager_(os_), file_store_(error_manager_),
        tokenizer_(file_store_, error_manager_, id_manager_, ppn_manager_, str_lit_manager_)
  {
    error_manager_.file_store(file_store_);
  }

  auto tokenizer() -> GD::CPP::PreprocessorTokenizer<GD::CPP::FileStore>& { return tokenizer_; }
  auto os() -> std::ostringstream& { return os_; }
  auto id_manager() -> GD::CPP::IdentifierManager& { return id_manager_; }
  auto ppn_manager() -> GD::CPP::PPNumberManager& { return ppn_manager_; }
  auto str_lit_manager() -> GD::CPP::StringLiteralManager& { return str_lit_manager_; }

private:
  std::ostringstream os_;
  GD::CPP::ErrorManager error_manager_;
  GD::CPP::PPNumberManager ppn_manager_;
  GD::CPP::IdentifierManager id_manager_;
  GD::CPP::StringLiteralManager str_lit_manager_;
  GD::CPP::FileStore file_store_;
  GD::CPP::PreprocessorTokenizer<GD::CPP::FileStore> tokenizer_;
};

TEST_CASE("GD::CPP::PreprocessorTokenizer - Whitespace", "[cpp][preprocessor-tokenizer]")
{
  TokenizerMaker tm;
  auto& tokenizer = tm.tokenizer();
  auto& id_manager = tm.id_manager();

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
  TokenizerMaker tm;
  auto& tokenizer = tm.tokenizer();
  auto& os = tm.os();
  auto& id_manager = tm.id_manager();

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
  TokenizerMaker tm;
  auto& tokenizer = tm.tokenizer();
  auto& os = tm.os();
  auto& id_manager = tm.id_manager();

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
  TokenizerMaker tm;
  auto& tokenizer = tm.tokenizer();
  auto& os = tm.os();
  auto& id_manager = tm.id_manager();

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
  TokenizerMaker tm;
  auto& tokenizer = tm.tokenizer();
  auto& os = tm.os();
  auto& id_manager = tm.id_manager();

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
  TokenizerMaker tm;
  auto& tokenizer = tm.tokenizer();
  auto& os = tm.os();
  auto& id_manager = tm.id_manager();

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
  TokenizerMaker tm;
  auto& tokenizer = tm.tokenizer();
  auto& os = tm.os();
  auto& id_manager = tm.id_manager();

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
  TokenizerMaker tm;
  auto& tokenizer = tm.tokenizer();
  auto& os = tm.os();
  auto& id_manager = tm.id_manager();

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
  TokenizerMaker tm;
  auto& tokenizer = tm.tokenizer();
  auto& os = tm.os();
  auto& id_manager = tm.id_manager();

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
  TokenizerMaker tm;
  auto& tokenizer = tm.tokenizer();
  auto& os = tm.os();
  auto& id_manager = tm.id_manager();

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
  TokenizerMaker tm;
  auto& tokenizer = tm.tokenizer();
  auto& os = tm.os();
  auto& ppn_manager = tm.ppn_manager();

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
  TokenizerMaker tm;
  auto& tokenizer = tm.tokenizer();
  auto& os = tm.os();
  auto& ppn_manager = tm.ppn_manager();

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
  TokenizerMaker tm;
  auto& tokenizer = tm.tokenizer();
  auto& os = tm.os();
  auto& ppn_manager = tm.ppn_manager();

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
  auto [in, out] = GENERATE(table<std::string, std::uint32_t>({{"a", U'a'},
                                                               {"\\?", U'?'},
                                                               {"\\'", U'\''},
                                                               {"\\\\", U'\\'},
                                                               {"\\a", 0x07},
                                                               {"\\b", 0x08},
                                                               {"\\f", 0x0c},
                                                               {"\\n", 0x0a},
                                                               {"\\r", 0x0d},
                                                               {"\\t", 0x09},
                                                               {"\\v", 0x0b},
                                                               {"\\x0000000000000f", 0x0f},
                                                               {"\\377", 0xff},
                                                               {"\\0", 0},
                                                               {"\\u00a0", 0xa0},
                                                               {"\\U000000a0", 0xa0}}));

  TokenizerMaker tm;
  auto& tokenizer = tm.tokenizer();
  auto& os = tm.os();

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

TEST_CASE("GD::CPP::PreprocessorTokenizer - String Literal", "[cpp][preprocessor-tokenizer]")
{
  TokenizerMaker tm;
  auto& tokenizer = tm.tokenizer();
  auto& os = tm.os();
  auto& sl_manager = tm.str_lit_manager();

  auto fname = std::string("Test");
  auto input = std::string(R"("a\"b'\"0\xff\x30")");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::string_literal);
  REQUIRE(sl_manager.display_name(tokenizer.peek().string_literal()) ==
          "\"a\\\"b'\\\"0\\xff\\x30\"");
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_include);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_source);
  REQUIRE(os.str().empty());
}
