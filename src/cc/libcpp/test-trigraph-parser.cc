/** \file   libcpp/test-trigraph-parser.cc
 *  \brief  Tests for GD::CPP::TrigraphTParser
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include <sstream>

#include "error.hh"
#include "file-store.hh"
#include "location.hh"
#include "tokenizers.hh"

TEST_CASE("GD::CPP::TrigraphParser - Trigraphs", "[cpp][trigraph-parser]")
{
  auto [in, replace] = GENERATE(table<char, char32_t>({{'=', U'#'},
                                                       {')', U']'},
                                                       {'!', U'|'},
                                                       {'(', U'['},
                                                       {'\'', U'^'},
                                                       {'>', U'}'},
                                                       {'/', U'\\'},
                                                       {'<', U'{'},
                                                       {'-', U'~'}}));

  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);
  auto tokenizer = GD::CPP::TrigraphParser(file_store, error_manager);

  auto fname = std::string("Test");
  auto input = std::string("??");
  input.push_back(in);
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  GD::CPP::Token const& t = tokenizer.peek();
  REQUIRE(t == replace);

  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_include);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_source);
}

TEST_CASE("GD::CPP::TrigraphParser - Special cases", "[cpp][trigraph-parser]")
{
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);
  auto tokenizer = GD::CPP::TrigraphParser(file_store, error_manager);

  auto fname = std::string("Test");
  /* Separated to stop trigraph replacement. */
  auto input = std::string("?a ??a ???"
                           "=");
  auto output = std::u32string(U"?a ??a ?#");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  for (auto c : output) {
    REQUIRE(tokenizer.peek() == c);
    tokenizer.chew(GD::CPP::TokenType::character);
  }

  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_include);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_source);
}
