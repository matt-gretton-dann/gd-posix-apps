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
#include "location.hh"
#include "preprocessor-tokenizer.hh"

TEST_CASE("GD::CPP::PreprocessorTokenizer - Whitespace", "[cpp][preprocessor-tokenizer]")
{
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);
  auto tokenizer = GD::CPP::PreprocessorTokenizer(file_store, error_manager);

  auto fname = std::string("Test");
  auto input = std::string("a \t \f \v b");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  REQUIRE(tokenizer.peek() == U'a');
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::white_space);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == U'b');
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_include);
  tokenizer.chew();
  REQUIRE(tokenizer.peek() == GD::CPP::TokenType::end_of_source);
}
