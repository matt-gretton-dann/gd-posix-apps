/** \file   libcpp/test-newline-chewer.cc
 *  \brief  Tests for GD::CPP::NewLineChewer
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include <sstream>

#include "error.hh"
#include "file-store.hh"
#include "location.hh"
#include "tokenizers.hh"

TEST_CASE("GD::CPP::NewLineChewer - Empty include", "[cpp][new-line-chewer]")
{
  /* Check that we don't error on an empty file.
   */
  GD::CPP::ErrorManager error_manager;
  GD::CPP::FileStore file_store(error_manager);
  auto tokenizer = GD::CPP::NewLineChewer(file_store, error_manager);

  auto fname = std::string("Test");
  auto input = std::string("");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  GD::CPP::Token const& t = tokenizer.peek();
  GD::CPP::Range r = t.range();
  REQUIRE(t.type() == GD::CPP::TokenType::end_of_source);
  REQUIRE(file_store.physical_filename(r.begin()) == "(command line)");
}

TEST_CASE("GD::CPP::NewLineChewer - Missing new line", "[cpp][new-line-chewer]")
{
  /* Check we error on missing newline at end of file. */
  GD::CPP::ErrorManager error_manager;
  GD::CPP::FileStore file_store(error_manager);
  auto tokenizer = GD::CPP::NewLineChewer(file_store, error_manager);

  auto fname = std::string("Test");
  auto input = std::string("a");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  GD::CPP::Token const& t = tokenizer.peek();
  GD::CPP::Range r = t.range();
  REQUIRE(t.type() == GD::CPP::TokenType::character);
  REQUIRE(r.size() == 1);
  REQUIRE(*file_store.range_begin(r) == 'a');

  tokenizer.chew();
  GD::CPP::Token const& t2 = tokenizer.peek();
  GD::CPP::Range r2 = t2.range();
  REQUIRE(t2.type() == GD::CPP::TokenType::error);
  REQUIRE(t2.get<GD::CPP::Error>().severity() == GD::CPP::ErrorSeverity::error);
  REQUIRE(file_store.physical_filename(r2.begin()) == fname);

  tokenizer.chew();
  GD::CPP::Token const& t3 = tokenizer.peek();
  GD::CPP::Range r3 = t3.range();
  REQUIRE(t3.type() == GD::CPP::TokenType::end_of_line);
  REQUIRE(file_store.physical_filename(r3.begin()) == fname);

  tokenizer.chew();
  GD::CPP::Token const& t4 = tokenizer.peek();
  GD::CPP::Range r4 = t4.range();
  REQUIRE(t4.type() == GD::CPP::TokenType::end_of_source);
  REQUIRE(file_store.physical_filename(r4.begin()) == "(command line)");
}

TEST_CASE("GD::CPP::NewLineChewer - new line", "[cpp][new-line-chewer]")
{
  /* Check we error on missing newline at end of file. */
  GD::CPP::ErrorManager error_manager;
  GD::CPP::FileStore file_store(error_manager);
  auto tokenizer = GD::CPP::NewLineChewer(file_store, error_manager);

  auto fname = std::string("Test");
  auto input = std::string("a\n");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  GD::CPP::Token const& t = tokenizer.peek();
  GD::CPP::Range r = t.range();
  REQUIRE(t.type() == GD::CPP::TokenType::character);
  REQUIRE(r.size() == 1);
  REQUIRE(*file_store.range_begin(r) == 'a');

  tokenizer.chew();
  GD::CPP::Token const& t3 = tokenizer.peek();
  GD::CPP::Range r3 = t3.range();
  REQUIRE(t3.type() == GD::CPP::TokenType::end_of_line);
  REQUIRE(file_store.physical_filename(r3.begin()) == fname);

  tokenizer.chew();
  GD::CPP::Token const& t4 = tokenizer.peek();
  GD::CPP::Range r4 = t4.range();
  REQUIRE(t4.type() == GD::CPP::TokenType::end_of_source);
  REQUIRE(file_store.physical_filename(r4.begin()) == "(command line)");
}

TEST_CASE("GD::CPP::NewLineChewer - splice at eof is an error", "[cpp][new-line-chewer]")
{
  /* Check we error on a splice at the end of a file. */
  GD::CPP::ErrorManager error_manager;
  GD::CPP::FileStore file_store(error_manager);
  auto tokenizer = GD::CPP::NewLineChewer(file_store, error_manager);

  auto fname = std::string("Test");
  auto input = std::string("a\\\n");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  GD::CPP::Token const& t = tokenizer.peek();
  GD::CPP::Range r = t.range();
  REQUIRE(t.type() == GD::CPP::TokenType::character);
  REQUIRE(r.size() == 1);
  REQUIRE(*file_store.range_begin(r) == 'a');

  tokenizer.chew();
  GD::CPP::Token const& t2 = tokenizer.peek();
  GD::CPP::Range r2 = t2.range();
  REQUIRE(t2.type() == GD::CPP::TokenType::error);
  REQUIRE(t2.get<GD::CPP::Error>().severity() == GD::CPP::ErrorSeverity::error);
  REQUIRE(file_store.physical_filename(r2.begin()) == fname);

  tokenizer.chew();
  GD::CPP::Token const& t3 = tokenizer.peek();
  GD::CPP::Range r3 = t3.range();
  REQUIRE(t3.type() == GD::CPP::TokenType::end_of_line);
  REQUIRE(file_store.physical_filename(r3.begin()) == fname);

  tokenizer.chew();
  GD::CPP::Token const& t4 = tokenizer.peek();
  GD::CPP::Range r4 = t4.range();
  REQUIRE(t4.type() == GD::CPP::TokenType::end_of_source);
  REQUIRE(file_store.physical_filename(r4.begin()) == "(command line)");
}

TEST_CASE("GD::CPP::NewLineChewer - splice working", "[cpp][new-line-chewer]")
{
  /* Check we error on a splice at the end of a file. */
  GD::CPP::ErrorManager error_manager;
  GD::CPP::FileStore file_store(error_manager);
  auto tokenizer = GD::CPP::NewLineChewer(file_store, error_manager);

  auto fname = std::string("Test");
  auto input = std::string("a\\\nb\n");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  GD::CPP::Token const& t = tokenizer.peek();
  GD::CPP::Range r = t.range();
  REQUIRE(t.type() == GD::CPP::TokenType::character);
  REQUIRE(r.size() == 1);
  REQUIRE(*file_store.range_begin(r) == 'a');

  tokenizer.chew();
  GD::CPP::Token const& t2 = tokenizer.peek();
  GD::CPP::Range r2 = t2.range();
  REQUIRE(t2.type() == GD::CPP::TokenType::character);
  REQUIRE(r2.size() == 1);
  REQUIRE(*file_store.range_begin(r2) == 'b');

  tokenizer.chew();
  GD::CPP::Token const& t3 = tokenizer.peek();
  GD::CPP::Range r3 = t3.range();
  REQUIRE(t3.type() == GD::CPP::TokenType::end_of_line);
  REQUIRE(file_store.physical_filename(r3.begin()) == fname);

  tokenizer.chew();
  GD::CPP::Token const& t4 = tokenizer.peek();
  GD::CPP::Range r4 = t4.range();
  REQUIRE(t4.type() == GD::CPP::TokenType::end_of_source);
  REQUIRE(file_store.physical_filename(r4.begin()) == "(command line)");
}

TEST_CASE("GD::CPP::NewLineChewer - not a splice", "[cpp][new-line-chewer]")
{
  /* Check we error on a splice at the end of a file. */
  GD::CPP::ErrorManager error_manager;
  GD::CPP::FileStore file_store(error_manager);
  auto tokenizer = GD::CPP::NewLineChewer(file_store, error_manager);

  auto fname = std::string("Test");
  auto input = std::string("a\\b\n");
  auto is = std::istringstream(input);
  tokenizer.push_stream(fname, is);

  GD::CPP::Token const& t = tokenizer.peek();
  GD::CPP::Range r = t.range();
  REQUIRE(t.type() == GD::CPP::TokenType::character);
  REQUIRE(r.size() == 1);
  REQUIRE(*file_store.range_begin(r) == 'a');

  tokenizer.chew();
  GD::CPP::Token const& ts = tokenizer.peek();
  GD::CPP::Range rs = ts.range();
  REQUIRE(ts.type() == GD::CPP::TokenType::character);
  REQUIRE(rs.size() == 1);
  REQUIRE(*file_store.range_begin(rs) == '\\');

  tokenizer.chew();
  GD::CPP::Token const& t2 = tokenizer.peek();
  GD::CPP::Range r2 = t2.range();
  REQUIRE(t2.type() == GD::CPP::TokenType::character);
  REQUIRE(r2.size() == 1);
  REQUIRE(*file_store.range_begin(r2) == 'b');

  tokenizer.chew();
  GD::CPP::Token const& t3 = tokenizer.peek();
  GD::CPP::Range r3 = t3.range();
  REQUIRE(t3.type() == GD::CPP::TokenType::end_of_line);
  REQUIRE(file_store.physical_filename(r3.begin()) == fname);

  tokenizer.chew();
  GD::CPP::Token const& t4 = tokenizer.peek();
  GD::CPP::Range r4 = t4.range();
  REQUIRE(t4.type() == GD::CPP::TokenType::end_of_source);
  REQUIRE(file_store.physical_filename(r4.begin()) == "(command line)");
}
