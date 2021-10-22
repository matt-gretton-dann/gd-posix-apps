/** \file   libcpp/test-location.cc
 *  \brief  Tests for Location
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include <sstream>

#include "error.hh"
#include "file-store.hh"
#include "location.hh"

TEST_CASE("GD::CPP::FileStore - Simple", "[cpp][file-store]")
{
  /* This test has a simple ASCII input and checks that we can handle new-lines and token generation
   * correctly.
   */
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);

  auto fname = std::string("Test");
  auto input = std::string("This is a test\nThis is a second line\n");
  auto is = std::istringstream(input);
  file_store.push_stream(fname, is);

  auto line = GD::CPP::Line{1};
  auto column = GD::CPP::Column{1};
  for (auto c : input) {
    auto const& t = file_store.peek();
    REQUIRE(t.type() == GD::CPP::TokenType::character);
    auto range = t.range();
    REQUIRE(range.size() == 1);
    auto loc = range.begin();
    REQUIRE(file_store.physical_filename(loc) == fname);
    REQUIRE(file_store.physical_line(loc) == line);
    REQUIRE(file_store.physical_column(loc) == column);
    REQUIRE(file_store.logical_filename(loc) == fname);
    REQUIRE(file_store.logical_line(loc) == line);
    REQUIRE(file_store.logical_column(loc) == column);
    REQUIRE(*file_store.range_begin(range) == c);
    REQUIRE(static_cast<std::uint32_t>(*file_store.range_begin(range)) ==
            static_cast<std::uint32_t>(t.character()));
    if (c == '\n') {
      line = static_cast<GD::CPP::Line>(static_cast<std::size_t>(line) + 1);
      column = GD::CPP::Column{1};
    }
    else {
      column = static_cast<GD::CPP::Column>(static_cast<std::size_t>(column) + 1);
    }
    file_store.chew(GD::CPP::TokenType::character);
  }
  REQUIRE(file_store.peek().type() == GD::CPP::TokenType::end_of_include);
  file_store.chew(GD::CPP::TokenType::end_of_include);
  REQUIRE(file_store.peek().type() == GD::CPP::TokenType::end_of_source);
  REQUIRE(file_store.physical_filename(file_store.peek().range().begin()) == "(command line)");
  REQUIRE(os.str().empty());
}

TEST_CASE("GD::CPP::FileStore - Malformed 1", "[cpp][file-store]")
{
  /* Shouldn't start with a character in range [0x80, 0xbf] */
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);

  auto fname = std::string("Test");
  auto input = std::string("\x85");
  auto is = std::istringstream(input);
  file_store.push_stream(fname, is);

  auto const& t = file_store.peek();
  REQUIRE(t.type() == GD::CPP::TokenType::character);
  REQUIRE(t.character() == U'\ufffd');
  auto errs{os.str()};
  REQUIRE(!errs.empty());
  file_store.chew(GD::CPP::TokenType::character);
  REQUIRE(file_store.peek().type() == GD::CPP::TokenType::end_of_include);
  file_store.chew(GD::CPP::TokenType::end_of_include);
  REQUIRE(file_store.peek().type() == GD::CPP::TokenType::end_of_source);
  REQUIRE(file_store.physical_filename(file_store.peek().range().begin()) == "(command line)");
  REQUIRE(errs == os.str());
}

TEST_CASE("GD::CPP::FileStore - Multichar", "[cpp][file-store]")
{
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);

  auto fname = std::string("Test");
  auto input = std::string("\xF0\x9F\x98\x81");
  char32_t output = U'\U0001f601';
  auto is = std::istringstream(input);
  file_store.push_stream(fname, is);

  REQUIRE(file_store.peek() == output);
  file_store.chew(GD::CPP::TokenType::character);
  REQUIRE(file_store.peek().type() == GD::CPP::TokenType::end_of_include);
  file_store.chew(GD::CPP::TokenType::end_of_include);
  REQUIRE(file_store.peek().type() == GD::CPP::TokenType::end_of_source);
  REQUIRE(os.str().empty());
}

TEST_CASE("GD::CPP::FileStore - End of line too early", "[cpp][file-store]")
{
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);

  auto fname = std::string("Test");
  auto input = std::string("\xF0\x9F\x98");
  char32_t output = U'\ufffd';
  auto is = std::istringstream(input);
  file_store.push_stream(fname, is);

  REQUIRE(file_store.peek() == output);
  file_store.chew(GD::CPP::TokenType::character);
  auto errs{os.str()};
  REQUIRE(!errs.empty());
  REQUIRE(file_store.peek().type() == GD::CPP::TokenType::end_of_include);
  file_store.chew(GD::CPP::TokenType::end_of_include);
  REQUIRE(file_store.peek().type() == GD::CPP::TokenType::end_of_source);
  REQUIRE(os.str() == errs);
}

TEST_CASE("GD::CPP::FileStore - Next character too early", "[cpp][file-store]")
{
  std::ostringstream os;
  GD::CPP::ErrorManager error_manager(os);
  GD::CPP::FileStore file_store(error_manager);
  error_manager.file_store(file_store);

  auto fname = std::string("Test");
  auto input = std::string("\xF0\x9F\x98?");
  char32_t output = U'\ufffd';
  auto is = std::istringstream(input);
  file_store.push_stream(fname, is);

  REQUIRE(file_store.peek() == output);
  file_store.chew(GD::CPP::TokenType::character);
  auto errs{os.str()};
  REQUIRE(!errs.empty());
  REQUIRE(file_store.peek() == U'?');
  file_store.chew(GD::CPP::TokenType::character);
  REQUIRE(file_store.peek().type() == GD::CPP::TokenType::end_of_include);
  file_store.chew(GD::CPP::TokenType::end_of_include);
  REQUIRE(file_store.peek().type() == GD::CPP::TokenType::end_of_source);
  REQUIRE(os.str() == errs);
}
