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
  std::string errs;
  std::ostringstream os(errs);
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
    REQUIRE(file_store.peek().type() == GD::CPP::TokenType::character);
    auto range = file_store.peek().range();
    REQUIRE(range.size() == 1);
    auto loc = range.begin();
    REQUIRE(file_store.physical_filename(loc) == fname);
    REQUIRE(file_store.physical_line(loc) == line);
    REQUIRE(file_store.physical_column(loc) == column);
    REQUIRE(file_store.logical_filename(loc) == fname);
    REQUIRE(file_store.logical_line(loc) == line);
    REQUIRE(file_store.logical_column(loc) == column);
    REQUIRE(*file_store.range_begin(range) == c);
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
  REQUIRE(errs.empty());
}
