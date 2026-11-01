/** \file   test-reader.cc
 *  \brief  Tests for Reader
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include <new>

#include <catch2/catch.hpp>

#include <limits>
#include <sstream>

#include "bc.hh"

TEST_CASE("GD::Bc::Reader - peeking", "[bc][reader]")
{
  GD::Bc::StringReader r("ABCD");

  REQUIRE(r.peek() == 'A');
  r.chew();
  REQUIRE(r.peek() == 'B');
  r.chew();
  r.chew();
  REQUIRE(r.peek() == 'D');
  r.chew();
  REQUIRE(r.peek() == EOF);
  r.chew();
  REQUIRE(r.peek() == EOF);
}

TEST_CASE("Reader tracks lines without advancing on peek", "[bc][reader][coverage]")
{
  GD::Bc::StringReader reader("A\nB");
  REQUIRE(reader.location() == GD::Bc::Location("Library", 1, 1));
  REQUIRE(reader.peek() == 'A');
  REQUIRE(reader.peek() == 'A');
  REQUIRE(reader.location() == GD::Bc::Location("Library", 1, 1));
  reader.chew();
  REQUIRE(reader.peek() == '\n');
  REQUIRE(reader.location() == GD::Bc::Location("Library", 1, 2));
  reader.chew();
  REQUIRE(reader.peek() == 'B');
  REQUIRE(reader.location() == GD::Bc::Location("Library", 2, 1));
  reader.chew();
  REQUIRE(reader.peek() == EOF);
  REQUIRE(reader.peek() == EOF);
  REQUIRE(reader.location() == GD::Bc::Location("Library", 2, 2));
}

TEST_CASE("Location saturation and comparison independence", "[bc][reader][coverage]")
{
  using GD::Bc::Location;
  auto const max_line = std::numeric_limits<Location::Line>::max();
  auto const max_column = std::numeric_limits<Location::Column>::max();
  Location location("input", max_line - 1, max_column - 1);
  location.next_column();
  REQUIRE(location.column() == max_column);
  location.next_column();
  REQUIRE(location.column() == max_column);
  location.next_line();
  REQUIRE(location.line() == max_line);
  REQUIRE(location.column() == 1);
  location.next_line();
  REQUIRE(location.line() == max_line);
  REQUIRE(location.column() == 1);

  Location const reference("file", 42, 7);
  REQUIRE(reference == Location("file", 42, 7));
  REQUIRE(reference != Location("other", 42, 7));
  REQUIRE(reference != Location("file", 43, 7));
  REQUIRE(reference != Location("file", 42, 8));
  std::ostringstream output;
  output << reference;
  REQUIRE(output.str() == "file:42:7");
}
