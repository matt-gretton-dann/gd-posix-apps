/** \file   test-reader.cc
 *  \brief  Tests for Reader
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

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
