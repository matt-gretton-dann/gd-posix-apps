/** \file   test-location.cc
 *  \brief  Tests for Location
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include "location.hh"

TEST_CASE("GD::CPP::Location", "[cpp][location]")
{
  GD::CPP::Location loc{0};

  GD::CPP::Range range(loc);
  REQUIRE(range.begin() == loc);
  REQUIRE(range.size() == 1);
}
