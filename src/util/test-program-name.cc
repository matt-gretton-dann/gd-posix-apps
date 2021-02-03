/** \file   src/util/test-program-name.cc
 *  \brief  Unit tests for program_name utils
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "util/utils.hh"

#include <catch2/catch.hpp>

TEST_CASE("program_name", "[util][program_name]")
{
  char test1[] = "A/B";
  char test2[] = "SED";
  GD::program_name(test1);
  REQUIRE(GD::program_name() == "B");

  GD::program_name(test2);
  REQUIRE(GD::program_name() == "SED");
}
