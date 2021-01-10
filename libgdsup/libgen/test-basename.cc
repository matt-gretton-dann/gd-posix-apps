/** \file   libgdsup/libgen/test-basename.cc
 *  \brief  Main file for gdsup unit tests
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/libgen.h"

#include <catch2/catch.hpp>
#include <cstring>

TEST_CASE("basename - POSIX Examples", "[libgen][basename]")
{
  char* dup = strdup("");
  REQUIRE(dup != nullptr);
  REQUIRE(std::strcmp(basename(nullptr), ".") == 0);
  REQUIRE(std::strcmp(basename(dup), ".") == 0);
}
