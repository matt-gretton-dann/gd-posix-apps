/** \file   libgdsup/libgen/test-dirname.cc
 *  \brief  Unit tests for dirname()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/libgen.h"
#include "gd/string.h"

#include <catch2/catch.hpp>
#include <cstring>

namespace {
void test_dirname(char const* in, char const* expected)
{
  REQUIRE(in != nullptr);
  REQUIRE(expected != nullptr);
  char* dup = ::strdup(in);
  REQUIRE(dup != nullptr);
  char* out = dirname(dup);
  CHECK(std::strcmp(out, expected) == 0);
}

}  // namespace

TEST_CASE("dirname - POSIX Examples", "[libgen][dirname]")
{
  REQUIRE(std::strcmp(dirname(nullptr), ".") == 0);
  test_dirname("", ".");
  test_dirname("usr", ".");
  test_dirname("usr/", ".");
  test_dirname("/", "/");
  test_dirname("//", "//");
  test_dirname("///", "/");
  test_dirname("/usr/", "/");
  test_dirname("/usr/lib", "/usr");
  test_dirname("//usr//lib//", "//usr");
  test_dirname("/home//dwc//test", "/home//dwc");
}
