/** \file   libgdsup/libgen/test-basename.cc
 *  \brief  Unit tests for basename()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/libgen.h"
#include "gd/string.h"

#include <catch2/catch.hpp>
#include <cstring>

namespace {
void test_basename(char const* in, char const* expected)
{
  REQUIRE(in != nullptr);
  REQUIRE(expected != nullptr);
  char* dup = ::strdup(in);
  REQUIRE(dup != nullptr);
  char* out = basename(dup);
  CHECK(std::strcmp(out, expected) == 0);
}

}  // namespace

TEST_CASE("basename - POSIX Examples", "[libgen][basename]")
{
  REQUIRE(std::strcmp(basename(nullptr), ".") == 0);
  test_basename("", ".");
  test_basename("usr", "usr");
  test_basename("usr/", "usr");
  test_basename("/", "/");
  test_basename("//", "//");
  test_basename("///", "/");
  test_basename("/usr/", "usr");
  test_basename("/usr/lib", "lib");
  test_basename("//usr//lib//", "lib");
  test_basename("/home//dwc//test", "test");
}
