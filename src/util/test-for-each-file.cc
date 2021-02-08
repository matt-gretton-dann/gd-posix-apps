/** \file   src/util/test-for-each-file.cc
 *  \brief  Unit tests for for_each_file utils
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "util/file.hh"

#include <catch2/catch.hpp>

#include <string_view>

TEST_CASE("for_each_file", "[util][file][for_each_file]")
{
  char file1[] = "file1";
  char file2[] = "file2";
  char* file12[] = {file1, file2, nullptr};

  int call_count = 0;
  std::string files;
  bool success = GD::for_each_file(2, file12, [&call_count, &files](std::string_view fname) {
    ++call_count;
    files += fname;
    return true;
  });
  REQUIRE(success == true);
  REQUIRE(files == "file1file2");
  REQUIRE(call_count == 2);

  success = GD::for_each_file(0, file12 + 2, [](std::string_view fname) { return fname == "-"; });
  REQUIRE(success == true);

  call_count = 0;
  success = GD::for_each_file(2, file12, [&call_count](std::string_view) {
    ++call_count;
    return false;
  });
  REQUIRE(success == false);
  REQUIRE(call_count == 2);
}
