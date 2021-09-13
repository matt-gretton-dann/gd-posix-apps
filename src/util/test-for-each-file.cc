/** \file   src/util/test-for-each-file.cc
 *  \brief  Unit tests for for_each_file utils
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "util/file.hh"

#include <catch2/catch.hpp>

#include <array>

#include <string_view>

TEST_CASE("for_each_file", "[util][file][for_each_file]")
{
  std::string file1 = "file1";
  std::string file2 = "file2";
  std::array<std::string, 2> file12 = {file1, file2};

  int call_count = 0;
  std::string files;
  bool success =
    GD::for_each_file(file12.begin(), file12.end(), [&call_count, &files](std::string_view fname) {
      ++call_count;
      files += fname;
      return true;
    });
  REQUIRE(success == true);
  REQUIRE(files == file1 + file2);
  REQUIRE(call_count == file12.size());

  success = GD::for_each_file(file12.end(), file12.end(),
                              [](std::string_view fname) { return fname == "-"; });
  REQUIRE(success == true);

  call_count = 0;
  success =
    GD::for_each_file(file12.begin(), file12.end(), [&call_count](std::string_view /*unused*/) {
      ++call_count;
      return false;
    });
  REQUIRE(success == false);
  REQUIRE(call_count == file12.size());
}

TEST_CASE("for_each_file flags", "[util][file][for_each_file]")
{
  std::array<char*, 0> files{};
  int call_count = 0;
  std::string fnames;
  bool success =
    GD::for_each_file(files.begin(), files.end(), [&call_count, &fnames](std::string_view fname) {
      ++call_count;
      fnames += fname;
      return true;
    });

  REQUIRE(success == true);
  REQUIRE(call_count == 1);
  REQUIRE(fnames == "-");

  success = GD::for_each_file(
    files.begin(), files.end(), [](std::string_view /*unused*/) { return false; },
    GD::FEFFlags::none);
  REQUIRE(success == true);
}
