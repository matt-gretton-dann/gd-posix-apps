/** \file   test-member-header.cc
 *  \brief  Tests for GD::Ar::Details::MemberHeader
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include <sstream>

#include "ar.hh"

TEST_CASE("GD::Ar::Details::MemberHeader - Construction BSD", "[ar][member-header]")
{
  std::string bsd_header = "file            "
                           "123456789012"
                           "1     "
                           "    10"
                           "0660    "
                           "1234567890"
                           "`\n";
  GD::Ar::MemorySpanInputFile file(std::span(bsd_header.begin(), bsd_header.end()));
  GD::Ar::Details::MemberHeader mh(file, GD::Ar::Format::bsd, nullptr);

  REQUIRE(mh.name() == std::string("file"));
  REQUIRE(mh.mtime() == 123456789012);
  REQUIRE(mh.uid() == 1);
  REQUIRE(mh.gid() == 10);
  REQUIRE(mh.mode() == 0660);
  REQUIRE(mh.size() == 1234567890);
  REQUIRE(mh.header_size() == bsd_header.size());
}

TEST_CASE("GD::Ar::Details::MemberHeader - Construction BSD Long", "[ar][member-header]")
{
  std::string bsd_header = "#1/20           "
                           "123456789012"
                           "1     "
                           "    10"
                           "  0660  "
                           "1234567890"
                           "`\n"
                           "12345678901234567890";
  GD::Ar::MemorySpanInputFile file(std::span(bsd_header.begin(), bsd_header.end()));
  GD::Ar::Details::MemberHeader mh(file, GD::Ar::Format::bsd, nullptr);

  REQUIRE(mh.name() == std::string("12345678901234567890"));
  REQUIRE(mh.mtime() == 123456789012);
  REQUIRE(mh.uid() == 1);
  REQUIRE(mh.gid() == 10);
  REQUIRE(mh.mode() == 0660);
  REQUIRE(mh.size() == 1234567890);
  REQUIRE(mh.header_size() == bsd_header.size());
}

TEST_CASE("GD::Ar::Details::MemberHeader - Construction GNU", "[ar][member-header]")
{
  std::string gnu_header = "Fred in a shed/ "
                           "123456789012"
                           "1     "
                           "    10"
                           "0660    "
                           "1234567890"
                           "`\n";
  GD::Ar::MemorySpanInputFile file(std::span(gnu_header.begin(), gnu_header.end()));
  GD::Ar::Details::MemberHeader mh(file, GD::Ar::Format::gnu, nullptr);

  REQUIRE(mh.name() == std::string("Fred in a shed"));
  REQUIRE(mh.mtime() == 123456789012);
  REQUIRE(mh.uid() == 1);
  REQUIRE(mh.gid() == 10);
  REQUIRE(mh.mode() == 0660);
  REQUIRE(mh.size() == 1234567890);
  REQUIRE(mh.header_size() == gnu_header.size());
}
