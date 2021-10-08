/** \file   test-member-header.cc
 *  \brief  Tests for GD::Ar::Details::MemberHeader
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include "gd/span.hh"

#include <catch2/catch.hpp>

#include <sstream>

#include "ar.hh"

TEST_CASE("GD::Ar::Details::MemberHeader - Construction BSD", "[ar][member-header]")
{
  std::string name = "file            ";
  std::string bsd_header = "123456789012"
                           "1     "
                           "    10"
                           "0660    "
                           "1234567890"
                           "`\n";
  GD::MemorySpanInputFile file((GD::Span::span<char>(bsd_header.data(), bsd_header.size())));
  GD::Ar::Details::MemberHeader mh(file, name, GD::Ar::Format::bsd, nullptr);

  REQUIRE(mh.name() == std::string("file"));
  REQUIRE(mh.mtime() == 123456789012);
  REQUIRE(mh.uid() == 1);
  REQUIRE(mh.gid() == 10);
  REQUIRE(mh.mode() == 0660);
  REQUIRE(mh.size() == 1234567890);
  REQUIRE(mh.header_size() == bsd_header.size() + name.size());
}

TEST_CASE("GD::Ar::Details::MemberHeader - Construction BSD Long", "[ar][member-header]")
{
  std::string name = "#1/20           ";
  std::string bsd_header = "123456789012"
                           "1     "
                           "    10"
                           "  0660  "
                           "1234567890"
                           "`\n"
                           "12345678901234567890";
  GD::MemorySpanInputFile file((GD::Span::span<char>(bsd_header.data(), bsd_header.size())));
  GD::Ar::Details::MemberHeader mh(file, name, GD::Ar::Format::bsd, nullptr);

  REQUIRE(mh.name() == std::string("12345678901234567890"));
  REQUIRE(mh.mtime() == 123456789012);
  REQUIRE(mh.uid() == 1);
  REQUIRE(mh.gid() == 10);
  REQUIRE(mh.mode() == 0660);
  REQUIRE(mh.size() == 1234567890 - 20);
  REQUIRE(mh.header_size() == bsd_header.size() + name.size());
}

TEST_CASE("GD::Ar::Details::MemberHeader - Construction GNU", "[ar][member-header]")
{
  std::string name = "Fred in a shed/ ";
  std::string gnu_header = "123456789012"
                           "1     "
                           "    10"
                           "0660    "
                           "1234567890"
                           "`\n";
  GD::MemorySpanInputFile file((GD::Span::span<char>(gnu_header.data(), gnu_header.size())));
  GD::Ar::Details::MemberHeader mh(file, name, GD::Ar::Format::gnu, nullptr);

  REQUIRE(mh.name() == std::string("Fred in a shed"));
  REQUIRE(mh.mtime() == 123456789012);
  REQUIRE(mh.uid() == 1);
  REQUIRE(mh.gid() == 10);
  REQUIRE(mh.mode() == 0660);
  REQUIRE(mh.size() == 1234567890);
  REQUIRE(mh.header_size() == gnu_header.size() + name.size());
}
