/** \file   libcpp/test-identifier-manager.cc
 *  \brief  Tests for Location
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include <catch2/catch.hpp>

#include "identifier-manager.hh"

TEST_CASE("GD::CPP::IdentifierManager - basic", "[cpp][identifier-manager]")
{
  GD::CPP::IdentifierManager id_manager;
  auto [in, display] = GENERATE(table<std::u32string, std::string>(
    {{U"HelloWorld", "HelloWorld"}, {U"\U0001f601", "\\U0001f601"}, {U"\ufffd", "\\ufffd"}}));

  auto id = id_manager.id(in);
  REQUIRE(id_manager.display_name(id) == display);
  REQUIRE(id_manager.id(in) == id);
}
