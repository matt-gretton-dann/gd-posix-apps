/** \file   src/bc/test-format.cc
 *  \brief  Exercise custom formatters with either formatting backend.
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include "bc.hh"

#include <catch2/catch.hpp>

TEST_CASE("bc custom formatters", "[bc][format]")
{
  GD::Bc::Number const number("123", 10);
  GD::Bc::Letter const letter('a');
  GD::Bc::Token const token(GD::Bc::Token::Type::add);
  REQUIRE(fmt::format("{}", number) == "123");
  REQUIRE(fmt::format("{}", letter) == "a");
  REQUIRE(fmt::format("{}", token) == "+");
}
