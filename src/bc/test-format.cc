/** \file   src/bc/test-format.cc
 *  \brief  Exercise custom formatters with either formatting backend.
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include <catch2/catch.hpp>

#include "bc.hh"

TEST_CASE("bc custom formatters", "[bc][format]")
{
  GD::Bc::Number const number("123", 10);
  GD::Bc::Letter const letter('a');
  GD::Bc::Token const token(GD::Bc::Token::Type::add);
  REQUIRE(fmt::format("{}", number) == "123");
  REQUIRE(fmt::format("{}", letter) == "a");
  REQUIRE(fmt::format("{}", token) == "+");
}

TEST_CASE("bc formatters accept an empty parse context", "[bc][format][context]")
{
  // vformat supplies a closing brace even for {}, so call the public parser
  // interface directly to exercise its genuinely empty-range boundary.
  fmt::format_parse_context token_context("");
  fmt::formatter<GD::Bc::Token> token_formatter;
  REQUIRE(token_formatter.parse(token_context) == token_context.end());
  fmt::format_parse_context letter_context("");
  fmt::formatter<GD::Bc::Letter> letter_formatter;
  REQUIRE(letter_formatter.parse(letter_context) == letter_context.end());
}
