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

TEMPLATE_TEST_CASE("Number formatters check runtime debug and parse boundaries",
                   "[bc][number][format][context][oracle]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  Number const value("42", 10);
  REQUIRE(fmt::vformat("{}", fmt::make_format_args(value)) == "42");
  REQUIRE(fmt::vformat("{:d}", fmt::make_format_args(value)) == "Number({42}, sign=+, scale=0)");
  REQUIRE_THROWS_AS(fmt::vformat("{:x}", fmt::make_format_args(value)), fmt::format_error);
  Number negative = value;
  negative.negate();
  REQUIRE(fmt::vformat("{}", fmt::make_format_args(negative)) == "-42");
  REQUIRE(fmt::vformat("{:d}", fmt::make_format_args(negative)) == "Number({42}, sign=-, scale=0)");

  fmt::formatter<Number> empty_formatter;
  fmt::format_parse_context empty("");
  REQUIRE(empty_formatter.parse(empty) == empty.end());
  fmt::formatter<Number> missing_end_formatter;
  fmt::format_parse_context missing_end("d");
  REQUIRE_THROWS_AS(missing_end_formatter.parse(missing_end), fmt::format_error);
  fmt::formatter<Number> invalid_formatter;
  fmt::format_parse_context invalid("x}");
  REQUIRE_THROWS_AS(invalid_formatter.parse(invalid), fmt::format_error);
}
