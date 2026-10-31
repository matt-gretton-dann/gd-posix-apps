/** \file test-number-output.cc
 * Output positions must preserve values and stay within POSIX line bounds.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include <sstream>
#include <string>

#include "bc.hh"

namespace {
auto unwrapped(std::string text) -> std::string
{
  for (auto at = text.find("\\\n"); at != std::string::npos; at = text.find("\\\n")) {
    text.erase(at, 2);
  }
  if (!text.empty() && text.front() == '\n') {
    text.erase(0, 1);
  }
  return text;
}

void check_lines(std::string const& text, unsigned initial_position)
{
  unsigned column = initial_position;
  for (char c : text) {
    if (c == '\n') {
      CHECK(column <= 70);
      column = 0;
    }
    else {
      ++column;
    }
  }
  CHECK(column <= 70);
}
}  // namespace

TEMPLATE_TEST_CASE("Number output preserves values at line boundaries",
                   "[bc][number][oracle][output-position]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using N = typename Number::NumType;
  unsigned const position = GENERATE(0U, 67U, 68U, 69U, 70U);
  N const radix = GENERATE(N(10), N(16));
  bool const negative = GENERATE(false, true);
  bool const fractional = GENERATE(false, true);
  INFO("position = " << position << ", radix = " << static_cast<unsigned>(radix)
                     << ", negative = " << negative << ", fractional = " << fractional
                     << ", limb base = " << Number::base_);
  Number value(fractional ? "0.5" : "42", 10);
  if (negative) {
    value.negate();
  }
  std::string expected = fractional ? (GD::Bc::extensions_enabled() ? "." : "0.") : "";
  expected += fractional ? (radix == 10 ? "5" : "8") : (radix == 10 ? "42" : "2A");
  if (negative) {
    expected.insert(expected.begin(), '-');
  }
  std::ostringstream output;
  REQUIRE_NOTHROW(value.output(output, radix, position));
  CHECK(unwrapped(output.str()) == expected);
  check_lines(output.str(), position);
}

TEMPLATE_TEST_CASE("Digit output helpers accept boundary positions",
                   "[bc][number][oracle][output-position]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Digits = GD::Bc::Details::BasicDigits<TestType>;
  using N = typename Digits::NumType;
  unsigned const position = GENERATE(0U, 67U, 68U, 69U, 70U);
  bool const decimal = GENERATE(false, true);
  Digits const value(N(5));
  std::ostringstream output;
  INFO("position = " << position << ", decimal = " << decimal << ", base = " << Digits::base_);
  if (decimal) {
    REQUIRE_NOTHROW(value.output_base10(output, N(1), position));
  }
  else {
    REQUIRE_NOTHROW(value.output(output, N(16), N(1), position));
  }
  std::string expected = GD::Bc::extensions_enabled() ? "." : "0.";
  expected += decimal ? '5' : '8';
  CHECK(unwrapped(output.str()) == expected);
  check_lines(output.str(), position);
}

TEMPLATE_TEST_CASE("Nondecimal continuation keeps established zero-position wrapping",
                   "[bc][number][oracle][output-position]", GD::Bc::NumberTraits16,
                   GD::Bc::NumberTraits32)
{
  // This value exceeds the small trait's existing decimal-length domain; wider
  // traits cover the final full continuation chunk without changing that limit.
  using Number = GD::Bc::BasicNumber<TestType>;
  Number const value(std::string(137, 'A'), 16);
  std::ostringstream output;
  REQUIRE_NOTHROW(value.output(output, 16, 0));
  REQUIRE(output.str() == std::string(68, 'A') + "\\\n" + std::string(68, 'A') + "\\\nA");
}

TEMPLATE_TEST_CASE("Long decimal output respects partial-line capacity",
                   "[bc][number][oracle][output-position]", GD::Bc::NumberTraits8,
                   GD::Bc::NumberTraits16, GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  unsigned const position = GENERATE(67U, 68U, 69U, 70U);
  bool const negative = GENERATE(false, true);
  bool const fractional = GENERATE(false, true);
  std::string const digits(80, '1');  // Still inside the small trait's supported length/scale.
  Number value(fractional ? "0." + digits : digits, 10);
  std::string expected = fractional ? (GD::Bc::extensions_enabled() ? "." : "0.") + digits : digits;
  if (negative) {
    value.negate();
    expected.insert(expected.begin(), '-');
  }
  INFO("position = " << position << ", negative = " << negative << ", fractional = " << fractional
                     << ", base = " << Number::base_);
  std::ostringstream output;
  REQUIRE_NOTHROW(value.output(output, 10, position));
  CHECK(unwrapped(output.str()) == expected);
  check_lines(output.str(), position);
}
