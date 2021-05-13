/** \file   test-number.cc
 *  \brief  Tests for bc's arbitrary precision arithmetic
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include <catch2/catch.hpp>

#include <sstream>
#include <stdint.h>
#include <string>

#include "bc.hh"
#include <string_view>

TEST_CASE("GD::Bc::Number - Number construction, directed", "[bc][number]")
{
  /* Test input in one base is output correctly in another. */
  auto [in_num, ibase, out_num,
        obase] = GENERATE(table<std::string_view, uint64_t, std::string_view, uint64_t>({
    {"0", 10, "0", 10},
    {"0.0", 10, "0", 10},
    {"1.0", 10, "1.0", 10},
    {".", 10, "0", 10},
    {".1", 10, "0.1", 10},
    {".0", 10, "0", 10},
    {"1", 10, "1", 10},
    {"00000000000000000000000000000000000000000000000000000000000000001", 10, "1", 10},
    {"123456789012345678901234567890", 10, "123456789012345678901234567890", 10},
    {"F", 10, "15", 10},
    {"0.A", 16, "0.6", 10},
    {"0.A0", 16, "0.62", 10},
    {"0.A00", 16, "0.625", 10},
    {"0.A000", 16, "0.6250", 10},
    {"0.A00", 16, "0.A00", 16},
    {"0.A", 16, "0.9", 16},   /* This is a surprise :-) */
    {"0.A0", 16, "0.9E", 16}, /* Another surprise. */
    {"0.1", 10, "0.0001", 2},
    {"12345678901234567890.12345678901234567890", 10,
     " 012 345 678 901 234 567 890. 123 456 789 012 345 678 900", 1000},
    {"123456789123456789.123456789123456789", 10,
     " 123 456 789 123 456 789. 123 456 789 123 456 789", 1000},
    {"FFFFFFFFFFFFFFFF", 16, "18446744073709551615", 10},
    {"FFFFFFFFFFFFFFFF", 16, "1111111111111111111111111111111111111111111111111111111111111111", 2},
    {"FFFFFFFFFFFFFFFF", 16, " 255 255 255 255 255 255 255 255", 256},
    {"FFFFFFFFFFFFFFFF", 16, " 65535 65535 65535 65535", 65536},
    {"FFFFFFFFFFFFFFFF", 16, " 00065535 16777215 16777215", 16777216},
    {".12345678", 10, "0. 12 34 56 78", 100},
    {".12345678", 10, "0. 123 456 780", 1000},
    {".123456789", 10, "0. 123 456 789", 1000},
  }));
  if (obase < GD::Bc::Number::base_) {
    GD::Bc::Number num(in_num, static_cast<GD::Bc::Number::NumType>(ibase));
    std::ostringstream ss;
    std::string expected(out_num);
    num.output(ss, static_cast<GD::Bc::Number::NumType>(obase));
    INFO("in_num = " << in_num << " ibase = " << ibase << " out_num = " << out_num
                     << " obase = " << obase);
    REQUIRE(ss.str() == expected);
  }
}

/* Make a number from a potentially signed string.
 *
 * GD::Bc::Number doesn't handle the sign.
 */
GD::Bc::Number make_number(std::string_view s)
{
  bool negate = false;
  if (s[0] == '-') {
    negate = true;
    s = s.substr(1);
  }
  GD::Bc::Number n(s, 10);
  if (negate) {
    n.negate();
  }
  return n;
}

void test_add_subtract(GD::Bc::Number lhs, GD::Bc::Number rhs, GD::Bc::Number expected_add,
                       GD::Bc::Number expected_sub)
{
  INFO("lhs = " << lhs << " rhs = " << rhs << " expected_add = " << expected_add
                << " expected_sub = " << expected_sub);

  GD::Bc::Number result = lhs;
  result.add(rhs);
  REQUIRE(result == expected_add);

  result.sub(rhs);
  REQUIRE(result == lhs);

  result.sub(rhs);
  REQUIRE(result == expected_sub);

  result.add(rhs);
  REQUIRE(result == lhs);

  result = rhs;
  expected_sub.negate();
  result.add(lhs);
  REQUIRE(result == expected_add);

  result.sub(lhs);
  REQUIRE(result == rhs);

  result.sub(lhs);
  REQUIRE(result == expected_sub);

  result.add(lhs);
  REQUIRE(result == rhs);
}

TEST_CASE("GD::Bc::Number - Addition and subtraction, directed", "[bc][number]")
{
  /* Test input in one base is output correctly in another. */
  auto [lhs, rhs, expected_add, expected_sub] =
    GENERATE(table<std::string_view, std::string_view, std::string_view, std::string_view>({
      {"1", "2", "3", "-1"},
      {"0", "0", "0", "0"},
      {"1", "0", "1", "1"},
      {"0", "1", "1", "-1"},
      {"1", "0.2", "1.2", "0.8"},
      {"999999999", "1", "1000000000", "999999998"},
      {"0.999999999", "0.000000001", "1.000000000", "0.999999998"},
      {"999999999999999999", "1000000000000000000", "1999999999999999999", "-1"},
    }));
  GD::Bc::Number lhs_num = make_number(lhs);
  GD::Bc::Number rhs_num = make_number(rhs);
  GD::Bc::Number expected_add_num = make_number(expected_add);
  GD::Bc::Number expected_sub_num = make_number(expected_sub);

  test_add_subtract(lhs_num, rhs_num, expected_add_num, expected_sub_num);
  rhs_num.negate();
  test_add_subtract(lhs_num, rhs_num, expected_sub_num, expected_add_num);
  lhs_num.negate();
  expected_add_num.negate();
  expected_sub_num.negate();
  test_add_subtract(lhs_num, rhs_num, expected_add_num, expected_sub_num);
  rhs_num.negate();
  test_add_subtract(lhs_num, rhs_num, expected_sub_num, expected_add_num);
}

TEST_CASE("GD::Bc::Number - Scale and length, directed", "[bc][number]")
{
  /* Test input in one base is output correctly in another. */
  auto [num, scale, length] =
    GENERATE(table<std::string_view, GD::Bc::Number::NumType, GD::Bc::Number::NumType>(
      {{"1", 0, 1},
       {"0", 0, 1},
       {"00000000000000000000000000000000000000001", 0, 1},
       {"0.00000000000", 11, 11},
       {"1.0000", 4, 5},
       {"123456789123456789123456789", 0, 27}}));

  GD::Bc::Number n(num, 10);
  REQUIRE(n.scale() == scale);
  REQUIRE(n.length() == length);
}
