/** \file   test-number.cc
 *  \brief  Tests for bc's arbitrary precision arithmetic
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include <catch2/catch.hpp>

#include <sstream>
#include <string>

#include "bc.hh"
#include <string_view>

TEST_CASE("GD::Bc::Number - Number construction, directed", "[bc][number]")
{
  /* Test input in one base is output correctly in another. */
  auto [in_num, ibase, out_num, obase] = GENERATE(
    table<std::string_view, GD::Bc::Number::NumType, std::string_view, GD::Bc::Number::NumType>({
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
      {"FFFFFFFFFFFFFFFF", 16, "1111111111111111111111111111111111111111111111111111111111111111",
       2},
      {"FFFFFFFFFFFFFFFF", 16, " 255 255 255 255 255 255 255 255", 256},
      {"FFFFFFFFFFFFFFFF", 16, " 65535 65535 65535 65535", 65536},
      {"FFFFFFFFFFFFFFFF", 16, " 00065535 16777215 16777215", 16777216},
    }));
  GD::Bc::Number num(in_num, ibase);
  std::ostringstream ss;
  std::string expected(out_num);
  num.output(ss, obase);
  REQUIRE(ss.str() == expected);
}
