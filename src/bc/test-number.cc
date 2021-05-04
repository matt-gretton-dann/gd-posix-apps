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

TEST_CASE("GD::Bc::Number - Number construction", "[bc][number]")
{
  /* Test input in one base is output correctly in another. */
  auto [in_num, ibase, out_num, obase] = GENERATE(
    table<std::string_view, GD::Bc::Number::NumType, std::string_view, GD::Bc::Number::NumType>({
      {"0", 10, "0", 10},
      {"0.0", 10, "0", 10},
      {"1.0", 10, "1.0", 10},
      {"1", 10, "1", 10},
      {"00000000000000000000000000000000000000000000000000000000000000001", 10, "1", 10},
      {"123456789012345678901234567890", 10, "123456789012345678901234567890", 10},
      {"F", 10, "15", 10},
    }));
  GD::Bc::Number num(in_num, ibase);
  std::ostringstream ss;
  std::string expected(out_num);
  expected.push_back('\n');
  num.output(ss, obase);
  REQUIRE(ss.str() == expected);
}
