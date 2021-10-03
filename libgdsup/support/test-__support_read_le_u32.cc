/** \file   libgdsup/support/test-__support_read_le_u32.cc
 *  \brief  Unit tests for __support_read_le_u32()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include <cstdint>

#include "support/support.h"

TEST_CASE("__support_read_le_u32", "[support][support_read_le_u32]")
{
  uint64_t test_value = UINT32_C(0x01020304);
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-reinterpret-cast)
  REQUIRE(__support_read_le_u32(reinterpret_cast<char const*>(&test_value)) == test_value);
}
