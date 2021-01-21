/** \file   libgdsup/support/test-__support_read_le_u32.cc
 *  \brief  Unit tests for __support_read_le_u32()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "support/support.h"

#include <catch2/catch.hpp>
#include <cstdint>

TEST_CASE("__support_read_le_u32", "[support][support_read_le_u32]")
{
  uint64_t test_value = UINT32_C(0x01020304);
  REQUIRE(__support_read_le_u32(reinterpret_cast<const char*>(&test_value)) == test_value);
}
