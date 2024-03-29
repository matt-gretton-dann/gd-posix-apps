/** \file   libgdsup/support/test-__support_read_le_u64.cc
 *  \brief  Unit tests for __support_read_le_u64()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include <cstdint>

#include "support/support.h"

TEST_CASE("__support_read_le_u64", "[support][support_read_le_u64]")
{
  uint64_t test_value = UINT64_C(0x0102030405060708);
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-reinterpret-cast)
  REQUIRE(__support_read_le_u64(reinterpret_cast<const char*>(&test_value)) == test_value);
}
