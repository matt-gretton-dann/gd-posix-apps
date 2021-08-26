/** \file   src/util/test-write-endian.cc
 *  \brief  Unit tests for (read|write)_[bl]e
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "util/utils.hh"

#include <catch2/catch.hpp>

#include <vector>

TEST_CASE("write_be", "[util][write_be]")
{
  std::vector<std::byte> result;
  std::vector<std::byte> expected({std::byte{3}, std::byte{2}, std::byte{1}, std::byte{0},
                                   std::byte{6}, std::byte{5}, std::byte{0x8}, std::byte{0x9},
                                   std::byte{0xa}, std::byte{0xb}, std::byte{0xc}, std::byte{0xd},
                                   std::byte{0xe}, std::byte{0xf}});
  auto it = std::back_inserter(result);
  GD::write_be(it, std::uint32_t{0x03020100});
  GD::write_be(it, std::uint16_t{0x0605});
  GD::write_be(it, std::uint64_t{0x08090a0b0c0d0e0f});
  REQUIRE_THAT(result, Catch::Matchers::Equals(expected));
}

TEST_CASE("write_le", "[util][write_le]")
{
  std::vector<std::byte> result;
  std::vector<std::byte> expected({std::byte{3}, std::byte{2}, std::byte{1}, std::byte{0},
                                   std::byte{6}, std::byte{5}, std::byte{0x8}, std::byte{0x9},
                                   std::byte{0xa}, std::byte{0xb}, std::byte{0xc}, std::byte{0xd},
                                   std::byte{0xe}, std::byte{0xf}});
  auto it = std::back_inserter(result);
  GD::write_le(it, std::uint32_t{0x00010203});
  GD::write_le(it, std::uint16_t{0x0506});
  GD::write_le(it, std::uint64_t{0x0f0e0d0c0b0a0908});
  REQUIRE_THAT(result, Catch::Matchers::Equals(expected));
}

TEST_CASE("read_be", "[util][read_be]")
{
  std::vector<std::byte> values({std::byte{1}, std::byte{2}, std::byte{3}, std::byte{4},
                                 std::byte{5}, std::byte{6}, std::byte{7}, std::byte{8},
                                 std::byte{9}});
  auto it = values.begin();
  REQUIRE(GD::read_be<std::uint8_t>(it) == 0x01);
  REQUIRE(GD::read_be<std::uint16_t>(it) == 0x0102);
  REQUIRE(GD::read_be<std::uint32_t>(it) == 0x01020304);
  REQUIRE(GD::read_be<std::uint64_t>(it) == 0x0102030405060708);
}

TEST_CASE("read_le", "[util][read_le]")
{
  std::vector<std::byte> values({std::byte{1}, std::byte{2}, std::byte{3}, std::byte{4},
                                 std::byte{5}, std::byte{6}, std::byte{7}, std::byte{8},
                                 std::byte{9}});
  auto it = values.begin();
  REQUIRE(GD::read_le<std::uint8_t>(it) == 0x01);
  REQUIRE(GD::read_le<std::uint16_t>(it) == 0x0201);
  REQUIRE(GD::read_le<std::uint32_t>(it) == 0x04030201);
  REQUIRE(GD::read_le<std::uint64_t>(it) == 0x0807060504030201);
}
