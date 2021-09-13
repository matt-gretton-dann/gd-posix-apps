/** \file   libgdsup/nl_types/test-catclose.cc
 *  \brief  Unit tests for catclose()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include <catch2/catch.hpp>

TEST_CASE("catclose", "[nl_types][catclose]")
{
  errno = 0;
  REQUIRE(::catclose((nl_catd)-1) == -1);
  REQUIRE(errno == EBADF);

  errno = 0;
  REQUIRE(::catclose((nl_catd)NULL) == -1);
  REQUIRE(errno == EBADF);

  constexpr size_t buf_size = 24;
  std::array<char, buf_size> buf = {0};
  errno = 0;
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-reinterpret-cast)
  REQUIRE(::catclose(reinterpret_cast<nl_catd>(buf.data())) == -1);
  REQUIRE(errno == EBADF);
}
