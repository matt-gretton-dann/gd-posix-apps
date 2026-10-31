/** \file   libgdsup/nl_types/test-catclose.cc
 *  \brief  Unit tests for gd_catclose()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include <catch2/catch.hpp>

#include <array>
#include <locale>
#include <type_traits>

#if __has_include(<nl_types.h>)
static_assert(std::is_same_v<decltype(::catopen("", 0)), nl_catd>);
#endif

TEST_CASE("gd_catclose", "[nl_types][gd_catclose]")
{
  errno = 0;
  REQUIRE(::gd_catclose((gd_nl_catd)-1) == -1);
  REQUIRE(errno == EBADF);

  errno = 0;
  REQUIRE(::gd_catclose((gd_nl_catd)NULL) == -1);
  REQUIRE(errno == EBADF);

  constexpr size_t buf_size = 24;
  std::array<char, buf_size> buf = {0};
  errno = 0;
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-reinterpret-cast)
  REQUIRE(::gd_catclose(reinterpret_cast<gd_nl_catd>(buf.data())) == -1);
  REQUIRE(errno == EBADF);
}
