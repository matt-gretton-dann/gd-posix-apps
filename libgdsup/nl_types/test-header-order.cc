/** \file   libgdsup/nl_types/test-header-order.cc
 *  \brief  Check coexistence with standard library locale headers.
 *  SPDX-License-Identifier: Apache-2.0
 */

#include <locale>

#include "gd/nl_types.h"

#include <catch2/catch.hpp>

#include <cerrno>
#include <type_traits>

#if __has_include(<nl_types.h>)
static_assert(std::is_same_v<decltype(::catopen(nullptr, 0)), nl_catd>);
#endif

TEST_CASE("Catalogue headers coexist with standard locale headers", "[nl_types][headers]")
{
  errno = 0;
  REQUIRE(gd_catclose(static_cast<gd_nl_catd>(-1)) == -1);
  REQUIRE(errno == EBADF);
}
