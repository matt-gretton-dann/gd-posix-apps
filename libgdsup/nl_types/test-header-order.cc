/** \file   libgdsup/nl_types/test-header-order.cc
 *  \brief  Check coexistence with standard library locale headers.
 *  SPDX-License-Identifier: Apache-2.0
 */

#include <locale>

#include "gd/nl_types.h"

#include <catch2/catch.hpp>

#include <cerrno>

TEST_CASE("Catalogue headers coexist with standard locale headers", "[nl_types][headers]")
{
  errno = 0;
  REQUIRE(catclose(static_cast<nl_catd>(-1)) == -1);
  REQUIRE(errno == EBADF);
}
