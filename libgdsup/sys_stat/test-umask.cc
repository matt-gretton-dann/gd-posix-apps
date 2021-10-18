#include "gd/sys/stat.h"

#include <catch2/catch.hpp>

// NOLINTNEXTLINE
TEST_CASE("umask-test", "[sys_stat][umask]")
{
  mode_t m = 0123;
  mode_t c = umask(m);
  REQUIRE(umask(m) == m);
  REQUIRE(umask(c) == m);
  REQUIRE(umask(c) == c);
}
