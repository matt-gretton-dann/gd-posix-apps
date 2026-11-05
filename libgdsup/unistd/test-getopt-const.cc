// SPDX-License-Identifier: Apache-2.0
#include "gd/libgen.h"
#include "gd/stdlib.h"
#include "gd/string.h"
#include "gd/unistd.h"

#include <cstdio>
#include <cstring>

#include <catch2/catch.hpp>

namespace {
auto const_search(char const* text, int character) noexcept -> char const*
{
  return std::strchr(text, character);
}

namespace ConstGetopt {
// Compile the actual implementation with a const-preserving search, including
// on C toolchains whose legacy strchr declaration still returns mutable char*.
// Private symbols keep this copy independent of the linked supplemental API.
#define strchr const_search
#include "getopt.c"
#undef strchr
}  // namespace ConstGetopt
}  // namespace

TEST_CASE("getopt preserves const in option lookup", "[unistd][getopt]")
{
  char program[] = "program";
  char argument[] = "-avalue";
  char flag[] = "-b";
  char operand[] = "operand";
  char* argv[] = {program, argument, flag, operand, nullptr};
  constexpr char options[] = "a:b";
  ConstGetopt::optarg = nullptr;
  ConstGetopt::opterr = 0;
  ConstGetopt::optind = 1;
  ConstGetopt::optopt = 0;
  CHECK(ConstGetopt::getopt(4, argv, options) == 'a');
  REQUIRE(ConstGetopt::optarg != nullptr);
  CHECK(std::strcmp(ConstGetopt::optarg, "value") == 0);
  CHECK(ConstGetopt::getopt(4, argv, options) == 'b');
  CHECK(ConstGetopt::getopt(4, argv, options) == -1);
  CHECK(ConstGetopt::optind == 3);
  CHECK(std::strcmp(options, "a:b") == 0);
}
