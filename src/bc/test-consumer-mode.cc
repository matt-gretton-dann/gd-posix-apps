/** \file test-consumer-mode.cc
 * Public-header configuration must follow the linked library usage requirements.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <cstdio>

#include "bc.hh"

static_assert(GD_BC_COMPACT_DIGIT_OWNER == GD_BC_EXPECT_COMPACT_OWNER,
              "Coefficient layout must match the linked library");

auto main() -> int
{
  // The expected value comes independently from CMake; the extension definition
  // must arrive through libbc rather than being repeated on this consumer.
  if constexpr (GD::Bc::extensions_enabled() != (GD_BC_EXPECT_EXTENSIONS != 0)) {
    std::fprintf(stderr, "Configured extension mode %d differs from consumer header mode %d\n",
                 GD_BC_EXPECT_EXTENSIONS, static_cast<int>(GD::Bc::extensions_enabled()));
    return 1;
  }
  return 0;
}
