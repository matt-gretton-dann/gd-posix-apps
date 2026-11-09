/** \file test-consumer-mode.cc
 * Public-header configuration must follow the linked library usage requirements.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <cstdint>
#include <cstdio>

#include "bc.hh"
#include "selected-digit-storage.hh"

static_assert(GD_BC_COMPACT_DIGITS == GD_BC_EXPECT_COMPACT_DIGITS,
              "Coefficient layout must propagate from the linked library");

auto main() -> int
{
  // The expected value comes independently from CMake; the extension definition
  // must arrive through libbc rather than being repeated on this consumer.
  if constexpr (GD::Bc::extensions_enabled() != (GD_BC_EXPECT_EXTENSIONS != 0)) {
    std::fprintf(stderr, "Configured extension mode %d differs from consumer header mode %d\n",
                 GD_BC_EXPECT_EXTENSIONS, static_cast<int>(GD::Bc::extensions_enabled()));
    return 1;
  }
  std::printf(
    "Compact coefficient option %d; selected storage bytes %zu/%zu/%zu; Number bytes %zu\n",
    GD_BC_COMPACT_DIGITS, sizeof(GD::Bc::Details::SelectedDigitStorage<std::uint8_t>),
    sizeof(GD::Bc::Details::SelectedDigitStorage<std::uint16_t>),
    sizeof(GD::Bc::Details::SelectedDigitStorage<std::uint32_t>), sizeof(GD::Bc::Number));
  return 0;
}
