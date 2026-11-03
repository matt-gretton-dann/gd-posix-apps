/** Public-API fixtures for retained-capacity admission checks.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_TEST_DIVISOR_CACHE_FIXTURES_HH
#define GD_BC_TEST_DIVISOR_CACHE_FIXTURES_HH

#include <algorithm>
#include <cstddef>
#include <limits>
#include <utility>

namespace GD::Bc::Test {
/** Leave room for one aligned limb in preflight, while normal vector growth
 * during COW alignment/normalization can exceed the final retained budget.
 * Exact-growth standard libraries may instead admit a bounded value.
 */
template<typename Digits>
auto capacity_growth_source() -> std::pair<Digits, std::size_t>
{
  using Limb = typename Digits::NumType;
  constexpr auto limit = Digits::DivisorCache::max_retained_limb_bytes / sizeof(Limb);
  constexpr auto limbs = sizeof(Limb) == 1 ? std::size_t{14000} : limit / 3 - 2;
  constexpr auto places = (limbs - 1) * Digits::base_log10_;
  Digits value(Limb{1});
  for (auto remaining = places; remaining != 0;) {
    auto const part =
      std::min(remaining, static_cast<std::size_t>(std::numeric_limits<Limb>::max()));
    value.mul_pow10(static_cast<Limb>(part));
    remaining -= part;
  }
  return {std::move(value), places};
}
}  // namespace GD::Bc::Test
#endif
