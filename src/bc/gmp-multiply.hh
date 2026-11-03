/** Experimental GMP multiplication of base-10^9 coefficients.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef SRC_BC_GMP_MULTIPLY_HH_INCLUDED
#define SRC_BC_GMP_MULTIPLY_HH_INCLUDED

#include <cstdint>
#include <span>
#include <vector>

namespace GD::Bc::Details {
/** Multiply nonempty little-endian base-10^9 coefficients exactly. */
auto gmp_multiply(std::span<std::uint32_t const> lhs, std::span<std::uint32_t const> rhs)
  -> std::vector<std::uint32_t>;
}

#endif
