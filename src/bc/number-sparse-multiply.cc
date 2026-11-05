/** \file number-sparse-multiply.cc
 * Native instantiation of sparse multiplication for default limbs.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "bc.hh"

#if defined(GD_BC_OUTLINED_SPARSE_MULTIPLY)
namespace GD::Bc::Details {
template auto
BasicDigits<NumberTraits32>::multiply_sparse(std::span<NumberTraits32::NumType const> lhs,
                                             std::span<NumberTraits32::NumType const> rhs)
  -> std::optional<BasicDigits<NumberTraits32>>;
}  // namespace GD::Bc::Details
#endif
