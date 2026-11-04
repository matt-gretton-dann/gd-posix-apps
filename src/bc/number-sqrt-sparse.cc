/** \file number-sqrt-sparse.cc
 * Isolate exact sparse root construction from the Newton kernel.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "bc.hh"

namespace GD::Bc {
template<>
auto BasicNumber<NumberTraits16>::sqrt_sparse_exact(std::size_t width, NumType target_scale) -> bool
{
  return sqrt_sparse_exact_impl(width, target_scale);
}

template<>
auto BasicNumber<NumberTraits32>::sqrt_sparse_exact(std::size_t width, NumType target_scale) -> bool
{
  return sqrt_sparse_exact_impl(width, target_scale);
}
}  // namespace GD::Bc
