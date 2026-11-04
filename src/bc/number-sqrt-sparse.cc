/** \file number-sqrt-sparse.cc
 * Isolate exact sparse root construction from the Newton kernel.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "bc.hh"

namespace GD::Bc {
template<>
auto BasicNumber<NumberTraits16>::sqrt_sparse_exact(NumType working_scale, NumType target_scale) -> bool
{
  return sqrt_sparse_exact_impl(working_scale, target_scale);
}

template<>
auto BasicNumber<NumberTraits32>::sqrt_sparse_exact(NumType working_scale, NumType target_scale) -> bool
{
  return sqrt_sparse_exact_impl(working_scale, target_scale);
}
}  // namespace GD::Bc
