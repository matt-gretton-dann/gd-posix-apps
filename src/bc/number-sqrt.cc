/** \file number-sqrt.cc
 * Native-width prefix-root kernels isolated from instruction dispatch.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "bc.hh"

namespace GD::Bc {
template<>
auto BasicNumber<NumberTraits16>::sqrt_prefix_if_large(NumType working_scale, NumType target_scale)
  -> bool
{
  return sqrt_prefix_impl(working_scale, target_scale);
}

template<>
auto BasicNumber<NumberTraits32>::sqrt_prefix_if_large(NumType working_scale, NumType target_scale)
  -> bool
{
  return sqrt_prefix_impl(working_scale, target_scale);
}
}  // namespace GD::Bc
