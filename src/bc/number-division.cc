/** \file number-division.cc
 * Native-width division drivers isolated from instruction dispatch.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "bc.hh"

namespace GD::Bc {
template<>
void BasicNumber<NumberTraits16>::divide(BasicNumber rhs, NumType target_scale)
{
  divide_impl(std::move(rhs), target_scale);
}

template<>
void BasicNumber<NumberTraits32>::divide(BasicNumber rhs, NumType target_scale)
{
  divide_impl(std::move(rhs), target_scale);
}
}  // namespace GD::Bc
