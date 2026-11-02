/** \file number-remainder.cc
 * Native-width scaled remainder kernels isolated from instruction dispatch.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "bc.hh"

namespace GD::Bc {
template<>
void BasicNumber<NumberTraits16>::modulo(BasicNumber rhs, NumType target_scale)
{
  modulo_impl(std::move(rhs), target_scale);
}

template<>
void BasicNumber<NumberTraits32>::modulo(BasicNumber rhs, NumType target_scale)
{
  modulo_impl(std::move(rhs), target_scale);
}
}  // namespace GD::Bc
