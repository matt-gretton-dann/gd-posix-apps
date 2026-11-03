/** \file number-division.cc
 * Native ordinary and cached division drivers, sharing generic implementations.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "bc.hh"

namespace GD::Bc {
// The generic header path remains available when ordinary outlining is disabled.
#if GD_BC_OUTLINED_DIVISION
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
#endif

template<>
auto BasicNumber<NumberTraits16>::try_divide_reusing(BasicNumber const& rhs, NumType target_scale,
                                                     DivisorCache& cache) -> bool
{
  return try_divide_reusing_impl<false>(rhs, target_scale, cache);
}

template<>
auto BasicNumber<NumberTraits16>::try_modulo_reusing(BasicNumber const& rhs, NumType target_scale,
                                                     DivisorCache& cache) -> bool
{
  return try_divide_reusing_impl<true>(rhs, target_scale, cache);
}

template<>
auto BasicNumber<NumberTraits32>::try_divide_reusing(BasicNumber const& rhs, NumType target_scale,
                                                     DivisorCache& cache) -> bool
{
  return try_divide_reusing_impl<false>(rhs, target_scale, cache);
}

template<>
auto BasicNumber<NumberTraits32>::try_modulo_reusing(BasicNumber const& rhs, NumType target_scale,
                                                     DivisorCache& cache) -> bool
{
  return try_divide_reusing_impl<true>(rhs, target_scale, cache);
}

}  // namespace GD::Bc
