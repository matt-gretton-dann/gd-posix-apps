/** \file number-division-cache.cc
 * Native cached arithmetic entries with a generic implementation for all traits.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "bc.hh"

namespace GD::Bc {
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
