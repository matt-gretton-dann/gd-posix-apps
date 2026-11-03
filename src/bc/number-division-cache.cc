/** \file number-division-cache.cc
 * Native cached arithmetic entries with a generic implementation for all traits.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "bc.hh"

namespace GD::Bc {
template<>
void BasicNumber<NumberTraits16>::divide_reusing(BasicNumber rhs, NumType target_scale,
                                                 DivisorCache& cache)
{
  divide_reusing_impl<false>(std::move(rhs), target_scale, cache);
}

template<>
void BasicNumber<NumberTraits16>::modulo_reusing(BasicNumber rhs, NumType target_scale,
                                                 DivisorCache& cache)
{
  divide_reusing_impl<true>(std::move(rhs), target_scale, cache);
}

template<>
void BasicNumber<NumberTraits32>::divide_reusing(BasicNumber rhs, NumType target_scale,
                                                 DivisorCache& cache)
{
  divide_reusing_impl<false>(std::move(rhs), target_scale, cache);
}

template<>
void BasicNumber<NumberTraits32>::modulo_reusing(BasicNumber rhs, NumType target_scale,
                                                 DivisorCache& cache)
{
  divide_reusing_impl<true>(std::move(rhs), target_scale, cache);
}

}  // namespace GD::Bc
