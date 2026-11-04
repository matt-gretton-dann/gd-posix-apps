/** \file number-multiply.cc
 *  \brief Isolate the production recursive coefficient multiplication.
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "bc.hh"

void GD::Bc::Details::multiply_recursive32(std::span<::uint32_t const> lhs,
                                           std::span<::uint32_t const> rhs,
                                           std::span<::uint32_t> result,
                                           std::span<::uint32_t> scratch, std::size_t split_point)
{
  LimbProduct<NumberTraits32>::multiply(lhs, rhs, result, scratch, split_point);
}
