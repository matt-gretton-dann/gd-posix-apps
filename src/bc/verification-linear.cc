/** \file verification-linear.cc
 * Verify straight-line outer streams without allocating a graph.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "verification-linear.hh"

auto GD::Bc::Details::is_verified_linear_stream(Instructions const& code) -> bool
{
  return verify_linear_stream(code);
}
