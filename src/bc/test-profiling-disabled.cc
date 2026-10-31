/** \file test-profiling-disabled.cc
 * The normal-build aliases must preserve types and require no counter runtime.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <memory>
#include <type_traits>
#include <vector>

#include "profiling.hh"

using Allocator = GD::Bc::Profile::Allocator<int, GD::Bc::Profile::AllocationKind::digits>;
static_assert(std::is_same_v<Allocator, std::allocator<int>>);

auto main() -> int
{
  // This executable deliberately does not link profiling.cc. Any accidental
  // normal-build counter reference is therefore also a link-time failure.
  GD_BC_PROFILE_EVENT(GD::Bc::Profile::Event::digit_copies, 1);
  auto values = GD_BC_PROFILE_SHARED(GD::Bc::Profile::AllocationKind::digits, std::vector<int>)();
  static_assert(std::is_same_v<decltype(values), std::shared_ptr<std::vector<int>>>);
  values->push_back(7);
  return values->front() == 7 ? 0 : 1;
}
