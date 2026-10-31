/** \file test-profiling.cc
 * Validate allocation lifetime, rebinding, alignment and shared ownership.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <cstdint>
#include <iostream>
#include <map>
#include <vector>

#include "profiling.hh"

namespace {
unsigned failures = 0;
void check(bool value, char const* message)
{
  if (!value) {
    std::cerr << message << '\n';
    ++failures;
  }
}
}  // namespace

auto main() -> int
{
  namespace P = GD::Bc::Profile;
  using K = P::AllocationKind;
  auto const initial = P::snapshot();
  {
    using Values = std::vector<int, P::CountingAllocator<int, K::digits>>;
    Values values;
    values.reserve(8);
    values.push_back(7);
    auto copied = values;
    copied[0] = 9;
    check(values[0] == 7, "vector copies must retain their independent value");
    auto const current = P::snapshot();
    auto const& digits = current.allocations[static_cast<std::size_t>(K::digits)];
    check(digits.allocation_calls >= 2, "vector allocations were not recorded");
    check(digits.live_bytes >= 9 * sizeof(int), "live vector storage was not recorded");

    using Pair = std::pair<int const, int>;
    std::map<int, int, std::less<int>, P::CountingAllocator<Pair, K::arrays>> nodes;
    nodes.emplace(1, 2);
    nodes.emplace(3, 4);
    check(P::snapshot().allocations[static_cast<std::size_t>(K::arrays)].allocation_calls >= 2,
          "map node allocator rebinding lost its category");

    auto shared = GD_BC_PROFILE_SHARED(K::instructions, Values)();
    shared->push_back(11);
    auto const before_copy = P::snapshot();
    auto alias = shared;
    check(P::snapshot().total.allocation_calls == before_copy.total.allocation_calls,
          "copying a shared handle must not allocate storage");
    shared.reset();
    check(alias->at(0) == 11, "shared storage must survive the first handle");

    struct alignas(64) Aligned
    {
      std::uint64_t value;
    };
    P::CountingAllocator<Aligned, K::results> allocator;
    auto* storage = allocator.allocate(2);
    check(reinterpret_cast<std::uintptr_t>(storage) % alignof(Aligned) == 0,
          "counting must preserve over-aligned allocation");
    allocator.deallocate(storage, 2);
  }
  auto const final = P::snapshot();
  check(final.total.live_bytes == initial.total.live_bytes, "tracked storage was not released");
  check(final.total.allocation_calls == final.total.deallocation_calls,
        "allocation/deallocation calls must balance");
  check(final.total.peak_live_bytes > 0, "simultaneous peak storage must be recorded");
  for (auto const& category : final.allocations) {
    check(category.live_bytes == 0, "a category retained released storage");
    check(category.peak_live_bytes <= final.total.peak_live_bytes,
          "category peak exceeds the simultaneous total peak");
  }
  P::event(P::Event::cow_limb_bytes, 128);
  check(P::snapshot().events[static_cast<std::size_t>(P::Event::cow_limb_bytes)] == 128,
        "event payload was not preserved");
  return failures == 0 ? 0 : 1;
}
