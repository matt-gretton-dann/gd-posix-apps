/** \file profiling.cc
 * Counter storage is linked only by profiling builds and its isolated tests.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "profiling.hh"

#include <algorithm>
#include <atomic>

namespace GD::Bc::Profile {
namespace {
Snapshot counters{};
std::atomic_flag gate = ATOMIC_FLAG_INIT;

// atomic_flag is guaranteed lock-free, including targets without native
// 64-bit atomic arithmetic. The guarded counters themselves remain 64-bit.
class Guard
{
public:
  Guard() noexcept
  {
    while (gate.test_and_set(std::memory_order_acquire)) {
    }
  }
  ~Guard() { gate.clear(std::memory_order_release); }
  Guard(Guard const&) = delete;
  auto operator=(Guard const&) -> Guard& = delete;
};

void add_allocation(AllocationStats& values, std::size_t bytes) noexcept
{
  ++values.allocation_calls;
  values.allocated_bytes += bytes;
  values.live_bytes += bytes;
  values.peak_live_bytes = std::max(values.peak_live_bytes, values.live_bytes);
}
void remove_allocation(AllocationStats& values, std::size_t bytes) noexcept
{
  ++values.deallocation_calls;
  values.live_bytes -= bytes;
}
}  // namespace

void allocated(AllocationKind kind, std::size_t bytes) noexcept
{
  Guard const guard;
  add_allocation(counters.total, bytes);
  add_allocation(counters.allocations[static_cast<std::size_t>(kind)], bytes);
}
void deallocated(AllocationKind kind, std::size_t bytes) noexcept
{
  Guard const guard;
  remove_allocation(counters.total, bytes);
  remove_allocation(counters.allocations[static_cast<std::size_t>(kind)], bytes);
}
void event(Event which, std::uint64_t amount) noexcept
{
  Guard const guard;
  counters.events[static_cast<std::size_t>(which)] += amount;
}
auto snapshot() noexcept -> Snapshot
{
  Guard const guard;
  return counters;
}
auto name(AllocationKind kind) noexcept -> char const*
{
  constexpr std::array names{"digits", "arrays", "instructions", "frames", "results"};
  static_assert(names.size() == static_cast<std::size_t>(AllocationKind::count));
  return names[static_cast<std::size_t>(kind)];
}
auto name(Event which) noexcept -> char const*
{
  constexpr std::array names{"digit_copies",
                             "nonempty_digit_copies",
                             "digit_moves",
                             "iterator_owner_copies",
                             "cow_clones",
                             "cow_limb_bytes",
                             "range_limb_bytes",
                             "basic_multiply_calls",
                             "karatsuba_multiply_calls",
                             "literal_conversions",
                             "function_calls",
                             "instructions_executed",
                             "array_clones",
                             "array_entries_copied"};
  static_assert(names.size() == static_cast<std::size_t>(Event::count));
  return names[static_cast<std::size_t>(which)];
}
}  // namespace GD::Bc::Profile
