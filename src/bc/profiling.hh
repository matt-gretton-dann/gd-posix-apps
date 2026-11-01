/** \file profiling.hh
 * Scoped allocation and ownership counters for separate diagnostic builds.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_PROFILING_HH
#define GD_BC_PROFILING_HH

#include <array>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <type_traits>
#include <utility>

namespace GD::Bc::Profile {
enum class AllocationKind : std::size_t {
  digits,
  arrays,
  instructions,
  frames,
  results,
  literals,
  execution_plans,
  count
};
enum class Event : std::size_t {
  digit_copies,
  owned_digit_copies,
  digit_moves,
  iterator_owner_copies,
  cow_clones,
  cow_limb_bytes,
  range_limb_bytes,
  basic_multiply_calls,
  karatsuba_multiply_calls,
  square_multiply_calls,
  literal_conversions,
  literal_cache_hits,
  literal_cache_misses,
  single_digit_literals,
  function_calls,
  instructions_executed,
  array_clones,
  array_entries_copied,
  comparison_limb_positions,
  zero_scan_limbs,
  length_scan_limbs,
  blocked_multiply_calls,
  result_validation_calls,
  radix_output_divisions,
  radix_dc_input_calls,
  radix_dc_input_leaf_conversions,
  temporary_consumptions,
  division_requests,
  division_dividend_limbs,
  division_divisor_limbs,
  sqrt_iterations,
  sqrt_working_scale_sum,
  count
};

struct AllocationStats
{
  std::uint64_t allocation_calls = 0;
  std::uint64_t deallocation_calls = 0;
  std::uint64_t allocated_bytes = 0;
  std::uint64_t live_bytes = 0;
  std::uint64_t peak_live_bytes = 0;
};

struct Snapshot
{
  AllocationStats total;
  std::array<AllocationStats, static_cast<std::size_t>(AllocationKind::count)> allocations{};
  std::array<std::uint64_t, static_cast<std::size_t>(Event::count)> events{};
};

void initialize_report() noexcept;
void allocated(AllocationKind kind, std::size_t bytes) noexcept;
void deallocated(AllocationKind kind, std::size_t bytes) noexcept;
void event(Event which, std::uint64_t amount = 1) noexcept;
auto snapshot() noexcept -> Snapshot;
auto name(AllocationKind kind) noexcept -> char const*;
auto name(Event which) noexcept -> char const*;

/** Track requested storage through std::allocator, including rebound nodes.
 * No global new/delete replacement or private allocation header is involved.
 * Counts describe these selected containers, not all process malloc traffic.
 */
template<typename T, AllocationKind Kind>
class CountingAllocator
{
public:
  using value_type = T;
  using is_always_equal = std::true_type;
  template<typename U>
  struct rebind
  {
    using other = CountingAllocator<U, Kind>;
  };

  CountingAllocator() noexcept = default;
  template<typename U>
  CountingAllocator(CountingAllocator<U, Kind> const&) noexcept
  {
  }

  [[nodiscard]] auto allocate(std::size_t count) -> T*
  {
    auto* result = std::allocator<T>{}.allocate(count);
    allocated(Kind, count * sizeof(T));
    return result;
  }

  void deallocate(T* pointer, std::size_t count) noexcept
  {
    std::allocator<T>{}.deallocate(pointer, count);
    deallocated(Kind, count * sizeof(T));
  }

  template<typename U>
  auto operator==(CountingAllocator<U, Kind> const&) const noexcept -> bool
  {
    return true;
  }
  template<typename U>
  auto operator!=(CountingAllocator<U, Kind> const&) const noexcept -> bool
  {
    return false;
  }
};

template<typename T, AllocationKind Kind>
struct SharedFactory
{
  template<typename... Args>
  static auto make(Args&&... args) -> std::shared_ptr<T>
  {
    return std::allocate_shared<T>(CountingAllocator<T, Kind>{}, std::forward<Args>(args)...);
  }
};

#if GD_BC_PROFILE
// Profile builds deliberately use tracked containers; ordinary timing builds
// retain exactly their original allocator types and direct make_shared calls.
template<typename T, AllocationKind Kind>
using Allocator = CountingAllocator<T, Kind>;
#  define GD_BC_PROFILE_EVENT(Which, Amount) ::GD::Bc::Profile::event(Which, Amount)
#  define GD_BC_PROFILE_SHARED(Kind, Type) ::GD::Bc::Profile::SharedFactory<Type, Kind>::make
#else
template<typename T, AllocationKind Kind>
using Allocator = std::allocator<T>;
#  define GD_BC_PROFILE_EVENT(Which, Amount) ((void)0)
#  define GD_BC_PROFILE_SHARED(Kind, Type) std::make_shared<Type>
#endif
}  // namespace GD::Bc::Profile
#endif
