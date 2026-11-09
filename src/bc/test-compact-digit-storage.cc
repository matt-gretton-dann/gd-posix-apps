/** \file test-compact-digit-storage.cc
 * Inline/heap transition and explicit COW boundary checks.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <catch2/catch.hpp>

#include <cstdint>
#include <limits>
#include <type_traits>
#include <utility>
#include <vector>

#include "compact-digit-storage.hh"
#include "selected-digit-storage.hh"

TEMPLATE_TEST_CASE("Compact digit storage preserves inline values and heap snapshots",
                   "[bc][number][storage]", std::uint8_t, std::uint16_t, std::uint32_t)
{
  using Storage = GD::Bc::Details::CompactDigitStorage<TestType>;
  Storage absent(nullptr);
  CHECK_FALSE(absent);
  CHECK(absent.empty());
  Storage value;
  CHECK(value);
  CHECK_FALSE(value.has_heap());
  for (unsigned i = 1; i <= Storage::inline_capacity; ++i) {
    value.push_back(static_cast<TestType>(i));
  }
  REQUIRE_FALSE(value.has_heap());
  auto inline_copy = value;
  inline_copy[0] = 9;
  CHECK(value[0] == 1);
  CHECK(inline_copy[0] == 9);

  value.push_back(4);
  REQUIRE(value.has_heap());
  CHECK_FALSE(inline_copy.has_heap());
  auto shared = value;
  CHECK(value.use_count() == 2);
  shared = shared.clone();  // BasicDigits detaches at its mutation boundary.
  CHECK(value.use_count() == 1);
  CHECK(shared.use_count() == 1);
  shared[0] = 8;
  CHECK(value[0] == 1);
  CHECK(shared[0] == 8);

  auto const* before = value.begin();
  value.resize(2);
  CHECK(value.has_heap());
  CHECK(value.begin() == before);  // Shrinking must not relocate live iterators.
  auto small_shared = value;
  small_shared = small_shared.clone();
  CHECK_FALSE(small_shared.has_heap());
  CHECK(small_shared.size() == 2);
  small_shared[0] = 7;
  CHECK(value[0] == 1);
  CHECK(small_shared[0] == 7);

  auto moved_inline = std::move(small_shared);
  CHECK_FALSE(small_shared);
  CHECK(moved_inline[0] == 7);
  auto moved_heap = std::move(value);
  CHECK_FALSE(value);
  CHECK(moved_heap.has_heap());
  CHECK(moved_heap[0] == 1);
  moved_heap.reset();
  CHECK_FALSE(moved_heap);
  moved_heap = Storage{3, 2, 1};
  CHECK_FALSE(moved_heap.has_heap());
  CHECK(moved_heap.size() == 3);
  CHECK(moved_heap.back() == 1);
  CHECK_THROWS_AS(moved_heap.at(3), std::out_of_range);

  Storage prefix{2};
  auto expanded = prefix;
  expanded.resize(Storage::inline_capacity, TestType{9});
  CHECK(expanded[0] == 2);
  CHECK(expanded[1] == 9);
  CHECK(expanded[2] == 9);
  CHECK(prefix.size() == 1);
  expanded.resize(1);
  expanded.resize(Storage::inline_capacity, TestType{5});
  CHECK(expanded[0] == 2);
  CHECK(expanded[1] == 5);
  CHECK(expanded[2] == 5);
  expanded.reset();
  expanded.resize(Storage::inline_capacity, TestType{7});
  CHECK(expanded[0] == 7);
  CHECK(expanded[1] == 7);
  CHECK(expanded[2] == 7);
}

TEMPLATE_TEST_CASE("Compact digit storage assignment covers every active-member transition",
                   "[bc][number][storage]", std::uint8_t, std::uint16_t, std::uint32_t)
{
  using Storage = GD::Bc::Details::CompactDigitStorage<TestType>;
  auto make = [](unsigned state) {
    if (state == 0) {
      return Storage(nullptr);
    }
    if (state == 1) {
      return Storage{1, 2};
    }
    return Storage{1, 2, 3, 4};
  };
  auto values = [](Storage const& value) {
    return std::vector<TestType>(value.begin(), value.end());
  };
  auto self_move = [](Storage& value) {
    auto* alias = std::addressof(value);
    value = std::move(*alias);
  };
  for (unsigned from = 0; from != 3; ++from) {
    for (unsigned to = 0; to != 3; ++to) {
      INFO("source state " << from << ", destination state " << to);
      auto source = make(from);
      auto target = make(to);
      auto const expected = values(source);
      bool const present = static_cast<bool>(source);
      bool const heap = source.has_heap();
      {
        Storage copy(source);
        CHECK(values(copy) == expected);
        CHECK(static_cast<bool>(copy) == present);
      }
      target = source;
      CHECK(values(target) == expected);
      CHECK(values(source) == expected);
      CHECK(static_cast<bool>(target) == present);
      CHECK(target.has_heap() == heap);
      if (heap) {
        CHECK(source.use_count() == 2);
      }
      auto const& alias = target;
      target = alias;
      self_move(target);
      CHECK(values(target) == expected);
      CHECK(static_cast<bool>(target) == present);
      target = make(to);
      target = std::move(source);
      CHECK(values(target) == expected);
      CHECK(static_cast<bool>(target) == present);
      CHECK(target.has_heap() == heap);
      CHECK_FALSE(source);
      CHECK(source.empty());
      CHECK_FALSE(source.has_heap());
      if (heap) {
        CHECK(target.use_count() == 1);
      }
    }
  }
  auto source = make(2);
  auto target = source;
  target = std::move(source);  // Aliased owners still transfer exactly once.
  CHECK_FALSE(source);
  CHECK(target.use_count() == 1);
  CHECK(values(target) == std::vector<TestType>{1, 2, 3, 4});
}

TEMPLATE_TEST_CASE("Compact unique heap mutation preserves identity without preserving value",
                   "[bc][number][storage][divisor-cache]", std::uint8_t, std::uint16_t,
                   std::uint32_t)
{
  using Storage = GD::Bc::Details::CompactDigitStorage<TestType>;
  Storage value{1, 2, 3, 4};
  REQUIRE(value.has_heap());
  REQUIRE(value.use_count() == 1);
  auto const* identity = value.begin();
  value[0] = 9;
  REQUIRE(value.begin() == identity);
  REQUIRE(value[0] == 9);
  REQUIRE(value.use_count() == 1);
  // A shrinking vector can still retain its heap allocation and identity.
  // Admission must bound retained capacity rather than just the used limbs.
  value.resize(2);
  REQUIRE(value.has_heap());
  REQUIRE(value.begin() == identity);
  REQUIRE(value.size() == 2);
}

TEMPLATE_TEST_CASE("Compact weak storage observations preserve unique ownership and expiry",
                   "[bc][number][storage][divisor-cache]", std::uint8_t, std::uint16_t,
                   std::uint32_t)
{
  using Storage = GD::Bc::Details::CompactDigitStorage<TestType>;
  Storage source{1, 2, 3, 4};
  auto weak = source.weak_owner();
  REQUIRE(source.use_count() == 1);
  REQUIRE(source.shares_owner(weak));
  source[0] = 9;
  REQUIRE(source.shares_owner(weak));  // Identity is not immutable content.
  auto copy = source;
  REQUIRE(copy.shares_owner(weak));
  copy = copy.clone();
  REQUIRE_FALSE(copy.shares_owner(weak));
  auto const retained = source.capacity();
  source.resize(1);
  REQUIRE(source.capacity() == retained);
  source.reset();
  REQUIRE(weak.expired());
  Storage replacement{9, 2, 3, 4};
  REQUIRE_FALSE(replacement.shares_owner(weak));
  Storage small{1};
  REQUIRE(small.weak_owner().expired());
  REQUIRE_FALSE(small.shares_owner(weak));
}

TEMPLATE_TEST_CASE("Compact empty heap aliases retain weak lifetime until final release",
                   "[bc][number][storage][compact-storage]", std::uint8_t, std::uint16_t,
                   std::uint32_t)
{
  using Storage = GD::Bc::Details::CompactDigitStorage<TestType>;
  Storage value{1, 2, 3, 4};
  auto weak = value.weak_owner();
  auto const* identity = value.begin();
  auto const capacity = value.capacity();
  value.resize(0);
  REQUIRE(value);
  REQUIRE(value.empty());
  REQUIRE(value.has_heap());
  REQUIRE(value.begin() == identity);
  REQUIRE(value.capacity() == capacity);
  auto alias = value;
  REQUIRE(value.use_count() == 2);
  auto detached = alias.clone();
  REQUIRE(detached);
  REQUIRE(detached.empty());
  REQUIRE_FALSE(detached.has_heap());
  REQUIRE_FALSE(detached.shares_owner(weak));
  value.reset();
  REQUIRE_FALSE(weak.expired());
  alias = std::move(detached);
  REQUIRE(weak.expired());
  REQUIRE_FALSE(detached);
  REQUIRE(alias);
  alias.resize(3, TestType{7});
  REQUIRE(alias.size() == 3);
  for (auto limb : alias) {
    CHECK(limb == 7);
  }
  alias = Storage(nullptr);
  REQUIRE_FALSE(alias);
  REQUIRE(alias.capacity() == 0);
}

TEMPLATE_TEST_CASE("Compact promotion failure preserves presence and coefficient values",
                   "[bc][number][storage][compact-storage]", std::uint8_t, std::uint16_t,
                   std::uint32_t)
{
  using Storage = GD::Bc::Details::CompactDigitStorage<TestType>;
  for (bool present : {false, true}) {
    Storage value = present ? Storage{1, 2} : Storage(nullptr);
    REQUIRE_THROWS_AS(value.resize(std::numeric_limits<std::size_t>::max()), std::length_error);
    REQUIRE(static_cast<bool>(value) == present);
    REQUIRE_FALSE(value.has_heap());
    REQUIRE(value.size() == (present ? 2 : 0));
    if (present) {
      CHECK(value[0] == 1);
      CHECK(value[1] == 2);
    }
    value.resize(3, TestType{9});
    CHECK(value.back() == 9);
  }
}

TEMPLATE_TEST_CASE("Selected coefficient layout requires both opt-in and a smaller object",
                   "[bc][number][storage][compact-storage]", std::uint8_t, std::uint16_t,
                   std::uint32_t)
{
  using namespace GD::Bc::Details;
#if GD_BC_COMPACT_DIGITS
  using Expected =
    std::conditional_t<(sizeof(CompactDigitStorage<TestType>) < sizeof(DigitStorage<TestType>)),
                       CompactDigitStorage<TestType>, DigitStorage<TestType>>;
#else
  using Expected = DigitStorage<TestType>;
#endif
  STATIC_REQUIRE(std::is_same_v<SelectedDigitStorage<TestType>, Expected>);
  STATIC_REQUIRE(std::is_nothrow_copy_constructible_v<CompactDigitStorage<TestType>>);
  STATIC_REQUIRE(std::is_nothrow_move_constructible_v<CompactDigitStorage<TestType>>);
  STATIC_REQUIRE(std::is_nothrow_copy_assignable_v<CompactDigitStorage<TestType>>);
  STATIC_REQUIRE(std::is_nothrow_move_assignable_v<CompactDigitStorage<TestType>>);
  CHECK(sizeof(SelectedDigitStorage<TestType>) <= sizeof(DigitStorage<TestType>));
}

#if GD_BC_PROFILE
TEST_CASE("Compact profile ownership releases limbs before the final weak control block",
          "[bc][number][storage][compact-storage]")
{
  namespace P = GD::Bc::Profile;
  using Storage = GD::Bc::Details::CompactDigitStorage<std::uint32_t>;
  auto const before = P::snapshot().total;
  Storage::WeakOwner weak;
  {
    Storage small{1, 2, 3};
    auto small_copy = small;
    REQUIRE(small_copy[1] == 2);
    CHECK(P::snapshot().total.allocation_calls == before.allocation_calls);
    Storage value{1, 2, 3, 4};
    auto const allocated = P::snapshot().total;
    REQUIRE(allocated.allocation_calls > before.allocation_calls);
    auto alias = value;
    weak = value.weak_owner();
    CHECK(P::snapshot().total.allocation_calls == allocated.allocation_calls);
    REQUIRE(value.use_count() == 2);
    alias = alias.clone();
    REQUIRE(value.use_count() == 1);
    REQUIRE_FALSE(alias.shares_owner(weak));
  }
  REQUIRE(weak.expired());
  auto const pending_weak = P::snapshot().total;
  REQUIRE(pending_weak.live_bytes > before.live_bytes);
  weak.reset();
  auto const after = P::snapshot().total;
  CHECK(after.live_bytes == before.live_bytes);
  CHECK(after.allocation_calls - before.allocation_calls ==
        after.deallocation_calls - before.deallocation_calls);
}
#endif
