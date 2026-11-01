/** \file test-digit-storage.cc
 * Inline/heap transition and explicit COW boundary checks.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <catch2/catch.hpp>

#include <cstdint>
#include <utility>
#include <vector>

#include "digit-storage.hh"

TEMPLATE_TEST_CASE("Digit storage preserves inline values and heap snapshots",
                   "[bc][number][storage]", std::uint8_t, std::uint16_t, std::uint32_t)
{
  using Storage = GD::Bc::Details::DigitStorage<TestType>;
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
  CHECK(value.use_count() == 1);  // Experimental eager-copy policy.
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

TEMPLATE_TEST_CASE("Digit storage assignment covers every active-member transition",
                   "[bc][number][storage]", std::uint8_t, std::uint16_t, std::uint32_t)
{
  using Storage = GD::Bc::Details::DigitStorage<TestType>;
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
        CHECK(source.use_count() == 1);  // Experimental eager-copy policy.
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
