/** \file test-digit-storage.cc
 * Inline/heap transition and explicit COW boundary checks.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <catch2/catch.hpp>

#include <cstdint>
#include <utility>

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
}
