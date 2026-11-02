/** \file test-array-storage.cc
 * Dense/sparse storage oracles independent of VM parsing and array lookup.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <catch2/catch.hpp>

#include <map>
#include <random>
#include <string>
#include <utility>

#include "bc.hh"

TEST_CASE("Selected array layout agrees with an ordered-map value oracle",
          "[bc][vm][arrays][array-storage]")
{
  using Storage = GD::Bc::ArrayStorage;
  using Number = GD::Bc::Number;
  using Index = GD::Bc::ArrayIndex;
  Index const high = Number::base_ - 1;
  Index const middle = high / 2;
  Storage actual;
  std::map<Index, Index> expected;
  auto verify = [&](Index key) {
    auto const* found = actual.lookup(key);
    auto const ref = expected.find(key);
    CHECK((found != nullptr) == (ref != expected.end()));
    if (found)
      CHECK(*found == Number(ref->second));
    CHECK(actual.size() == expected.size());
    CHECK(actual.empty() == expected.empty());
    CHECK(actual.length() == (expected.empty() ? 0U : expected.rbegin()->first + 1));
  };
  verify(0);
  auto write = [&](Index key, Index value) {
    actual.insert_or_assign(key, Number(value));
    expected.insert_or_assign(key, value);
    verify(key);
  };
  // Sparse entries inside a future dense prefix must migrate exactly once.
  for (auto key : {Index(100), Index(127), Index(255), middle, high})
    write(key, 0);
  for (Index key = 0; key < 1024; ++key)
    write(key, key % 23);
  std::mt19937 random(197209);
  for (unsigned i = 0; i < 3000; ++i) {
    Index const key = (i % 3 == 0) ? Index(middle + random() % middle) : Index(random() % 2048);
    write(key, Index(random() % 31));
    verify(Index(random() % 4096));
    verify(high - 1);
  }
  // Copying the container preserves value snapshots even for heap coefficients.
  actual.insert_or_assign(0, Number("123456789012345678901234567890", 10));
  auto copy = actual;
  copy.insert_or_assign(0, Number(7));
  CHECK(*actual.lookup(0) == Number("123456789012345678901234567890", 10));
  CHECK(*copy.lookup(0) == Number(7));
  auto moved = std::move(copy);
  CHECK(copy.empty());
  CHECK(copy.lookup(0) == nullptr);
  CHECK(*moved.lookup(0) == Number(7));
  copy = moved;
  CHECK(*copy.lookup(0) == Number(7));
}

TEST_CASE("Array assignment protects a value borrowed before dense growth",
          "[bc][vm][arrays][array-storage]")
{
  GD::Bc::ArrayStorage values;
  GD::Bc::Number expected("123456789012345678901234567890", 10);
  values.insert_or_assign(0, expected);
  for (GD::Bc::ArrayIndex i = 1; i < 300; ++i) {
    values.insert_or_assign(i, *values.lookup(0));
    CHECK(*values.lookup(i) == expected);
  }
  CHECK(values.size() == 300);
  CHECK(values.length() == 300);
}

#if GD_BC_PROFILE
TEST_CASE("Sparse high writes do not materialize their index range",
          "[bc][vm][arrays][array-storage][array-profile]")
{
  namespace P = GD::Bc::Profile;
  auto stats = [] {
    return P::snapshot().allocations[static_cast<std::size_t>(P::AllocationKind::arrays)];
  };
  auto before = stats();
  {
    GD::Bc::ArrayStorage values;
    GD::Bc::ArrayIndex const high = GD::Bc::Number::base_ - 1;
    CHECK(values.lookup(high) == nullptr);
    CHECK(stats().allocation_calls == before.allocation_calls);
    values.insert_or_assign(high, GD::Bc::Number(0));
    CHECK(values.length() == GD::Bc::Number::base_);
    CHECK(values.size() == 1);
// A single fallback node is bounded independently of the written index.
#  if GD_BC_ARRAY_STORAGE == 1
    // A paged candidate charges one complete page even for a single key.
    CHECK(stats().allocated_bytes - before.allocated_bytes <
          64 * sizeof(std::optional<GD::Bc::Number>) + 1024);
#  else
    CHECK(stats().allocated_bytes - before.allocated_bytes < 1024);
#  endif
  }
  CHECK(stats().live_bytes == before.live_bytes);
}
#endif

TEMPLATE_TEST_CASE("Hybrid arrays preserve each numeric index width",
                   "[bc][vm][arrays][array-storage]", GD::Bc::NumberTraits8, GD::Bc::NumberTraits16,
                   GD::Bc::NumberTraits32)
{
  using Number = GD::Bc::BasicNumber<TestType>;
  using Index = typename Number::NumType;
  GD::Bc::Details::HybridArrayStorage<Number> values;
  Index const high = TestType::base_ - 1;
  values.insert_or_assign(high, Number(Index(0)));
  for (unsigned i = 0; i < 32; ++i) {
    values.insert_or_assign(Index(i), Number(Index(i % 7)));
  }
  for (unsigned i = 0; i < 32; ++i) {
    REQUIRE(values.lookup(Index(i)) != nullptr);
    CHECK(*values.lookup(Index(i)) == Number(Index(i % 7)));
  }
  CHECK(values.size() == 33);
  CHECK(values.length() == TestType::base_);
  values.insert_or_assign(high, Number(Index(1)));
  CHECK(values.size() == 33);
  CHECK(*values.lookup(high) == Number(Index(1)));
  auto snapshot = values;
  values.insert_or_assign(Index(0), Number(Index(7)));
  CHECK(*snapshot.lookup(Index(0)) == Number(Index(0)));
  CHECK(*values.lookup(Index(0)) == Number(Index(7)));
}

namespace {
template<typename Storage, typename Number>
void check_storage_candidate()
{
  using Index = typename Number::NumType;
  Index const high = Number::base_ - 1;
  Storage actual;
  std::map<Index, unsigned> expected;
  auto verify = [&](Index index) {
    auto const found = expected.find(index);
    auto const* value = actual.lookup(index);
    CHECK((value != nullptr) == (found != expected.end()));
    if (value)
      CHECK(*value == Number(static_cast<Index>(found->second)));
    CHECK(actual.size() == expected.size());
    CHECK(actual.length() == (expected.empty() ? Index{0} : Index(expected.rbegin()->first + 1)));
  };
  verify(high);
  CHECK(actual.empty());
  // Cover absent page slots, page boundaries, high sparse keys, explicit zero,
  // replacement and backwards/random insertion without a dense allocation.
  for (Index key : {Index{0}, Index{63}, Index{64}, Index{65}, high}) {
    actual.insert_or_assign(key, Number(0));
    expected.insert_or_assign(key, 0);
    verify(key);
  }
  std::mt19937 random(197209);
  for (unsigned i = 0; i < 3000; ++i) {
    auto const key = static_cast<Index>(random() % Number::base_);
    auto const value = static_cast<Index>(random() % 23);
    actual.insert_or_assign(key, Number(value));
    expected.insert_or_assign(key, value);
    verify(key);
    verify(static_cast<Index>(random() % Number::base_));
  }
  auto snapshot = actual;
  actual.insert_or_assign(0, Number(7));
  CHECK(*snapshot.lookup(0) == Number(static_cast<Index>(expected.at(0))));
  auto moved = std::move(snapshot);
  CHECK(snapshot.empty());
  CHECK(snapshot.length() == 0);
  CHECK(snapshot.lookup(high) == nullptr);
  snapshot = moved;
  CHECK(snapshot.size() == expected.size());
  // Passing a borrowed value must be safe even when insertion reallocates or
  // rehashes the container. Heap digit payloads must preserve value isolation.
  Number large("123456789012345678901234567890", 10);
  actual.insert_or_assign(0, large);
  for (unsigned i = 1; i < 80; ++i) {
    auto const key = static_cast<Index>(i);
    actual.insert_or_assign(key, *actual.lookup(0));
    CHECK(*actual.lookup(key) == large);
  }
  auto copy = actual;
  copy.insert_or_assign(0, Number(3));
  CHECK(*actual.lookup(0) == large);
  CHECK(*copy.lookup(0) == Number(3));
}

template<typename Traits>
void check_all_storage_candidates()
{
  using Number = GD::Bc::BasicNumber<Traits>;
  check_storage_candidate<GD::Bc::Details::HybridArrayStorage<Number>, Number>();
  check_storage_candidate<GD::Bc::Details::PagedArrayStorage<Number>, Number>();
  check_storage_candidate<GD::Bc::Details::FlatArrayStorage<Number>, Number>();
  check_storage_candidate<GD::Bc::Details::MapArrayStorage<Number, true>, Number>();
  check_storage_candidate<GD::Bc::Details::MapArrayStorage<Number, false>, Number>();
}
}  // namespace

TEST_CASE("Array layout candidates preserve sparse value semantics",
          "[bc][vm][arrays][array-storage][array-candidates]")
{
  // Retain the small-width generic model and complement it with native widths.
  check_all_storage_candidates<GD::Bc::NumberTraits8>();
  check_all_storage_candidates<GD::Bc::NumberTraits16>();
  check_all_storage_candidates<GD::Bc::NumberTraits32>();
}
