/** Activation-prefix work, reference stability and failure recovery contracts.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <catch2/catch.hpp>

#include <limits>
#include <memory>
#include <stdexcept>
#include <type_traits>
#include <utility>
#include <vector>

#include "result-prefix-buffer.hh"

namespace {
struct Cell
{
  inline static std::size_t live = 0;
  inline static std::size_t constructions = 0;
  inline static std::size_t remaining = std::numeric_limits<std::size_t>::max();
  int value = 0;

  Cell()
  {
    if (remaining == 0)
      throw std::runtime_error("cell construction");
    if (remaining != std::numeric_limits<std::size_t>::max())
      --remaining;
    ++live;
    ++constructions;
  }
  Cell(Cell const&) = delete;
  Cell(Cell&& other) noexcept : value(other.value)
  {
    ++live;
    ++constructions;
  }
  ~Cell() { --live; }
};

struct Allocations
{
  inline static bool deny = false;
  inline static std::size_t attempts = 0;
  inline static std::size_t live = 0;
};

template<typename Value>
struct Allocator
{
  using value_type = Value;
  using is_always_equal = std::true_type;
  Allocator() = default;
  template<typename Other>
  Allocator(Allocator<Other> const&) noexcept
  {
  }
  auto allocate(std::size_t count) -> Value*
  {
    ++Allocations::attempts;
    if (std::exchange(Allocations::deny, false))
      throw std::bad_alloc();
    auto* value = std::allocator<Value>{}.allocate(count);
    ++Allocations::live;
    return value;
  }
  void deallocate(Value* value, std::size_t count) noexcept
  {
    --Allocations::live;
    std::allocator<Value>{}.deallocate(value, count);
  }
  template<typename Other>
  auto operator==(Allocator<Other> const&) const noexcept -> bool
  {
    return true;
  }
};

using Buffer = GD::Bc::Details::ResultPrefixBuffer<Cell, Allocator<Cell>>;
void reset_counts()
{
  REQUIRE(Cell::live == 0);
  REQUIRE(Allocations::live == 0);
  Cell::constructions = 0;
  Cell::remaining = std::numeric_limits<std::size_t>::max();
  Allocations::deny = false;
  Allocations::attempts = 0;
}
}  // namespace

TEST_CASE("Result prefixes avoid unused tail lifetimes and preserve references", "[bc][vm][prefix]")
{
  reset_counts();
  STATIC_REQUIRE(sizeof(Buffer) == sizeof(std::vector<Cell, Allocator<Cell>>));
  STATIC_REQUIRE(alignof(Buffer) == alignof(std::vector<Cell, Allocator<Cell>>));
  {
    Buffer buffer;
    buffer.prepare(332);
    CHECK(buffer.empty());
    CHECK(Cell::constructions == 0);
    auto const allocations = Allocations::attempts;
    buffer[2].value = 7;
    CHECK(Cell::constructions == 3);
    auto const* first = &buffer[0];
    CHECK(buffer[2].value == 7);
    CHECK(Cell::constructions == 3);
    buffer[249].value = 11;
    CHECK(Cell::constructions == 250);
    CHECK(Cell::live == 250);
    CHECK(&buffer[0] == first);
    CHECK(Allocations::attempts == allocations);
    buffer.clear();
    CHECK(Cell::live == 0);
    CHECK(buffer.capacity() >= 332);
    buffer.prepare(10);
    CHECK(Cell::constructions == 250);
    CHECK(buffer[2].value == 0);
    CHECK(Cell::constructions == 253);
    CHECK(Allocations::attempts == allocations);
    CHECK(std::as_const(buffer)[2].value == 0);
  }
  CHECK(Cell::live == 0);
  CHECK(Allocations::live == 0);
}

TEST_CASE("Result prefixes recover after preparation allocation failure", "[bc][vm][prefix]")
{
  reset_counts();
  {
    Buffer buffer;
    buffer.prepare(8);
    buffer[3].value = 17;
    buffer.clear();
    auto const capacity = buffer.capacity();
    Allocations::deny = true;
    CHECK_THROWS_AS(buffer.prepare(capacity + 1), std::bad_alloc);
    CHECK(buffer.empty());
    CHECK(Cell::live == 0);
    CHECK(buffer.capacity() == capacity);
    buffer.prepare(capacity + 1);
    auto const allocations = Allocations::attempts;
    CHECK(buffer[capacity].value == 0);
    CHECK(buffer[0].value == 0);
    CHECK(Allocations::attempts == allocations);
  }
  CHECK(Cell::live == 0);
  CHECK(Allocations::live == 0);
}

TEST_CASE("Result prefixes roll back partial construction without moving live cells",
          "[bc][vm][prefix]")
{
  reset_counts();
  {
    Buffer buffer;
    buffer.prepare(32);
    buffer[1].value = 29;
    auto const* earlier = &buffer[1];
    auto const allocations = Allocations::attempts;
    Cell::remaining = 2;
    CHECK_THROWS_AS(buffer[10], std::runtime_error);
    CHECK(buffer.size() == 2);
    CHECK(Cell::live == 2);
    CHECK(Cell::constructions == 4);
    CHECK(&buffer[1] == earlier);
    CHECK(buffer[1].value == 29);
    Cell::remaining = std::numeric_limits<std::size_t>::max();
    CHECK(buffer[10].value == 0);
    CHECK(buffer.size() == 11);
    CHECK(Cell::live == 11);
    CHECK(Allocations::attempts == allocations);
  }
  CHECK(Cell::live == 0);
  CHECK(Allocations::live == 0);
}

TEST_CASE("Result prefixes retain dense diagnostic initialization", "[bc][vm][prefix]")
{
  reset_counts();
  {
    Buffer buffer;
    buffer.prepare(19, true);
    CHECK(Cell::constructions == 19);
    CHECK(Cell::live == 19);
    CHECK(buffer.size() == 19);
    buffer[18].value = 31;
    CHECK(Cell::constructions == 19);
    buffer.clear();
    CHECK(Cell::live == 0);
    buffer.prepare(0, true);
    CHECK(buffer.empty());
    CHECK(Cell::constructions == 19);
  }
  CHECK(Allocations::live == 0);
}

TEST_CASE("Result prefixes leave empty storage after eager initialization fails",
          "[bc][vm][prefix]")
{
  reset_counts();
  {
    Buffer buffer;
    Cell::remaining = 2;
    CHECK_THROWS_AS(buffer.prepare(11, true), std::runtime_error);
    CHECK(Cell::live == 0);
    REQUIRE(buffer.empty());
    Cell::remaining = std::numeric_limits<std::size_t>::max();
    buffer.prepare(11, true);
    CHECK(buffer.size() == 11);
    CHECK(Cell::live == 11);
    CHECK(Cell::constructions == 13);
  }
  CHECK(Cell::live == 0);
  CHECK(Allocations::live == 0);
}

TEST_CASE("Result prefixes destroy owned cells while escaped values survive", "[bc][vm][prefix]")
{
  GD::Bc::Details::ResultPrefixBuffer<std::shared_ptr<int>> buffer;
  buffer.prepare(40);
  auto owner = std::make_shared<int>(43);
  std::weak_ptr<int> weak = owner;
  buffer[4] = std::move(owner);
  auto escaped = buffer[4];
  buffer.clear();
  CHECK_FALSE(weak.expired());
  CHECK(*escaped == 43);
  escaped.reset();
  CHECK(weak.expired());
  buffer.prepare(6);
  CHECK_FALSE(buffer[4]);
}
