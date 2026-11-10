/** \file test-prefix-result-buffer.cc
 * Result ownership, failure and finite work contracts; no elapsed benchmarks.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <catch2/catch.hpp>

#include <deque>
#include <memory>
#include <utility>

#include "prefix-result-buffer.hh"

namespace {
struct Cell
{
  inline static std::size_t constructions = 0;
  std::shared_ptr<int> owner;
  Cell() noexcept { ++constructions; }
};
struct Reset
{
  inline static std::size_t calls = 0;
  void operator()(Cell& cell) const noexcept
  {
    ++calls;
    cell.owner.reset();
  }
};
struct AllocationFailure
{
  inline static bool fail = false;
};
template<typename T>
struct Allocator : std::allocator<T>
{
  using is_always_equal = std::true_type;
  template<typename U>
  struct rebind
  {
    using other = Allocator<U>;
  };
  auto allocate(std::size_t count) -> T*
  {
    if (AllocationFailure::fail)
      throw std::bad_alloc();
    return std::allocator<T>::allocate(count);
  }
};
using Buffer = GD::Bc::Details::PrefixResultBuffer<Cell, Reset, Allocator<Cell>>;
auto const_cell(Buffer const& buffer, std::size_t index) -> Cell const& { return buffer[index]; }
}  // namespace

TEST_CASE("Result prefixes retain empty objects without repeated construction",
          "[bc][vm][prefix-results]")
{
  Cell::constructions = Reset::calls = 0;
  Buffer buffer;
  buffer.resize(512);
  REQUIRE(Cell::constructions == 512);
  buffer.clear();
  CHECK(Reset::calls == 0);
  for (unsigned i = 0; i < 100; ++i) {
    buffer.resize(512);
    buffer[7].owner = std::make_shared<int>(42);
    std::weak_ptr<int> weak = const_cell(buffer, 7).owner;
    CHECK_FALSE(const_cell(buffer, 511).owner);
    buffer.clear();
    CHECK(weak.expired());
    CHECK(buffer.empty());
  }
  CHECK(Cell::constructions == 512);
  CHECK(Reset::calls == 800);
}

TEST_CASE("Early and late result paths release owners before smaller reuse",
          "[bc][vm][prefix-results]")
{
  Buffer buffer;
  buffer.resize(128);
  buffer[127].owner = std::make_shared<int>(9);
  auto escaped = const_cell(buffer, 127).owner;
  std::weak_ptr<int> weak = escaped;
  buffer.clear();
  REQUIRE_FALSE(weak.expired());
  buffer.resize(8);
  for (std::size_t i = 0; i < buffer.size(); ++i)
    CHECK_FALSE(const_cell(buffer, i).owner);
  buffer[1].owner = std::make_shared<int>(3);
  buffer.clear();
  buffer.resize(128);
  for (std::size_t i = 0; i < buffer.size(); ++i)
    CHECK_FALSE(const_cell(buffer, i).owner);
  buffer.clear();
  escaped.reset();
  CHECK(weak.expired());
}

TEST_CASE("Access records ownership before a throwing result operation", "[bc][vm][prefix-results]")
{
  Reset::calls = 0;
  Buffer buffer;
  buffer.resize(128);
  std::weak_ptr<int> weak;
  try {
    auto& cell = buffer[97];
    cell.owner = std::make_shared<int>(11);
    weak = cell.owner;
    throw std::bad_alloc();
  }
  catch (std::bad_alloc const&) {
    buffer.clear();
  }
  CHECK(weak.expired());
  CHECK(Reset::calls == 98);
  CHECK(buffer.empty());
  buffer.resize(128);
  CHECK_FALSE(const_cell(buffer, 97).owner);
  buffer.clear();
}

TEST_CASE("Failed initial or retained growth leaves an inactive usable buffer",
          "[bc][vm][prefix-results]")
{
  Buffer buffer;
  AllocationFailure::fail = true;
  CHECK_THROWS_AS(buffer.resize(16), std::bad_alloc);
  AllocationFailure::fail = false;
  REQUIRE(buffer.empty());
  buffer.resize(16);
  buffer[15].owner = std::make_shared<int>(5);
  std::weak_ptr<int> weak = const_cell(buffer, 15).owner;
  buffer.clear();
  REQUIRE(weak.expired());
  AllocationFailure::fail = true;
  CHECK_THROWS_AS(buffer.resize(1024), std::bad_alloc);
  CHECK(buffer.empty());
  CHECK_NOTHROW(buffer.resize(16));
  for (std::size_t i = 0; i < buffer.size(); ++i)
    CHECK_FALSE(const_cell(buffer, i).owner);
  buffer.clear();
  AllocationFailure::fail = false;
}

TEST_CASE("Moved result buffers transfer cleanup bounds and permit source reuse",
          "[bc][vm][prefix-results]")
{
  Buffer original;
  original.resize(32);
  original[29].owner = std::make_shared<int>(7);
  std::weak_ptr<int> weak = const_cell(original, 29).owner;
  Buffer moved(std::move(original));
  CHECK(original.empty());
  REQUIRE(moved.size() == 32);
  CHECK(*const_cell(moved, 29).owner == 7);
  Buffer replacement;
  replacement.resize(2);
  replacement[1].owner = std::make_shared<int>(6);
  std::weak_ptr<int> old = const_cell(replacement, 1).owner;
  replacement = std::move(moved);
  CHECK(old.expired());
  CHECK(moved.empty());
  replacement.clear();
  CHECK(weak.expired());
  original.resize(4);
  original.clear();
  moved.resize(4);
  moved.clear();
}

TEST_CASE("Result buffer copies stage storage before replacing cleanup bounds",
          "[bc][vm][prefix-results]")
{
  Buffer source;
  source.resize(64);
  source[63].owner = std::make_shared<int>(13);
  Buffer destination;
  destination.resize(8);
  destination[7].owner = std::make_shared<int>(17);
  std::weak_ptr<int> previous = const_cell(destination, 7).owner;
  AllocationFailure::fail = true;
  CHECK_THROWS_AS(destination = source, std::bad_alloc);
  AllocationFailure::fail = false;
  REQUIRE(destination.size() == 8);
  CHECK(*const_cell(destination, 7).owner == 17);
  destination = source;
  CHECK(previous.expired());
  source.clear();
  CHECK(*const_cell(destination, 63).owner == 13);
  std::weak_ptr<int> copied = const_cell(destination, 63).owner;
  destination.clear();
  CHECK(copied.expired());
}

TEST_CASE("Simultaneous result depths retain references and distinct owners",
          "[bc][vm][prefix-results]")
{
  std::deque<Buffer> buffers;
  buffers.emplace_back();
  buffers.front().resize(16);
  auto& outer = buffers.front()[9];
  outer.owner = std::make_shared<int>(23);
  for (unsigned i = 0; i < 300; ++i) {
    buffers.emplace_back();
    buffers.back().resize(32);
    buffers.back()[31].owner = std::make_shared<int>(static_cast<int>(i));
  }
  REQUIRE(*outer.owner == 23);
  for (std::size_t i = buffers.size(); i > 1; --i)
    buffers[i - 1].clear();
  CHECK(*outer.owner == 23);
  std::weak_ptr<int> weak = outer.owner;
  buffers.front().clear();
  CHECK(weak.expired());
}
