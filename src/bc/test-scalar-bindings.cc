/** \file test-scalar-bindings.cc
 * Dynamic scalar binding, ownership and setup-failure contracts for #707.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "scalar-bindings.hh"

#include <array>
#include <cstddef>
#include <iostream>
#include <memory>
#include <new>
#include <span>
#include <stdexcept>

namespace {
struct AllocationControl
{
  static inline int remaining = -1;
};
template<typename T>
struct Allocator : std::allocator<T>
{
  using value_type = T;
  template<typename U> struct rebind { using other = Allocator<U>; };
  Allocator() = default;
  template<typename U> Allocator(Allocator<U> const&) {}
  auto allocate(std::size_t n) -> T*
  {
    if (AllocationControl::remaining == 0) throw std::bad_alloc();
    if (AllocationControl::remaining > 0) --AllocationControl::remaining;
    return std::allocator<T>{}.allocate(n);
  }
};
struct Value
{
  static inline unsigned transfers = 0;
  static inline int constructions = -1;
  int value = 0;
  std::shared_ptr<int> owner;
  Value()
  {
    if (constructions == 0) throw std::bad_alloc();
    if (constructions > 0) --constructions;
  }
  Value(Value const& other) : value(other.value), owner(other.owner) { ++transfers; }
  Value(Value&& other) noexcept : value(other.value), owner(std::move(other.owner)) { ++transfers; }
  auto operator=(Value const&) -> Value& = default;
  auto operator=(Value&&) noexcept -> Value& = default;
};
using Bindings = GD::Bc::Details::ScalarBindings<Value, 4, Allocator<Value>>;
void require(bool value)
{
  if (!value) throw std::runtime_error("scalar binding contract failed");
}
void deep(Bindings& values, unsigned depth)
{
  std::array<unsigned, 1> const keys{1};
  auto frame = values.enter(keys);
  auto* address = &values.at(1);
  values.at(1).value = static_cast<int>(depth);
  if (depth != 0) deep(values, depth - 1);
  require(&values.at(1) == address && values.at(1).value == static_cast<int>(depth));
}
}  // namespace

auto main() -> int
{
  try {
    Bindings values;
    values.at(0).value = 11;
    values.at(1).value = 22;
    auto* global = &values.at(0);
    std::weak_ptr<int> weak;
    std::shared_ptr<int> snapshot;
    std::array<unsigned, 2> const first{0, 2};
    {
      auto outer = values.enter(first);
      require(values.at(0).value == 0 && values.at(2).value == 0);
      require(values.at(1).value == 22);  // Dynamic inheritance, not lexical globals.
      values.at(0).value = 33;
      values.at(0).owner = std::make_shared<int>(99);
      weak = values.at(0).owner;
      snapshot = values.at(0).owner;
      auto* outer_value = &values.at(0);
      {
        std::array<unsigned, 1> const second{2};
        auto inner = values.enter(second);
        require(&values.at(0) == outer_value && values.at(2).value == 0);
        values.at(0).value = 44;  // A callee can modify an inherited local.
        values.at(1).value = 55;  // Or an inherited global.
        deep(values, 128);       // Deque/vector growth cannot relocate active values.
        require(values.at(1).value == 55);
      }
      require(&values.at(0) == outer_value && values.at(0).value == 44);
    }
    require(&values.at(0) == global && values.at(0).value == 11);
    require(values.at(1).value == 55 && !weak.expired() && *snapshot == 99);
    snapshot.reset();
    require(weak.expired());  // Reusable frames retain capacity, not owners.
    Value::transfers = 0;
    {
      auto again = values.enter(first);
      require(values.at(0).value == 0 && !values.at(0).owner);
    }
    require(Value::transfers == 0);  // Binding alone must not move/copy values.
    for (auto const keys : {std::array<unsigned, 2>{0, 4}, std::array<unsigned, 2>{0, 0}}) {
      bool rejected = false;
      try { auto invalid = values.enter(keys); }
      catch (std::out_of_range const&) { rejected = true; }
      catch (std::invalid_argument const&) { rejected = true; }
      require(rejected && &values.at(0) == global && values.at(0).value == 11);
    }
    bool bounds = false;
    try { (void)values.at(4); } catch (std::out_of_range const&) { bounds = true; }
    require(bounds);
    unsigned denied = 0;
    for (int budget = 0; budget != 20; ++budget) {
      Bindings fresh;
      fresh.at(0).value = 71;
      auto* before = &fresh.at(0);
      AllocationControl::remaining = budget;
      try { auto frame = fresh.enter(first); }
      catch (std::bad_alloc const&) { ++denied; }
      AllocationControl::remaining = -1;
      require(&fresh.at(0) == before && fresh.at(0).value == 71);
      auto recovered = fresh.enter(first);
      require(fresh.at(0).value == 0);
    }
    require(denied != 0);
    for (int budget = 0; budget != 2; ++budget) {
      Bindings fresh;
      fresh.at(0).value = 81;
      Value::constructions = budget;
      bool failed = false;
      try { auto frame = fresh.enter(first); }
      catch (std::bad_alloc const&) { failed = true; }
      Value::constructions = -1;
      require(failed && fresh.at(0).value == 81);
      auto recovered = fresh.enter(first);
      require(fresh.at(0).value == 0 && fresh.at(2).value == 0);
    }
    std::array<unsigned, 0> const none{};
    { auto empty = values.enter(none); require(&values.at(0) == global); }
    require(values.at(0).value == 11);
    std::cout << "scalar binding contracts passed\n";
  }
  catch (std::exception const& error) {
    AllocationControl::remaining = -1;
    Value::constructions = -1;
    std::cerr << error.what() << '\n';
    return 1;
  }
}
