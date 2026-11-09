/** \file test-function-slot.cc
 * Isolated ownership, replacement and failed-construction contracts.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <array>
#include <cstddef>
#include <cstdint>
#include <exception>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <string_view>
#include <type_traits>
#include <utility>

#include "function-slot.hh"

namespace {
void check(bool condition)
{
  if (!condition)
    throw std::runtime_error("function-slot contract failed");
}

struct Definition
{
  static inline int live = 0;
  explicit Definition(int incoming) : value(incoming)
  {
    if (value < 0)
      throw std::runtime_error("injected definition construction failure");
    ++live;
  }
  Definition(Definition&& other) noexcept : value(std::exchange(other.value, 0)) { ++live; }
  Definition(Definition const&) = delete;
  auto operator=(Definition const&) -> Definition& = delete;
  ~Definition() { --live; }

  int value;
  // Exercise an owner larger than the borrowed descriptor, as in the VM.
  std::array<std::uintptr_t, 16> payload{};
};

struct Pending
{
  std::string_view image;
  auto (*decode)(std::string_view) -> std::size_t;
};

auto decode(std::string_view image) -> std::size_t { return image.size(); }
using Slot = GD::Bc::Details::FunctionSlot<Definition, Pending>;
static_assert(std::is_nothrow_default_constructible_v<Slot>);
static_assert(noexcept(std::declval<Slot&>() = std::declval<std::optional<Definition>&&>()));

void empty_and_deferred()
{
  Slot slot;
  check(!slot && !slot.has_value() && slot.pending() == nullptr);
  slot.defer({"first", decode});
  check(!slot && slot.pending() != nullptr && Definition::live == 0);
  check(slot.pending()->decode(slot.pending()->image) == 5);
  slot.defer({"second", decode});
  check(slot.pending()->image == "second" && Definition::live == 0);
  slot.reset();
  check(!slot && slot.pending() == nullptr);
}

void replacements_release_exactly_one_owner()
{
  Slot slot;
  slot.defer({"pending", decode});
  slot = std::make_optional<Definition>(7);
  check(slot && slot.pending() == nullptr && slot.value().value == 7 && Definition::live == 1);
  slot = std::make_optional<Definition>(9);
  check((*slot).value == 9 && Definition::live == 1);
  auto const& view = slot;
  check(view.value().value == 9 && (*view).value == 9 && view.pending() == nullptr);
  slot.defer({"replacement", decode});
  check(!slot && slot.pending()->image == "replacement" && Definition::live == 0);
  slot = std::make_optional<Definition>(11);
  check(Definition::live == 1);
  slot = std::optional<Definition>{};
  check(!slot && slot.pending() == nullptr && Definition::live == 0);
}

void failed_construction_keeps_the_previous_alternative()
{
  Slot slot;
  slot.defer({"retryable", decode});
  bool caught = false;
  try {
    slot = std::make_optional<Definition>(-1);
  }
  catch (std::runtime_error const&) {
    caught = true;
  }
  check(caught && !slot && slot.pending()->image == "retryable" && Definition::live == 0);
  slot = std::make_optional<Definition>(13);
  caught = false;
  try {
    slot = std::make_optional<Definition>(-1);
  }
  catch (std::runtime_error const&) {
    caught = true;
  }
  check(caught && slot.value().value == 13 && Definition::live == 1);
}

void independent_names_and_repeated_destruction()
{
  std::array<Slot, 26> names;
  names[0].defer({"a", decode});
  names[25] = std::make_optional<Definition>(25);
  for (int value = 0; value != 1000; ++value) {
    names[0] = std::make_optional<Definition>(value);
    check(Definition::live == 2 && names[25].value().value == 25);
    names[0].defer({"a", decode});
    check(Definition::live == 1 && names[25].value().value == 25);
  }
}
}  // namespace

auto main() -> int
{
  try {
    empty_and_deferred();
    check(Definition::live == 0);
    replacements_release_exactly_one_owner();
    check(Definition::live == 0);
    failed_construction_keeps_the_previous_alternative();
    check(Definition::live == 0);
    independent_names_and_repeated_destruction();
    check(Definition::live == 0);
    std::cout << "Function-slot ownership and construction-failure contracts passed\n";
    std::cout << "Fixture bytes: combined=" << sizeof(Slot)
              << ", separate=" << sizeof(std::optional<Definition>) + sizeof(Pending) << '\n';
  }
  catch (std::exception const& error) {
    std::cerr << error.what() << '\n';
    return 1;
  }
  return 0;
}
