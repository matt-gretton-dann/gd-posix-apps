/** \file test-vm-lifetime.cc
 * Isolated allocation-balance regression for VM ownership, without test hooks.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <atomic>
#include <cstdio>
#include <cstdlib>
#include <new>
#include <sstream>
#include <type_traits>
#include <utility>

#include "bc.hh"

namespace {
// These replacements belong only to this small regression executable. They
// track ordinary allocations by count without changing production allocators.
std::atomic<std::ptrdiff_t> outstanding{0};
}  // namespace

auto operator new(std::size_t size) -> void*
{
  while (true) {
    if (auto* value = std::malloc(size == 0 ? 1 : size)) {
      outstanding.fetch_add(1, std::memory_order_relaxed);
      return value;
    }
    auto const handler = std::get_new_handler();
    if (handler == nullptr) {
      throw std::bad_alloc();
    }
    handler();
  }
}
void operator delete(void* value) noexcept
{
  if (value != nullptr) {
    outstanding.fetch_sub(1, std::memory_order_relaxed);
    std::free(value);
  }
}
auto operator new[](std::size_t size) -> void* { return ::operator new(size); }
void operator delete[](void* value) noexcept { ::operator delete(value); }
void operator delete(void* value, std::size_t) noexcept { ::operator delete(value); }
void operator delete[](void* value, std::size_t) noexcept { ::operator delete(value); }

namespace {
auto run() -> bool
{
  std::ostringstream output;
  std::ostringstream errors;
  GD::Bc::Parser parser(std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(
                          "a=12345678901234567890\na[3]=7\ndefine f(x) {\nreturn(x+1)\n}\n")),
                        true);
  GD::Bc::VM vm(output, errors, false);
  while (auto code = parser.parse()) {
    if (!vm.execute(*code)) {
      break;
    }
  }
  static_assert(!std::is_copy_constructible_v<GD::Bc::VM>);
  static_assert(!std::is_copy_assignable_v<GD::Bc::VM>);
  // Transfer populated state, then replace an already allocated destination.
  GD::Bc::VM moved(std::move(vm));
  GD::Bc::VM replaced(output, errors, false);
  replaced = std::move(moved);
  GD::Bc::Parser query(
    std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>("a\na[3]\nf(8)\n")),
    true);
  while (auto code = query.parse()) {
    if (!replaced.execute(*code)) {
      break;
    }
  }
  return output.str() == "12345678901234567890\n7\n9\n" && errors.str().empty();
}
}  // namespace

auto main() -> int
{
  // Warm lazy catalog/iostream initialization before observing VM lifetimes.
  if (!run()) {
    return 1;
  }
  auto const before = outstanding.load(std::memory_order_relaxed);
  for (unsigned i = 0; i < 3; ++i) {
    if (!run()) {
      return 1;
    }
  }
  auto const after = outstanding.load(std::memory_order_relaxed);
  if (before != after) {
    std::fprintf(stderr, "VM destruction retained %td ordinary allocations\n", after - before);
    return 1;
  }
  return 0;
}
