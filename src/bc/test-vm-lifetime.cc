/** \file test-vm-lifetime.cc
 * Isolated VM/parser ownership and failure guards, without production test hooks.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <atomic>
#include <cstdio>
#include <cstdlib>
#include <limits>
#include <new>
#include <sstream>
#include <type_traits>
#include <utility>

#include "bc.hh"

namespace {
// These replacements belong only to this small regression executable. They
// track ordinary allocations by count without changing production allocators.
std::atomic<std::ptrdiff_t> outstanding{0};
std::atomic<std::size_t> allocation_calls{0};
// A scoped ceiling lets native-width scale-boundary tests reject the attempted
// coefficient growth before allocating gigabytes. It is inactive elsewhere.
thread_local std::size_t allocation_ceiling = std::numeric_limits<std::size_t>::max();
thread_local std::size_t denied_allocations = 0;
// Deny one ordinary allocation, then allow exception capture and cleanup.
thread_local std::size_t fail_after = std::numeric_limits<std::size_t>::max();
thread_local std::size_t injected_failures = 0;

class AllocationFailure
{
public:
  explicit AllocationFailure(std::size_t after) : previous_(std::exchange(fail_after, after)) {}
  ~AllocationFailure() { fail_after = previous_; }
  AllocationFailure(AllocationFailure const&) = delete;
  auto operator=(AllocationFailure const&) -> AllocationFailure& = delete;

private:
  std::size_t previous_;
};

class AllocationCeiling
{
public:
  explicit AllocationCeiling(std::size_t ceiling)
      : previous_(std::exchange(allocation_ceiling, ceiling))
  {
  }
  ~AllocationCeiling() { allocation_ceiling = previous_; }
  AllocationCeiling(AllocationCeiling const&) = delete;
  auto operator=(AllocationCeiling const&) -> AllocationCeiling& = delete;

private:
  std::size_t previous_;
};
}  // namespace

auto operator new(std::size_t size) -> void*
{
  if (fail_after == 0) {
    fail_after = std::numeric_limits<std::size_t>::max();
    ++injected_failures;
    throw std::bad_alloc();
  }
  if (fail_after != std::numeric_limits<std::size_t>::max()) {
    --fail_after;
  }
  if (size > allocation_ceiling) {
    ++denied_allocations;
    throw std::bad_alloc();
  }
  while (true) {
    if (auto* value = std::malloc(size == 0 ? 1 : size)) {
      outstanding.fetch_add(1, std::memory_order_relaxed);
      allocation_calls.fetch_add(1, std::memory_order_relaxed);
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
template<typename Traits>
auto inline_numbers() -> bool
{
  using Number = GD::Bc::BasicNumber<Traits>;
  auto const before = allocation_calls.load(std::memory_order_relaxed);
  {
    Number original(7);
    auto copy = original;
    copy.add(Number(1));
    if (!(original == Number(7)) || !(copy == Number(8))) {
      return false;
    }
    copy.sub(Number(1));
    if (!(copy == original)) {
      return false;
    }
  }
  return allocation_calls.load(std::memory_order_relaxed) == before;
}

// Native maximum scales cannot be materialized in an ordinary regression.
// A valid request must either retain its scale or attempt the large coefficient
// allocation. Returning a wrapped small scale without that attempt is wrong.
template<typename Traits>
auto native_sqrt_scale_boundary() -> bool
{
  using Number = GD::Bc::BasicNumber<Traits>;
  using NumType = typename Traits::NumType;
  auto const maximum = std::numeric_limits<NumType>::max();
  for (auto const requested : {static_cast<NumType>(maximum - 1), maximum}) {
    Number value(4);
    auto const snapshot = value;
    auto const denied_before = denied_allocations;
    bool valid = false;
    try {
      AllocationCeiling const ceiling(1024);
      value.sqrt(requested);
      valid = value.scale() == requested;
    }
    catch (std::bad_alloc const&) {
      // Only the injected size ceiling is evidence of coefficient growth.
      valid = denied_allocations > denied_before;
    }
    // Do not assume a strong exception guarantee for the mutable result.
    // Its previously shared immutable value must remain intact.
    if (!valid || !(snapshot == Number(4))) {
      return false;
    }
  }
  return true;
}

// Walk ordinary allocation points while parsing a malformed second item.
// A retry may return the recorded fatal diagnostic or rethrow a terminal
// exception; it must never accept the remaining tail as a fresh item.
auto parser_allocation_failures() -> bool
{
  using I = GD::Bc::Instruction;
  using O = I::Opcode;
  auto canonical_failure = [](std::shared_ptr<GD::Bc::Instructions> const& code) {
    return code && code->size() == 4 && code->at(0).opcode() == O::string &&
           code->at(1).opcode() == O::print &&
           std::get<I::Stream>(code->at(1).op2()) == I::Stream::error &&
           code->at(2).opcode() == O::quit && std::get<unsigned>(code->at(2).op1()) == 1 &&
           code->at(3).opcode() == O::eof;
  };
  unsigned escaped = 0;
  for (std::size_t point = 0; point != 128; ++point) {
    GD::Bc::Parser parser(
      std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>("7\n1+\n99\n")), true);
    auto original = parser.parse();
    if (!original || original->at(0).opcode() != O::number ||
        std::get<std::string>(original->at(0).op1()) != "7") {
      return false;
    }
    auto const before = injected_failures;
    std::shared_ptr<GD::Bc::Instructions> code;
    bool threw = false;
    try {
      AllocationFailure const failure(point);
      code = parser.parse();
    }
    catch (std::bad_alloc const&) {
      threw = true;
      ++escaped;
    }
    if (!threw && !canonical_failure(code)) {
      return false;
    }
    if (threw) {
      try {
        if (!canonical_failure(parser.parse())) {
          return false;
        }
      }
      catch (std::bad_alloc const&) {
        // A captured exception remains terminal after the injector is gone.
      }
    }
    if (std::get<std::string>(original->at(0).op1()) != "7") {
      return false;
    }
    if (injected_failures == before) {
      return escaped != 0;  // All allocation points in this parse were visited.
    }
  }
  return false;  // Do not silently accept a truncated fault sweep.
}

auto first_array_store_failures() -> bool
{
  using I = GD::Bc::Instruction;
  using O = I::Opcode;
  using D = I::Offset;
  auto const array = GD::Bc::Array('a');
  bool observed_failure = false;
  for (std::size_t point = 0; point != 128; ++point) {
    std::ostringstream output, errors;
    GD::Bc::VM vm(output, errors, false);
    GD::Bc::Instructions warm{I(O::eof)};
    if (vm.execute(warm))
      return false;
    GD::Bc::Instructions store{I(O::number, "9"), I(O::array_element, array, D(-1)),
                               I(O::number, "7"), I(O::store, D(-2), D(-1)), I(O::eof)};
    GD::Bc::Instructions query{I(O::array, array),
                               I(O::length, D(-1)),
                               I(O::print, D(-1), I::Stream::output),
                               I(O::number, "9"),
                               I(O::array_element, array, D(-1)),
                               I(O::load, D(-1)),
                               I(O::print, D(-1), I::Stream::output),
                               I(O::eof)};
    auto const before = injected_failures;
    bool failed = false;
    try {
      AllocationFailure const failure(point);
      if (vm.execute(store))
        return false;
    }
    catch (std::bad_alloc const&) {
      failed = true;
    }
    if (failed != (injected_failures != before))
      return false;
    if (failed) {
      observed_failure = true;
      // Failure can leave an allocated map with no entries. Check both the
      // length and the absent element before retrying the same VM.
      if (vm.execute(query) || output.str() != "0\n0\n" || vm.execute(store))
        return false;
    }
    if (vm.execute(query) || !errors.str().empty() ||
        output.str() != (failed ? "0\n0\n10\n7\n" : "10\n7\n"))
      return false;
    if (!failed)
      return observed_failure;
  }
  return false;
}

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
  // Measure real ordinary allocations in this isolated executable, including
  // constructor/copy/COW arithmetic, rather than inferring them from a tag.
  if (!inline_numbers<GD::Bc::NumberTraits8>() || !inline_numbers<GD::Bc::NumberTraits16>() ||
      !inline_numbers<GD::Bc::NumberTraits32>()) {
    std::fprintf(stderr, "Small-number value operations allocated or lost isolation\n");
    return 1;
  }
  // Warm diagnostic/catalog paths before measuring the fault sweep's lifetime.
  if (!parser_allocation_failures()) {
    std::fprintf(stderr, "Parser allocation failure resumed a partial input item\n");
    return 1;
  }
  if (!first_array_store_failures()) {
    std::fprintf(stderr, "Failed first array store changed visible state or prevented retry\n");
    return 1;
  }
  auto const before = outstanding.load(std::memory_order_relaxed);
  if (!parser_allocation_failures()) {
    std::fprintf(stderr, "Repeated parser allocation sweep lost terminal state\n");
    return 1;
  }
  if (!first_array_store_failures()) {
    std::fprintf(stderr, "Repeated array failure sweep changed visible state\n");
    return 1;
  }
  if (!native_sqrt_scale_boundary<GD::Bc::NumberTraits16>() ||
      !native_sqrt_scale_boundary<GD::Bc::NumberTraits32>()) {
    std::fprintf(stderr, "Native square-root scale wrapped or lost snapshot isolation\n");
    return 1;
  }
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
