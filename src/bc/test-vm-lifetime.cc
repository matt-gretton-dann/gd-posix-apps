/** \file test-vm-lifetime.cc
 * Isolated VM/parser ownership and failure guards, without production test hooks.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <algorithm>
#include <atomic>
#include <cstdio>
#include <cstdlib>
#include <limits>
#include <sstream>
#include <stdexcept>
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

// Keep counters and injected failures at an explicit replacement boundary,
// including caller-enabled IPO. Validate these observations with both ordinary
// and IPO builds; this isolated fixture changes no production allocator.
#if defined(_MSC_VER)
#  define GD_BC_TEST_NOINLINE __declspec(noinline)
#elif defined(__GNUC__) || defined(__clang__)
#  define GD_BC_TEST_NOINLINE [[gnu::noinline]]
#else
#  define GD_BC_TEST_NOINLINE
#endif
GD_BC_TEST_NOINLINE auto operator new(std::size_t size) -> void*
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
// This also keeps GCC's allocation-family analysis at the intentional
// malloc/free boundary instead of diagnosing inlined cleanup as mismatched.
GD_BC_TEST_NOINLINE void operator delete(void* value) noexcept
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
#undef GD_BC_TEST_NOINLINE

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

// A quotient which truncates to zero should not allocate the decimal padding
// of a denominator much larger than the coefficient. Parse before the ceiling
// so this observes division work, while retaining ordinary public snapshots.
template<typename Traits>
auto shrinking_division_storage() -> bool
{
  using Number = GD::Bc::BasicNumber<Traits>;
  using NumType = typename Number::NumType;
  auto const zeros = std::min<std::size_t>(8000, std::numeric_limits<NumType>::max() / 2);
  std::string const source = "0." + std::string(zeros, '0') + "1";
  Number const original(source, 10);
  Number value = original;
  Number const divisor(7);
  auto const denied_before = denied_allocations;
  bool valid = false;
  try {
    AllocationCeiling const ceiling(sizeof(NumType) == 1 ? 32 : 1024);
    value.divide(divisor, 0);
    valid = value.is_zero() && value.scale() == 0;
  }
  catch (std::bad_alloc const&) {
    return false;
  }
  return valid && denied_allocations == denied_before && original == Number(source, 10) &&
         divisor == Number(7);
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

class InterruptibleOutput : public std::stringbuf
{
public:
  bool fail = false;

private:
  auto xsputn(char const* data, std::streamsize size) -> std::streamsize override
  {
    if (fail)
      throw std::runtime_error("deliberate output failure");
    return std::stringbuf::xsputn(data, size);
  }
};

auto restore_nested_call_after_exception() -> bool
{
  using Checks = GD::Bc::VM::ExecutionChecks;
  for (auto checks : {Checks::full, Checks::verified}) {
    InterruptibleOutput buffer;
    std::ostream output(&buffer);
    output.exceptions(std::ios::badbit);
    std::ostringstream errors;
    GD::Bc::VM vm(output, errors, true, checks);
    auto run = [&vm](std::string const& text) {
      GD::Bc::Parser parser(
        std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(text)), false);
      auto code = parser.parse();
      return code && !vm.execute(*code);
    };
    if (!run("x=17\ny=23\na[3]=9\nb[3]=2\nscale=2\n"
             "define g(y) {\nscale=7\nibase=16\nz=8\n\"ready\"\nreturn(y)\n}\n"
             "define f(x,a[]) {\nauto y\ny=3\nscale=4\nobase=16\nreturn(g(6))\n}\n"))
      return false;
    buffer.fail = true;
    bool threw = false;
    try {
      (void)run("f(4,b[])\n");
    }
    catch (std::runtime_error const& error) {
      threw = std::string_view(error.what()) == "deliberate output failure";
    }
    if (!threw)
      return false;
    buffer.fail = false;
    output.clear();
    // The global write to z occurred before the exception and survives. Each
    // active call must still restore its local bindings and saved specials.
    if (!run("x\ny\na[3]\nscale\nibase\nobase\nz\n") ||
        buffer.str() != "17\n23\n9\n2\n10\n10\n8\n" || !run("f(4,b[])\n") ||
        buffer.str() != "17\n23\n9\n2\n10\n10\n8\nready6\n" || !errors.str().empty())
      return false;
  }
  return true;
}

auto restore_call_after_allocation_failure() -> bool
{
  auto parse = [](std::string const& text) {
    GD::Bc::Parser parser(
      std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(text)), false);
    return parser.parse();
  };
  bool observed_failure = false;
  for (std::size_t point = 0; point != 256; ++point) {
    std::ostringstream output, errors;
    GD::Bc::VM vm(output, errors, true, GD::Bc::VM::ExecutionChecks::verified);
    auto setup = parse("x=17\ny=23\na[3]=9\nb[3]=2\nscale=2\n"
                       "define g(y) {\nscale=7\nreturn(y)\n}\n"
                       "define f(x,a[]) {\nauto y\ny=3\nscale=4\nreturn(g(6))\n}\n");
    auto call = parse("q=f(4,b[])\n");
    auto query = parse("x\ny\na[3]\nscale\n");
    auto result = parse("q\n");
    if (!setup || !call || !query || !result || vm.execute(*setup))
      return false;
    auto const before = injected_failures;
    bool threw = false;
    try {
      AllocationFailure const failure(point);
      if (vm.execute(*call))
        return false;
    }
    catch (std::bad_alloc const&) {
      threw = true;
      observed_failure = true;
    }
    if (threw != (injected_failures != before) || vm.execute(*query) ||
        output.str() != "17\n23\n9\n2\n" || (threw && vm.execute(*call)) || vm.execute(*result) ||
        output.str() != "17\n23\n9\n2\n6\n" || !errors.str().empty())
      return false;
    if (!threw)
      return observed_failure;
  }
  return false;  // Require a complete sweep of call setup and execution.
}

auto restore_private_call_after_allocation_failure() -> bool
{
  auto parse = [](std::string const& text) {
    GD::Bc::Parser parser(
      std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(text)), false);
    return parser.parse();
  };
  bool observed_failure = false;
  for (std::size_t point = 0; point != 256; ++point) {
    std::ostringstream output, errors;
    GD::Bc::VM vm(output, errors, true, GD::Bc::VM::ExecutionChecks::verified);
    auto setup = parse("x=17\ny=23\nz=99\nscale=2\n"
                       "define f(x) {\nauto y,z\nscale=7\nif(x==0) return(z)\n"
                       "y=x\nz=f(x-1)\nreturn(y+z)\n}\n");
    auto call = parse("q=f(20)\n");
    auto query = parse("x\ny\nz\nscale\n");
    auto result = parse("q\n");
    if (!setup || !call || !query || !result || vm.execute(*setup))
      return false;
    auto const before = injected_failures;
    bool threw = false;
    try {
      AllocationFailure const failure(point);
      if (vm.execute(*call))
        return false;
    }
    catch (std::bad_alloc const&) {
      threw = true;
      observed_failure = true;
    }
    if (threw != (injected_failures != before) || vm.execute(*query) ||
        output.str() != "17\n23\n99\n2\n" || (threw && vm.execute(*call)) || vm.execute(*result) ||
        output.str() != "17\n23\n99\n2\n210\n" || !errors.str().empty())
      return false;
    if (!threw)
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

// Exercise every ordinary allocation in mixed verified activation setup and
// literal execution. This extends the earlier typed-pool retry check to both
// new lanes and both format transitions. A failed setup must not consume a
// depth or retain live slots, including after discarding the old capacities.
auto verified_activation_allocation_failures(bool typed_first) -> bool
{
  using I = GD::Bc::Instruction;
  using O = I::Opcode;
  for (std::size_t point = 0; point != 1024; ++point) {
    std::ostringstream output, errors;
    GD::Bc::VM vm(output, errors, false, GD::Bc::VM::ExecutionChecks::verified);
    GD::Bc::Instructions code{I(O::array, GD::Bc::Array('a')), I(O::load, I::Offset(-1)),
                              I(O::variable, GD::Bc::Variable('a'))};
    for (unsigned i = 0; i != 67; ++i) {
      code.emplace_back(O::number, "12345678901234567890123456789012345678901234567890");
      code.emplace_back(O::branch, I::Offset(1));
    }
    code.emplace_back(O::eof);
    GD::Bc::Instructions warm{I(O::eof)};
    if (typed_first) {
      warm = code;
      // The target is too small for typed execution, but still constructs
      // an owning number and an array snapshot in its generic buffer.
      code.erase(code.begin() + 3, code.end());
      code.emplace_back(O::number, "12345678901234567890123456789012345678901234567890");
      code.emplace_back(O::eof);
    }
    if (vm.execute(warm))
      return false;
    auto const before = injected_failures;
    bool rejected = false;
    {
      AllocationFailure const failure(point);
      try {
        if (vm.execute(code))
          return false;
      }
      catch (std::bad_alloc const&) {
        rejected = true;
      }
    }
    // No output/global stores or argument frames occur in this pack. Retrying
    // tests this boundary, not rollback/reuse after arbitrary runtime effects.
    if (vm.execute(code) || !output.str().empty() || !errors.str().empty())
      return false;
    if (injected_failures == before)
      return !rejected;
    if (!rejected)
      return false;
  }
  return false;  // A larger allocation count cannot silently truncate the sweep.
}
}  // namespace

auto main() -> int
{
  if (!restore_nested_call_after_exception()) {
    std::fprintf(stderr, "Nested call exception left VM locals or special registers changed\n");
    return 1;
  }
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
  if (!verified_activation_allocation_failures(false) ||
      !verified_activation_allocation_failures(true)) {
    std::fprintf(stderr, "Verified activation allocation failure damaged retry state\n");
    return 1;
  }
  if (!first_array_store_failures()) {
    std::fprintf(stderr, "Failed first array store changed visible state or prevented retry\n");
    return 1;
  }
  if (!restore_call_after_allocation_failure()) {
    std::fprintf(stderr, "Allocation failure damaged an active call or its retry\n");
    return 1;
  }
  if (!restore_private_call_after_allocation_failure()) {
    std::fprintf(stderr, "Private scalar allocation failure damaged recursive activation state\n");
    return 1;
  }
  auto const before = outstanding.load(std::memory_order_relaxed);
  if (!parser_allocation_failures()) {
    std::fprintf(stderr, "Repeated parser allocation sweep lost terminal state\n");
    return 1;
  }
  if (!verified_activation_allocation_failures(false) ||
      !verified_activation_allocation_failures(true)) {
    std::fprintf(stderr, "Repeated verified activation sweep retained state\n");
    return 1;
  }
  if (!first_array_store_failures()) {
    std::fprintf(stderr, "Repeated array failure sweep changed visible state\n");
    return 1;
  }
  if (!restore_call_after_allocation_failure()) {
    std::fprintf(stderr, "Repeated call allocation sweep damaged recovery\n");
    return 1;
  }
  if (!restore_private_call_after_allocation_failure()) {
    std::fprintf(stderr, "Repeated private scalar failure sweep damaged recovery\n");
    return 1;
  }
  if (!native_sqrt_scale_boundary<GD::Bc::NumberTraits16>() ||
      !native_sqrt_scale_boundary<GD::Bc::NumberTraits32>()) {
    std::fprintf(stderr, "Native square-root scale wrapped or lost snapshot isolation\n");
    return 1;
  }
  if (!shrinking_division_storage<GD::Bc::NumberTraits8>() ||
      !shrinking_division_storage<GD::Bc::NumberTraits16>() ||
      !shrinking_division_storage<GD::Bc::NumberTraits32>()) {
    std::fprintf(stderr, "Scale-reducing division padded its divisor or lost snapshots\n");
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
