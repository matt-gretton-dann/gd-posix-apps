/** Isolated staged-conversion allocation failure, without production test hooks.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <cstdio>
#include <cstdlib>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>

#include "bc.hh"

namespace {
namespace P = GD::Bc::Profile;
thread_local bool armed = false;
thread_local std::size_t remaining = 0;
thread_local unsigned failures = 0;
thread_local std::uint64_t initial_attempts = 0;
thread_local std::uint64_t initial_successes = 0;
thread_local std::uint64_t initial_fallbacks = 0;

auto event(P::Snapshot const& snapshot, P::Event which) -> std::uint64_t
{
  return snapshot.events[static_cast<std::size_t>(which)];
}
}  // namespace

#if defined(_MSC_VER)
#  define GD_BC_TEST_NOINLINE __declspec(noinline)
#elif defined(__GNUC__) || defined(__clang__)
#  define GD_BC_TEST_NOINLINE [[gnu::noinline]]
#else
#  define GD_BC_TEST_NOINLINE
#endif

// Only this small test executable replaces allocation. Snapshot has no
// allocation, and ordinary allocator calls occur outside the profile gate.
GD_BC_TEST_NOINLINE auto operator new(std::size_t size) -> void*
{
  if (armed) {
    auto const snapshot = P::snapshot();
    if (event(snapshot, P::Event::portable_scalar_attempts) > initial_attempts &&
        event(snapshot, P::Event::portable_scalar_successes) == initial_successes &&
        event(snapshot, P::Event::portable_scalar_fallbacks) == initial_fallbacks) {
      if (remaining == 0) {
        armed = false;  // Allow exception handling and interpreter replay.
        ++failures;
        throw std::bad_alloc();
      }
      --remaining;
    }
  }
  while (true) {
    if (auto* value = std::malloc(size == 0 ? 1 : size))
      return value;
    auto const handler = std::get_new_handler();
    if (!handler)
      throw std::bad_alloc();
    handler();
  }
}
GD_BC_TEST_NOINLINE void operator delete(void* value) noexcept { std::free(value); }
auto operator new[](std::size_t size) -> void* { return ::operator new(size); }
void operator delete[](void* value) noexcept { ::operator delete(value); }
void operator delete(void* value, std::size_t) noexcept { ::operator delete(value); }
void operator delete[](void* value, std::size_t) noexcept { ::operator delete(value); }
#undef GD_BC_TEST_NOINLINE

namespace {
auto parse(std::string const& source) -> GD::Bc::Instructions
{
  GD::Bc::Parser parser(
    std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(source)), false);
  auto code = parser.parse();
  if (!code)
    throw std::logic_error("missing fault-test instruction stream");
  if (!code->empty() && code->back().opcode() == GD::Bc::Instruction::Opcode::eof)
    code->pop_back();
  return *code;
}

auto exercise(std::size_t position, bool& injected) -> bool
{
  std::ostringstream output;
  std::ostringstream errors;
  GD::Bc::VM vm(output, errors, false, GD::Bc::VM::ExecutionChecks::verified);
  auto setup = parse("{a=7;a[0]=11;}\n");
  if (!vm.execute(setup))
    return false;
  // The initializer reads old i. Premature publication of i or s before a
  // later conversion failure would therefore corrupt interpreter replay.
  auto code = parse("for(i=i+18446744073709551614;i<18446744073709551615;i++)s+=i\n");
  auto const before = P::snapshot();
  initial_attempts = event(before, P::Event::portable_scalar_attempts);
  initial_successes = event(before, P::Event::portable_scalar_successes);
  initial_fallbacks = event(before, P::Event::portable_scalar_fallbacks);
  remaining = position;
  failures = 0;
  armed = true;
  bool continued = false;
  try {
    continued = vm.execute(code);
  }
  catch (...) {
    armed = false;
    return false;
  }
  armed = false;
  auto const after = P::snapshot();
  injected = failures == 1;
  if (!continued || failures > 1 ||
      event(after, P::Event::portable_scalar_attempts) - initial_attempts != 1 ||
      event(after, P::Event::portable_scalar_successes) - initial_successes != (injected ? 0 : 1) ||
      event(after, P::Event::portable_scalar_fallbacks) - initial_fallbacks != (injected ? 1 : 0)) {
    return false;
  }
  auto inspect = parse("{i;s;scale(s);a;a[0];}\n");
  return vm.execute(inspect) && errors.str().empty() &&
         output.str() == "18446744073709551615\n18446744073709551614\n0\n7\n11\n";
}
}  // namespace

auto main() -> int
{
  unsigned denied = 0;
  for (std::size_t position = 0; position < 64; ++position) {
    bool injected = false;
    if (!exercise(position, injected)) {
      std::fprintf(stderr, "staged scalar conversion/replay failed at allocation %zu\n", position);
      return 1;
    }
    if (injected) {
      ++denied;
      continue;
    }
    if (denied == 0) {
      // Some native string/Number representations keep every uint64 result
      // inline. Do not insert a production allocation merely to hit a catch.
      std::printf("no ordinary staged allocations observed for this fixture/build; complete "
                  "publication verified\n");
      return 0;
    }
    std::printf(
      "%u staged allocation failures replayed; complete publication and unrelated state checked\n",
      denied);
    return 0;
  }
  std::fprintf(stderr, "staged conversion fault sweep never reached a successful publication\n");
  return 1;
}
