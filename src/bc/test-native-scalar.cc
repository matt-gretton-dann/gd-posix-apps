/** Native execution must agree with independent integer and checked VM oracles.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <catch2/catch.hpp>

#include <cstdint>
#include <limits>
#include <sstream>
#include <string>

#include "native-scalar.hh"
#include "verification.hh"

#if defined(_WIN32)
#  ifndef WIN32_LEAN_AND_MEAN
#    define WIN32_LEAN_AND_MEAN
#  endif
#  ifndef NOMINMAX
#    define NOMINMAX
#  endif
#  include <windows.h>
#  if defined(_MSC_VER)
#    include <intrin.h>
#  endif
#endif

namespace {
using namespace GD::Bc;
using namespace GD::Bc::Details;
using I = Instruction;
using O = I::Opcode;
using D = I::Offset;
unsigned polls = 0;
bool stop_on_poll = false;
auto check_interrupt() noexcept -> bool
{
  ++polls;
  return stop_on_poll;
}

auto loop(std::uint64_t limit, std::string const& initial = "0") -> Instructions
{
  // i=0; s=initial; while(i<limit) { s+=i; i+=1; }
  return {I(O::variable, Variable('i')),
          I(O::number, "0"),
          I(O::store, D(-2), D(-1)),
          I(O::variable, Variable('s')),
          I(O::number, initial),
          I(O::store, D(-2), D(-1)),
          I(O::load, D(-6)),
          I(O::number, std::to_string(limit)),
          I(O::less_than, D(-2), D(-1)),
          I(O::branch_zero, D(-1), D(10)),
          I(O::load, D(-7)),
          I(O::load, D(-11)),
          I(O::add, D(-2), D(-1)),
          I(O::store, D(-10), D(-1)),
          I(O::load, D(-14)),
          I(O::number, "1"),
          I(O::add, D(-2), D(-1)),
          I(O::store, D(-17), D(-1)),
          I(O::branch, D(-12))};
}

auto checked_output(Instructions code, VM::ExecutionChecks checks) -> std::string
{
  std::ostringstream out, err;
  VM vm(out, err, false, checks);
  CHECK(vm.execute(code));
  Instructions query{I(O::variable, Variable('s')), I(O::load, D(-1)),
                     I(O::print, D(-1), I::Stream::output), I(O::eof)};
  CHECK_FALSE(vm.execute(query));
  CHECK(err.str().empty());
  return out.str();
}
}  // namespace

TEST_CASE("Native scalar execution preserves exact loop results", "[bc][native-scalar]")
{
  stop_on_poll = false;
  for (std::uint64_t n :
       {0U, 1U, 2U, 19U, 127U, 255U, 256U, 257U, 1023U, 1024U, 1025U, 2048U, 32768U}) {
    auto code = loop(n);
    REQUIRE_FALSE(verify_control_flow(code, false).has_value());
    auto const variables = native_scalar_variables(code);
    REQUIRE(variables.count() == 2);
    polls = 0;
    auto const result = run_native_scalar(code, variables, check_interrupt);
    REQUIRE(result.has_value());
    CHECK((*result)[static_cast<unsigned>(Letter('i'))] == n);
    CHECK((*result)[static_cast<unsigned>(Letter('s'))] == (n == 0 ? 0 : n * (n - 1) / 2));
    if (n >= 1024)
      CHECK(polls > 0);
  }
}

TEST_CASE("Native scalar failure leaves the checked interpreter usable", "[bc][native-scalar]")
{
  auto code = loop(2, std::to_string(std::numeric_limits<std::uint64_t>::max()));
  REQUIRE_FALSE(verify_control_flow(code, false).has_value());
  stop_on_poll = false;
  CHECK_FALSE(run_native_scalar(code, native_scalar_variables(code), check_interrupt).has_value());
  CHECK(checked_output(code, VM::ExecutionChecks::full) == "18446744073709551616\n");
  CHECK(checked_output(code, VM::ExecutionChecks::verified) == "18446744073709551616\n");
  for (auto n : {0U, 17U, 1025U}) {
    auto ordinary = loop(n);
    CHECK(checked_output(ordinary, VM::ExecutionChecks::verified) ==
          checked_output(ordinary, VM::ExecutionChecks::full));
  }
}

TEST_CASE("Native scalar loops keep fuel and interrupt bounds", "[bc][native-scalar]")
{
  auto code = loop(4'000'002);
  REQUIRE_FALSE(verify_control_flow(code, false).has_value());
  polls = 0;
  stop_on_poll = true;
  CHECK_FALSE(run_native_scalar(code, native_scalar_variables(code), check_interrupt).has_value());
  CHECK(polls == 1);
  polls = 0;
  stop_on_poll = false;
  CHECK_FALSE(run_native_scalar(code, native_scalar_variables(code), check_interrupt).has_value());
  CHECK(polls > 0);
  auto finite = loop(2048);
  auto result = run_native_scalar(finite, native_scalar_variables(finite), check_interrupt);
  REQUIRE(result.has_value());
  CHECK((*result)[static_cast<unsigned>(Letter('i'))] == 2048);
}

TEST_CASE("Native scalar proof keeps unsupported operations on the interpreter",
          "[bc][native-scalar]")
{
  auto code = loop(17);
  code[12] = I(O::multiply, D(-2), D(-1));
  REQUIRE_FALSE(verify_control_flow(code, false).has_value());
  CHECK(native_scalar_variables(code).none());
  auto too_large = loop(17);
  too_large.insert(too_large.end(), native_scalar_result_slots, I(O::number, "0"));
  CHECK(native_scalar_variables(too_large).none());
}

#if GD_BC_PROFILE
TEST_CASE("Native scalar profiling distinguishes completion and fallback", "[bc][native-scalar]")
{
  auto const attempts = static_cast<std::size_t>(Profile::Event::native_scalar_attempts);
  auto const completed = static_cast<std::size_t>(Profile::Event::native_scalar_completions);
  auto before = Profile::snapshot();
  CHECK(checked_output(loop(17), VM::ExecutionChecks::verified) == "136\n");
  auto after = Profile::snapshot();
  CHECK(after.events[attempts] - before.events[attempts] == 1);
  CHECK(after.events[completed] - before.events[completed] == 1);
  before = after;
  CHECK(checked_output(loop(17), VM::ExecutionChecks::full) == "136\n");
  after = Profile::snapshot();
  CHECK(after.events[attempts] == before.events[attempts]);
  CHECK(after.events[completed] == before.events[completed]);
  before = after;
  CHECK(checked_output(loop(2, "18446744073709551615"), VM::ExecutionChecks::verified) ==
        "18446744073709551616\n");
  after = Profile::snapshot();
  CHECK(after.events[attempts] - before.events[attempts] == 1);
  CHECK(after.events[completed] == before.events[completed]);
}
#endif

#if defined(_WIN32)
namespace {
bool unwind_seen = false;
unsigned stack_depth = 0;
#  if defined(_MSC_VER)
__declspec(noinline)
#  elif defined(__GNUC__)
__attribute__((noinline))
#  endif
auto unwind_probe() noexcept -> bool
{
  DWORD64 image_base = 0;
#  if defined(_MSC_VER)
  auto const pc = reinterpret_cast<DWORD64>(_ReturnAddress());
#  else
  auto const pc = reinterpret_cast<DWORD64>(__builtin_return_address(0));
#  endif
  unwind_seen = RtlLookupFunctionEntry(pc, &image_base, nullptr) != nullptr;
  void* frames[16]{};
  stack_depth = RtlCaptureStackBackTrace(0, 16, frames, nullptr);
  return false;
}
}  // namespace
TEST_CASE("Windows native scalar callbacks have registered unwind information",
          "[bc][native-scalar]")
{
  auto code = loop(2048);
  auto result = run_native_scalar(code, native_scalar_variables(code), unwind_probe);
  REQUIRE(result.has_value());
  CHECK(unwind_seen);
  CHECK(stack_depth >= 4);
}
#endif
