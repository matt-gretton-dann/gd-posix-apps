/** Independent value/domain oracles for cached native leaf code.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <catch2/catch.hpp>

#include <array>
#include <cstdint>
#include <limits>
#include <sstream>
#include <string>
#include <vector>

#include "bc.hh"
#include "native-expression.hh"

namespace {
using namespace GD::Bc::Details;
using Op = NativeExpressionOp;
using K = Op::Kind;
}  // namespace

TEST_CASE("Cached native expressions preserve input and exact results", "[bc][native-expression]")
{
  std::array<Op, 4> const square{
    {{K::argument}, {K::multiply, 0, 0}, {K::constant, 0, 0, 1}, {K::add, 1, 2}}};
  auto code = NativeExpression::compile(square, 3);
  REQUIRE(code);
  CHECK_FALSE(code->code_bytes().empty());
  for (std::uint64_t x :
       {0ULL, 1ULL, 2ULL, 19ULL, 65535ULL, 200000ULL, 999999999ULL, 3037000499ULL}) {
    std::uint64_t result = 77;
    REQUIRE(code->execute(x, result));
    CHECK(result == x * x + 1);
  }
  for (auto value : {3037000500ULL, std::numeric_limits<unsigned long long>::max()}) {
    std::uint64_t unchanged = 77;
    CHECK_FALSE(code->execute(value, unchanged));
    CHECK(unchanged == 77);
  }
  std::uint64_t retry = 0;
  CHECK(code->execute(7, retry));
  CHECK(retry == 50);
}

TEST_CASE("Native expression register reuse preserves independent values",
          "[bc][native-expression]")
{
  std::array<Op, 6> const formula{{{K::argument},
                                   {K::constant, 0, 0, 3},
                                   {K::add, 0, 1},
                                   {K::constant, 0, 0, 5},
                                   {K::add, 0, 3},
                                   {K::multiply, 2, 4}}};
  auto code = NativeExpression::compile(formula, 5);
  REQUIRE(code);
  for (std::uint64_t n = 0; n != 1024; ++n) {
    std::uint64_t result = 0;
    REQUIRE(code->execute(n, result));
    CHECK(result == (n + 3) * (n + 5));
  }
  auto retained = NativeExpression::compile(formula, 2);
  REQUIRE(retained);
  std::uint64_t result = 0;
  REQUIRE(retained->execute(7, result));
  CHECK(result == 10);
}

TEST_CASE("Native expression signed bounds decline before publication", "[bc][native-expression]")
{
  std::array<Op, 3> const add{{{K::argument}, {K::constant, 0, 0, 1}, {K::add, 0, 1}}};
  auto code = NativeExpression::compile(add, 2);
  REQUIRE(code);
  auto const maximum = static_cast<std::uint64_t>(std::numeric_limits<std::int64_t>::max());
  std::uint64_t result = 123;
  REQUIRE(code->execute(maximum - 1, result));
  CHECK(result == maximum);
  CHECK_FALSE(code->execute(maximum, result));
  CHECK(result == maximum);
  CHECK_FALSE(code->execute(maximum + 1, result));
  CHECK(result == maximum);
}

TEST_CASE("Native expression limits retain generic fallback", "[bc][native-expression]")
{
  CHECK_FALSE(NativeExpression::compile({}, 0));
  std::array<Op, 1> const forward{{{K::add, 0, 0}}};
  CHECK_FALSE(NativeExpression::compile(forward, 0));
  std::array<Op, 1> const unknown{{{static_cast<K>(255)}}};
  CHECK_FALSE(NativeExpression::compile(unknown, 0));
  std::array<Op, 1> const constant{
    {{K::constant, 0, 0, std::numeric_limits<std::uint64_t>::max()}}};
  CHECK_FALSE(NativeExpression::compile(constant, 0));
  std::vector<Op> too_large(65, Op{K::argument});
  CHECK_FALSE(NativeExpression::compile(too_large, 0));
  // Six simultaneously live constants exceed the five-register leaf budget.
  // A fallback is required rather than an unrecorded spill/stack frame.
  std::array<Op, 11> const pressure{{{K::constant, 0, 0, 1},
                                     {K::constant, 0, 0, 2},
                                     {K::constant, 0, 0, 3},
                                     {K::constant, 0, 0, 4},
                                     {K::constant, 0, 0, 5},
                                     {K::constant, 0, 0, 6},
                                     {K::add, 0, 1},
                                     {K::add, 2, 3},
                                     {K::add, 4, 5},
                                     {K::add, 6, 7},
                                     {K::add, 9, 8}}};
  CHECK_FALSE(NativeExpression::compile(pressure, 10));
}

namespace {
auto run_functions(std::string const& source, GD::Bc::VM::ExecutionChecks checks) -> std::string
{
  GD::Bc::Parser parser(
    std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(source)), false);
  std::ostringstream output;
  std::ostringstream errors;
  GD::Bc::VM vm(output, errors, false, checks);
  while (auto code = parser.parse()) {
    REQUIRE_FALSE(vm.execute(*code));
    if (!code->empty() && code->back().opcode() == GD::Bc::Instruction::Opcode::eof)
      break;
  }
  CHECK(errors.str().empty());
  return output.str();
}
}  // namespace

TEST_CASE("Native function calls preserve exact values and dynamic bindings",
          "[bc][native-expression][vm]")
{
  std::string const source = "define f(x) {\nreturn(x*x+1)\n}\n"
                             "x=73\nf(0)\nf(7)\nf(999999999)\nx\n"
                             "f(-2)\nf(1.5)\nf(1000000000)\n"
                             "define f(x) {\nreturn(x+5)\n}\nf(7)\nx\n"
                             "ibase=2\nf(1)\n";
  std::string const expected =
    "1\n50\n999999998000000002\n73\n5\n3.2\n1000000000000000001\n12\n73\n6\n";
  CHECK(run_functions(source, GD::Bc::VM::ExecutionChecks::full) == expected);
  CHECK(run_functions(source, GD::Bc::VM::ExecutionChecks::verified) == expected);
}

TEST_CASE("Native function proof retains generic effects and base decoding",
          "[bc][native-expression][vm]")
{
  for (auto const& body : {"x=x+1; return(x)", "return(x+g)", "return(x+10)",
                           "if(x) return(3); return(4)", "return(g(x))"}) {
    std::string const source = "g=5\ndefine g(x) {\nreturn(x+2)\n}\n"
                               "define f(x) {\n" +
                               std::string(body) +
                               "\n}\n"
                               "x=17\nf(7)\nx\nibase=2\nf(1)\nx\n";
    auto const checked = run_functions(source, GD::Bc::VM::ExecutionChecks::full);
    CHECK(run_functions(source, GD::Bc::VM::ExecutionChecks::verified) == checked);
  }
}

#if GD_BC_PROFILE
TEST_CASE("Native function profiling proves cached execution and fallback",
          "[bc][native-expression][vm]")
{
  using namespace GD::Bc;
  auto const attempts = static_cast<std::size_t>(Profile::Event::native_function_attempts);
  auto const completed = static_cast<std::size_t>(Profile::Event::native_function_completions);
  auto before = Profile::snapshot();
  std::string const source = "define f(x) {\nreturn(x*x+1)\n}\nf(7)\nf(8)\nf(-2)\n";
  CHECK(run_functions(source, VM::ExecutionChecks::verified) == "50\n65\n5\n");
  auto after = Profile::snapshot();
  CHECK(after.events[attempts] - before.events[attempts] == 2);
  CHECK(after.events[completed] - before.events[completed] == 2);
  before = after;
  CHECK(run_functions(source, VM::ExecutionChecks::full) == "50\n65\n5\n");
  after = Profile::snapshot();
  CHECK(after.events[attempts] == before.events[attempts]);
  CHECK(after.events[completed] == before.events[completed]);
  before = after;
  std::string const overflow = "define f(x) {\nreturn(x*x*x+1)\n}\nf(999999999)\n";
  CHECK(run_functions(overflow, VM::ExecutionChecks::verified) == "999999997000000003000000000\n");
  after = Profile::snapshot();
  CHECK(after.events[attempts] - before.events[attempts] == 1);
  CHECK(after.events[completed] == before.events[completed]);
}
#endif
