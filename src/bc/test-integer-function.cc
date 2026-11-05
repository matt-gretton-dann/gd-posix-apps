/** Independent semantic and guard tests for the portable integer tier.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <catch2/catch.hpp>

#include <limits>
#include <memory>
#include <sstream>

#include "integer-function.hh"

namespace {
using namespace GD::Bc;
using namespace GD::Bc::Details;
using I = Instruction;
using O = I::Opcode;
using D = I::Offset;
auto running() noexcept -> bool { return false; }
auto interrupted() noexcept -> bool { return true; }

auto prologue() -> Instructions
{
  return {I(O::pop_param), I(O::variable, Variable('x')), I(O::store, D(-1), D(-2)),
          I(O::pop_param_mark)};
}
auto factorial() -> Instructions
{
  auto code = prologue();
  for (auto const& instruction :
       {I(O::variable, Variable('x')), I(O::number, "1"), I(O::less_than_equals, D(-2), D(-1)),
        I(O::branch_zero, D(-1), D(3)), I(O::number, "1"), I(O::return_, D(-1)),
        I(O::push_param_mark), I(O::variable, Variable('x')), I(O::number, "1"),
        I(O::subtract, D(-2), D(-1)), I(O::push_param, D(-1)),
        I(O::call, Letter('f'), Location("integer-test")), I(O::variable, Variable('x')),
        I(O::multiply, D(-1), D(-2)), I(O::return_, D(-1))})
    code.push_back(instruction);
  return code;
}
}  // namespace

TEST_CASE("Pure integer functions use bounded iterative self calls", "[bc][integer-function]")
{
  auto const plan = plan_integer_function(factorial(), VariableMask("x", ""), Letter('f'));
  REQUIRE(plan);
  IntegerFunctionScratch scratch;
  std::int64_t expected = 1;
  for (std::int64_t n = 0; n <= 20; ++n) {
    if (n != 0)
      expected *= n;
    CHECK(run_integer_function(*plan, n, scratch, running) == expected);
  }
  CHECK_FALSE(run_integer_function(*plan, 21, scratch, running));
  CHECK_FALSE(run_integer_function(*plan, 100000, scratch, running));
  CHECK_FALSE(run_integer_function(*plan, 5, scratch, interrupted));
  CHECK(scratch.values.empty());
  CHECK(scratch.frames.empty());
  CHECK(scratch.arguments.empty());
  CHECK(run_integer_function(*plan, 5, scratch, running) == 120);
}

TEST_CASE("Pure integer operands preserve locations until consumed", "[bc][integer-function]")
{
  auto source = prologue();
  source.emplace_back(O::variable, Variable('x'));
  source.emplace_back(O::variable, Variable('x'));
  source.emplace_back(O::number, "2");
  source.emplace_back(O::store, D(-2), D(-1));
  source.emplace_back(O::add, D(-4), D(-2));
  source.emplace_back(O::return_, D(-1));
  auto plan = plan_integer_function(source, VariableMask("x", ""), Letter('f'));
  REQUIRE(plan);
  IntegerFunctionScratch scratch;
  CHECK(run_integer_function(*plan, 7, scratch, running) == 4);
  // An explicit load is a snapshot even if the underlying scalar changes.
  source.insert(source.begin() + 5, I(O::load, D(-1)));
  source[9] = I(O::add, D(-4), D(-2));
  plan = plan_integer_function(source, VariableMask("x", ""), Letter('f'));
  REQUIRE(plan);
  CHECK(run_integer_function(*plan, 7, scratch, running) == 9);
  source[5] = I(O::load, D(-4));  // Prologue references are deliberately rejected.
  CHECK_FALSE(plan_integer_function(source, VariableMask("x", ""), Letter('f')));
}

TEST_CASE("Pure integer plans reject observable effects and base dependence",
          "[bc][integer-function]")
{
  auto source = factorial();
  CHECK_FALSE(plan_integer_function(source, VariableMask("y", ""), Letter('f')));
  CHECK_FALSE(plan_integer_function(source, VariableMask("x", "a"), Letter('f')));
  source[5] = I(O::number, "10");
  CHECK_FALSE(plan_integer_function(source, VariableMask("x", ""), Letter('f')));
  source = factorial();
  source[15] = I(O::call, Letter('g'), Location("integer-test"));
  CHECK_FALSE(plan_integer_function(source, VariableMask("x", ""), Letter('f')));
  for (auto const& instruction :
       {I(O::print, D(-1), I::Stream::output), I(O::scale), I(O::ibase), I(O::obase),
        I(O::array, Array('a')), I(O::divide, D(-1), D(-1)), I(O::power, D(-1), D(-1)),
        I(O::pop_param), I(O::pop_param_array), I(O::quit, 0U)}) {
    source = factorial();
    source[5] = instruction;
    CHECK_FALSE(plan_integer_function(source, VariableMask("x", ""), Letter('f')));
  }
}

TEST_CASE("Pure integer arithmetic guards both signed endpoints", "[bc][integer-function]")
{
  for (auto opcode : {O::add, O::subtract, O::multiply}) {
    auto source = prologue();
    source.emplace_back(O::variable, Variable('x'));
    source.emplace_back(O::number, "2");
    source.emplace_back(opcode, D(-2), D(-1));
    source.emplace_back(O::return_, D(-1));
    auto const plan = plan_integer_function(source, VariableMask("x", ""), Letter('f'));
    REQUIRE(plan);
    IntegerFunctionScratch scratch;
    auto const min = std::numeric_limits<std::int64_t>::min();
    auto const max = std::numeric_limits<std::int64_t>::max();
    CHECK(run_integer_function(*plan, 7, scratch, running) == (opcode == O::add        ? 9
                                                               : opcode == O::subtract ? 5
                                                                                       : 14));
    CHECK(run_integer_function(*plan, min, scratch, running).has_value() == (opcode == O::add));
    CHECK(run_integer_function(*plan, max, scratch, running).has_value() ==
          (opcode == O::subtract));
  }
}

TEST_CASE("Pure integer VM calls agree with the full diagnostic interpreter",
          "[bc][integer-function]")
{
  for (auto const* definition :
       {"define f(x) {\nauto y\ny=x+1\nreturn (x*y)\n}\n",
        "define f(x) {\nif(x<=1) return (1)\nreturn (x*f(x-1))\n}\n",
        "define f(x) {\nreturn (x+(x=2))\n}\n",
        "define f(x) {\nauto y\ny=x\nx=2\nreturn (x+y)\n}\n",
        "define f(x) {\nauto y\ny=0\nwhile(x>0) {\ny=y+x\nx=x-1\n}\nreturn (y)\n}\n"}) {
    std::string const text = std::string(definition) +
                             "for(i=0;i<20;i++) a=f(5)\na\n"
                             "f(9223372036854775807)\nf(-9223372036854775808)\nf(1.0)\n"
                             "scale=2\nf(5)\nscale=0\n"
                             "define f(x) {\nreturn (x+2)\n}\nf(5)\n";
    // Avoid impractically large recursive/loop arguments; overflow fallback
    // belongs to the arithmetic cases, not to unbounded generic recursion.
    auto program = text;
    if (program.find("f(x-1)") != std::string::npos || program.find("while") != std::string::npos) {
      auto const begin = program.find("f(922337");
      auto const end = program.find("f(1.0)", begin);
      program.erase(begin, end - begin);
    }
    auto run = [&](VM::ExecutionChecks checks) {
      Parser parser(std::make_unique<Lexer>(std::make_unique<StringReader>(program)), false);
      auto source = parser.parse();
      REQUIRE(source);
      std::ostringstream out;
      std::ostringstream err;
      VM vm(out, err, false, checks);
      CHECK_FALSE(vm.execute(*source));
      CHECK(err.str().empty());
      return out.str();
    };
    INFO(definition);
    CHECK(run(VM::ExecutionChecks::verified) == run(VM::ExecutionChecks::full));
  }
}
