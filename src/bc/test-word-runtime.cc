/** Ownership, generations and differential VM checks for the word tier.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <catch2/catch.hpp>

#include <algorithm>
#include <limits>
#include <sstream>
#include <string>

#include "word-runtime.hh"

namespace {
using namespace GD::Bc;
using namespace GD::Bc::Details;
using O = WordFunctions::Outcome;
using I = Instruction;
auto running() noexcept -> bool { return false; }
auto stopped() noexcept -> bool { return true; }
struct Definition
{
  Instructions code;
  VariableMask locals;
};
auto definition(std::string const& source) -> Definition
{
  Parser parser(std::make_unique<Lexer>(std::make_unique<StringReader>(source)), false);
  auto code = parser.parse();
  REQUIRE(code);
  auto begin = std::find_if(code->begin(), code->end(),
                            [](auto const& i) { return i.opcode() == I::Opcode::function_begin; });
  REQUIRE(begin != code->end());
  auto end = std::find_if(begin, code->end(),
                          [](auto const& i) { return i.opcode() == I::Opcode::function_end; });
  REQUIRE(end != code->end());
  return {Instructions(begin + 1, end), std::get<VariableMask>(begin->op1())};
}
auto text(Number const& value) -> std::string
{
  std::ostringstream out;
  value.output(out, 10, 0);
  return out.str();
}
auto evaluate(WordFunctions& owner, Definition const& def, std::int64_t argument) -> O
{
  return owner.try_call(0, def.code, def.locals, Letter('f'), argument, running);
}
auto run(std::string const& source, VM::ExecutionChecks checks, bool save = false) -> std::string
{
  Parser parser(std::make_unique<Lexer>(std::make_unique<StringReader>(source)), false);
  auto code = parser.parse();
  REQUIRE(code);
  std::ostringstream output, errors;
  VM vm(output, errors, save, checks);
  CHECK_FALSE(vm.execute(*code));
  CHECK(errors.str().empty());
  return output.str();
}
std::string const factorial = "define f(x) {\nif(x<=1)return(1)\nreturn(x*f(x-1))\n}\n";
}  // namespace

TEST_CASE("Ordinary word boundary retains escaped results and resets immutable generations",
          "[bc][word]")
{
#if GD_BC_PROFILE
  auto before = Profile::snapshot();
#endif
  {
    auto owner = WordFunctions::create();
    auto first = definition(factorial);
    REQUIRE(evaluate(*owner, first, 50) == O::ready);
    auto escaped = owner->take_result();
    std::string const answer = "30414093201713378043612608166064768844377641568960512000000000000";
    CHECK(text(escaped) == answer);
    REQUIRE(owner->try_call(0, first.code, first.locals, Letter('f'), 5, running) == O::ready);
    CHECK(text(owner->take_result()) == "120");
    CHECK(text(escaped) == answer);
    owner->reset(0);
    auto second = definition("define f(x) {\nreturn(x+3)\n}\n");
    REQUIRE(evaluate(*owner, second, 5) == O::ready);
    CHECK(text(owner->take_result()) == "8");
    CHECK(text(escaped) == answer);
    owner->reset(0);
    auto rejected = definition("define f(x) {\ng=x\nreturn(x)\n}\n");
    CHECK(evaluate(*owner, rejected, 5) == O::declined);
    owner->reset(0);
    REQUIRE(evaluate(*owner, first, 6) == O::ready);
    CHECK(text(owner->take_result()) == "720");
  }
#if GD_BC_PROFILE
  CHECK(Profile::snapshot().total.live_bytes == before.total.live_bytes);
#endif
}

TEST_CASE("Word boundary guards retry interruption and deterministic failure scopes", "[bc][word]")
{
  auto owner = WordFunctions::create();
  auto def = definition(factorial);
  REQUIRE(owner->try_call(0, def.code, def.locals, Letter('f'), 50, running) == O::ready);
  owner->discard_result();
  CHECK(owner->try_call(0, def.code, def.locals, Letter('f'), 50, stopped) == O::failed);
  REQUIRE(owner->try_call(0, def.code, def.locals, Letter('f'), 50, running) == O::ready);
  CHECK(text(owner->take_result()).size() > 19);
  CHECK(owner->try_call(0, def.code, def.locals, Letter('f'), 10000, running) == O::failed);
  // A cached deterministic failure still asks the VM to suppress speculation
  // throughout the subsequent generic chain; it is not an ordinary cold miss.
  CHECK(owner->try_call(0, def.code, def.locals, Letter('f'), 10000, running) == O::failed);
  REQUIRE(owner->try_call(0, def.code, def.locals, Letter('f'), 5, running) == O::ready);
  CHECK(text(owner->take_result()) == "120");
  for (char const* value : {"0", "1", "999999999"}) {
    Number input(value, 10);
    auto word = WordFunctions::argument(input);
    REQUIRE(word);
    input.negate();
    auto negative = WordFunctions::argument(input);
    REQUIRE(negative);
    CHECK(*negative == -*word);
  }
  CHECK_FALSE(WordFunctions::argument(Number("1.0", 10)));
  CHECK(WordFunctions::argument(Number("1000000000", 10)) == 1000000000LL);
  CHECK(WordFunctions::argument(Number("9223372036854775807", 10)) == 9223372036854775807LL);
  Number minimum("9223372036854775808", 10);
  minimum.negate();
  CHECK(WordFunctions::argument(minimum) == std::numeric_limits<std::int64_t>::min());
  CHECK_FALSE(WordFunctions::argument(Number("9223372036854775808", 10)));
  Number below("9223372036854775809", 10);
  below.negate();
  CHECK_FALSE(WordFunctions::argument(below));
  CHECK_FALSE(WordFunctions::argument(Number("999999999999999999999999999999999999", 10)));
}

TEST_CASE("VM word routing matches full checks across escapes, effects and redefinition",
          "[bc][word]")
{
  auto checks = GENERATE(VM::ExecutionChecks::full, VM::ExecutionChecks::verified);
  bool save = GENERATE(false, true);
  std::string source = "x=73\nb=0\n" + factorial +
                       "for(i=0;i<12;i++)z=f(50)\nr=z\nf(-2)\n"
                       "define f(x) {\nreturn(x+3)\n}\nfor(i=0;i<12;i++)z=f(2)\nz\n"
                       "define f(x) {\nb+=1\nreturn(x+b)\n}\nfor(i=0;i<10;i++)z=f(2)\nz\nb\nr\nx\n"
                       "define f(x) {\nreturn(0-x)\n}\nfor(i=0;i<12;i++)z=f(2)\nf(1.00)\n"
                       "scale=f(-0)\nscale\nibase=2\nf(3)\n";
#if GD_BC_PROFILE
  auto before = Profile::snapshot();
#endif
  auto output = run(source, checks, save);
  CHECK(output == "1\n5\n12\n10\n30414093201713378043612608166064768844377641568960512000000000000"
                  "\n73\n-1.00\n0\n-3\n");
#if GD_BC_PROFILE
  auto after = Profile::snapshot();
  auto index = static_cast<std::size_t>(Profile::Event::word_function_completions);
  CHECK((after.events[index] > before.events[index]) ==
        (GD_BC_EXPECT_WORD_FUNCTIONS && checks == VM::ExecutionChecks::verified));
#endif
}

TEST_CASE("VM integer expressions retain lazy locations and nested call snapshots", "[bc][word]")
{
  std::string source = "x=83\ny=79\ndefine f(x) {\nauto y\ny=x\nx+=1\nreturn(x-y)\n}\n"
                       "for(i=0;i<20;i++)z=f(i)\nz\nx\ny\n"
                       "define f(x) {\nif(x<=0)return(1)\nreturn(f(x-1)+f(x-1))\n}\n"
                       "for(i=0;i<12;i++)z=f(9)\nz\n"
                       "define f(x) {\nif(x<=0)return(1)\nreturn(f(f(x-1)-1))\n}\n"
                       "for(i=0;i<12;i++)z=f(5)\nz\nx\ny\n";
  auto checked = run(source, VM::ExecutionChecks::full);
  CHECK(checked == "1\n83\n79\n512\n1\n83\n79\n");
  CHECK(run(source, VM::ExecutionChecks::verified) == checked);
}

TEST_CASE("Word calls preserve nondecimal bases and a changed global scale", "[bc][word]")
{
  std::string source = "scale=7\nibase=2\nobase=8\n"
                       "define f(x) {\nauto y\ny=x*x+1\nreturn(y)\n}\n"
                       "for(i=0;i<9;i++)z=f(3)\nz\nscale\nibase\nobase\n";
  auto checked = run(source, VM::ExecutionChecks::full);
  CHECK(checked == "12\n7\n2\n10\n");
#if GD_BC_PROFILE
  auto before = Profile::snapshot();
#endif
  CHECK(run(source, VM::ExecutionChecks::verified) == checked);
#if GD_BC_PROFILE
  auto event = static_cast<std::size_t>(Profile::Event::word_function_completions);
  CHECK((Profile::snapshot().events[event] > before.events[event]) ==
        bool(GD_BC_EXPECT_WORD_FUNCTIONS));
#endif
}

TEST_CASE("A promoted word return survives destruction of its executor", "[bc][word]")
{
  std::optional<Number> escaped;
  {
    auto owner = WordFunctions::create();
    auto def = definition(factorial);
    REQUIRE(owner->try_call(0, def.code, def.locals, Letter('f'), 50, running) == O::ready);
    escaped.emplace(owner->take_result());
  }
  CHECK(text(*escaped) == "30414093201713378043612608166064768844377641568960512000000000000");
}

TEST_CASE("VM word entries accept signed extrema without changing Number APIs", "[bc][word]")
{
  std::string source = "define f(x) {\nauto y\ny=0-x\nreturn(y)\n}\n"
                       "for(i=0;i<8;i++)z=f(-9223372036854775808)\nz\n"
                       "f(9223372036854775807)\nf(9223372036854775808)\n";
  auto checked = run(source, VM::ExecutionChecks::full);
  CHECK(checked == "9223372036854775808\n-9223372036854775807\n-9223372036854775808\n");
#if GD_BC_PROFILE
  auto before = Profile::snapshot();
#endif
  CHECK(run(source, VM::ExecutionChecks::verified) == checked);
#if GD_BC_PROFILE
  auto event = static_cast<std::size_t>(Profile::Event::word_function_completions);
  CHECK((Profile::snapshot().events[event] > before.events[event]) ==
        bool(GD_BC_EXPECT_WORD_FUNCTIONS));
#endif
}
