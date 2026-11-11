/** Independent value, fallback and resource checks for integer reductions.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <catch2/catch.hpp>

#include <algorithm>
#include <memory>
#include <sstream>
#include <string>

#include "bc.hh"
#include "integer-recurrence.hh"

namespace {
using namespace GD::Bc;
using Details::IntegerRecurrence;
auto run(std::string const& source, VM::ExecutionChecks checks) -> std::string
{
  Parser parser(std::make_unique<Lexer>(std::make_unique<StringReader>(source)), false);
  std::ostringstream output;
  std::ostringstream errors;
  VM vm(output, errors, false, checks);
  while (auto code = parser.parse()) {
    REQUIRE_FALSE(vm.execute(*code));
    if (!code->empty() && code->back().opcode() == Instruction::Opcode::eof)
      break;
  }
  CHECK(errors.str().empty());
  return output.str();
}
auto recognize(std::string const& body) -> std::optional<IntegerRecurrence>
{
  std::string source = "define f(n) {\n" + body + "\n}\n";
  Parser parser(std::make_unique<Lexer>(std::make_unique<StringReader>(source)), false);
  auto code = parser.parse();
  REQUIRE(code);
  REQUIRE(code->front().opcode() == Instruction::Opcode::function_begin);
  auto end = std::find_if(code->begin(), code->end(), [](auto const& instruction) {
    return instruction.opcode() == Instruction::Opcode::function_end;
  });
  REQUIRE(end != code->end());
  auto locals = std::get<VariableMask>(code->front().op1());
  return IntegerRecurrence::match(
    {code->data() + 1, static_cast<std::size_t>(end - code->begin() - 1)}, locals, Letter('f'));
}
unsigned interrupt_checks;
auto interrupt() -> bool { return ++interrupt_checks > 3; }
auto uninterrupted() -> bool { return false; }
}  // namespace

TEST_CASE("Pure integer reductions preserve arbitrary precision recursion",
          "[bc][vm][integer-recurrence]")
{
  // Vary threshold, decrement, base value, operand order and operator.
  // Full checked execution uses ordinary recursive evaluation.
  for (auto const& body :
       {"if(n<=1)return(1); return(n*f(n-1))", "if(n<2)return(1); return(f(n-1)*n)",
        "if(n>1)return(n*f(n-1)); return(1)", "if(n>=2)return(f(n-1)*n); return(1)",
        "if(n<=3)return(n); return(n+f(n-2))", "if(n<0)return(n); return(f(n-3)+n)",
        "if(n<= -9)return(-2); return(n*f(n-3))",
        "if(n< -8)return(n); return(f(n-3)+n)",
        "if(n<=4)return(-2); return(n*f(n-3))", "if(n<=2)return(0); return(f(n-2)+n)"}) {
    std::string const source = "define f(n) {\n" + std::string(body) +
                               "\n}\nn=73\nf(0)\nf(1)\nf(2)\nf(7)\nf(31)\nf(50)\nn\n"
                               "scale=17\nf(57)\nscale\nn\nibase=2\nf(111)\nibase\n";
    INFO(body);
    REQUIRE(recognize(body));
    CHECK(run(source, VM::ExecutionChecks::verified) == run(source, VM::ExecutionChecks::full));
  }
  auto const factorial = "define f(n) {\nif(n<=1)return(1)\nreturn(n*f(n-1))\n}\nf(50)\n";
  CHECK(run(factorial, VM::ExecutionChecks::verified) ==
        "30414093201713378043612608166064768844377641568960512000000000000\n");
}

TEST_CASE("Integer reductions preserve negative intermediate factors",
          "[bc][vm][integer-recurrence]")
{
  IntegerRecurrence product{Letter('f'), -9, -2, 3, false, true};
  IntegerRecurrence sum{Letter('f'), -9, 0, 3, true, false};
  Number result(77);
  REQUIRE(product.execute(1, result, uninterrupted));
  CHECK(result == Number(160));
  REQUIRE(sum.execute(1, result, uninterrupted));
  Number expected(25);
  expected.negate();
  CHECK(result == expected);
  auto const source = "define f(n) {\nif(n<= -9)return(-2)\nreturn(n*f(n-3))\n}\nf(1)\n"
                      "define f(n) {\nif(n< -8)return(n)\nreturn(f(n-3)+n)\n}\nf(1)\n";
  CHECK(run(source, VM::ExecutionChecks::full) == "160\n-25\n");
  CHECK(run(source, VM::ExecutionChecks::verified) == "160\n-25\n");
}

TEST_CASE("Integer reduction rejects effects and retains generic call semantics",
          "[bc][vm][integer-recurrence]")
{
  for (auto const& body :
       {"if(n<=1)return(1); n=n-1; return((n+1)*f(n))", "if(n<=1)return(1); g+=1; return(n*f(n-1))",
        "if(n<=1)return(1); return(n*f(n-1)+g)", "if(n<=1)return(1); return(n/f(n-1))",
        "if(n<=1)return(1); return(n-f(n-1))", "if(n<=1)return(1); return(n*f(n-1)+f(n-2))",
        "if(n<=1)return(1); return(n*h(n-1))", "if(n<=1)return(10); return(n*f(n-1))",
        "if(n<=1)return(1); scale=3; return(n*f(n-1))"}) {
    std::string const source = "define h(n) {\nreturn(n+1)\n}\ndefine f(n) {\n" +
                               std::string(body) + "\n}\ng=5\nn=73\nf(7)\ng\nn\nscale\n";
    INFO(body);
    CHECK_FALSE(recognize(body));
    CHECK(run(source, VM::ExecutionChecks::verified) == run(source, VM::ExecutionChecks::full));
  }
  std::string const inputs = "define f(n) {\nif(n<=1)return(1)\nreturn(n*f(n-1))\n}\n"
                             "f(-1)\nf(.5)\nf(4.0)\nf(5.5)\n"
                             "define f(n) {\nif(n<=2)return(7)\nreturn(n+f(n-2))\n}\nf(7)\n";
  CHECK(run(inputs, VM::ExecutionChecks::verified) == run(inputs, VM::ExecutionChecks::full));
}

TEST_CASE("Integer reduction bounds and interruptions preserve the output owner",
          "[bc][integer-recurrence]")
{
  IntegerRecurrence factorial{Letter('f'), 1, 1, 1, false, true};
  CHECK(factorial.admits(50));
  CHECK_FALSE(factorial.admits(65537));
  CHECK_FALSE(factorial.admits(1000000000));
  Number output(77);
  interrupt_checks = 0;
  CHECK_FALSE(factorial.execute(50, output, interrupt));
  CHECK(output == Number(77));
  CHECK_FALSE(factorial.execute(65537, output, uninterrupted));
  CHECK(output == Number(77));
  REQUIRE(factorial.execute(5, output, uninterrupted));
  CHECK(output == Number(120));
  IntegerRecurrence sum{Letter('f'), 3, 0, 2, true, false};
  REQUIRE(sum.execute(65535, output, uninterrupted));
  CHECK(output == Number("1073741823", 10));
  CHECK_FALSE(sum.execute(131076, output, uninterrupted));
  CHECK(output == Number("1073741823", 10));
}

#if GD_BC_PROFILE
TEST_CASE("Integer recurrence profiling proves folding of the real factorial shape",
          "[bc][vm][integer-recurrence]")
{
  auto const calls = static_cast<std::size_t>(Profile::Event::function_calls);
  auto before = Profile::snapshot();
  CHECK(run("define f(n) {\nif(n<=1)return(1)\nreturn(n*f(n-1))\n}\nf(50)\n",
            VM::ExecutionChecks::verified) ==
        "30414093201713378043612608166064768844377641568960512000000000000\n");
  auto after = Profile::snapshot();
  CHECK(after.events[calls] - before.events[calls] == 1);
  before = after;
  CHECK(run("define f(n) {\nif(n<=3)return(n)\nreturn(n+f(n-2))\n}\nf(65535)\n",
            VM::ExecutionChecks::verified) == "1073741823\n");
  after = Profile::snapshot();
  CHECK(after.events[calls] - before.events[calls] == 1);
}
#endif
