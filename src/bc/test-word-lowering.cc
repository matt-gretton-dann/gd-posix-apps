/** Source ownership and differential oracles for pure word lowering.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <catch2/catch.hpp>

#include <algorithm>
#include <limits>
#include <sstream>

#include "word-lowering.hh"

namespace {
using namespace GD::Bc;
using namespace GD::Bc::Details;
using I = Instruction;
using O = I::Opcode;
using D = I::Offset;
auto parsed(std::string const& text) -> Instructions
{
  Parser parser(std::make_unique<Lexer>(std::make_unique<StringReader>(text)), false);
  auto code = parser.parse();
  REQUIRE(code);
  return std::move(*code);
}
auto lower(Instructions const& code) -> std::optional<Word::Program>
{
  auto begin = std::find_if(code.begin(), code.end(),
                            [](auto const& i) { return i.opcode() == O::function_begin; });
  REQUIRE(begin != code.end());
  auto end =
    std::find_if(begin, code.end(), [](auto const& i) { return i.opcode() == O::function_end; });
  REQUIRE(end != code.end());
  return lower_word_function(Instructions(begin + 1, end), std::get<VariableMask>(begin->op1()),
                             Letter('f'));
}
auto reference(std::string const& text) -> std::string
{
  auto code = parsed(text);
  std::ostringstream output, errors;
  VM vm(output, errors, false, VM::ExecutionChecks::full);
  CHECK_FALSE(vm.execute(code));
  CHECK(errors.str().empty());
  return output.str();
}
}  // namespace

TEST_CASE("Pure source lowering agrees with independent checked VM activations", "[bc][word]")
{
  for (std::string body :
       {"return(x*x+1)\n", "auto y\ny=x\nx=7\nreturn(x-y)\n",
        "auto y\nif(x<0)return(0-x)\ny=0\nwhile(x>0){y+=x;x-=1}\nreturn(y)\n",
        "if(x<=1)return(1)\nreturn(x*f(x-1))\n", "if(x<=0)return(1)\nreturn(f(x-1)+f(x-1))\n"}) {
    std::string definition = "define f(x) {\n" + body + "}\n";
    auto source = parsed(definition);
    auto program = lower(source);
    REQUIRE(program);
    // Change and destroy the original owners before using the cached plan.
    source = parsed("define f(x) {\nreturn(9)\n}\n");
    source.clear();
    for (std::int64_t input = -3; input <= 9; ++input) {
      Word::Scratch scratch;
      Word::Counters counters;
      auto result = Word::execute(*program, input, scratch, {}, counters);
      REQUIRE(result);
      CHECK_FALSE(result->big);
      CHECK(reference(definition + "f(" + std::to_string(input) + ")\n") ==
            std::to_string(result->payload) + "\n");
    }
  }
}

TEST_CASE("Lowering refuses effects and runtime-base or scale sensitive arithmetic", "[bc][word]")
{
  for (std::string definition :
       {"define f(x) {\nreturn(x+g)\n}\n", "define f(x) {\ng=x\nreturn(x)\n}\n",
        "define f(x) {\nreturn(g(x))\n}\n", "define f(x) {\nreturn(scale)\n}\n",
        "define f(x) {\nscale=0\nreturn(x)\n}\n", "define f(x) {\nreturn(ibase)\n}\n",
        "define f(x) {\nreturn(obase)\n}\n", "define f(x) {\nreturn(a[0])\n}\n",
        "define f(x) {\nauto a[]\nreturn(x)\n}\n", "define f(x) {\nreturn(x/2)\n}\n",
        "define f(x) {\nreturn(x%2)\n}\n", "define f(x) {\nreturn(x^2)\n}\n",
        "define f(x) {\nreturn(10)\n}\n", "define f(x) {\nreturn(A)\n}\n",
        "define f(x) {\nreturn(.1)\n}\n", "define f(x,y) {\nreturn(x+y)\n}\n",
        "define f(a[]) {\nreturn(a[0])\n}\n", "define f(x) {\nx\nreturn(x)\n}\n",
        "define f(x) {\nreturn(x)\ng=1\n}\n"}) {
    INFO(definition);
    CHECK_FALSE(lower(parsed(definition)));
  }
}

TEST_CASE("Source lowering bounds hostile offsets before signed index addition", "[bc][word]")
{
  Instructions code{I(O::pop_param),           I(O::variable, Variable('x')),
                    I(O::store, D(-1), D(-2)), I(O::pop_param_mark),
                    I(O::branch, D(0)),        I(O::number, "1"),
                    I(O::return_, D(-1))};
  for (D offset : {std::numeric_limits<D>::min(), std::numeric_limits<D>::max(), D(-5), D(9)}) {
    code[4].op1(offset);
    CHECK_FALSE(lower_word_function(code, VariableMask("x", ""), Letter('f')));
  }
}
