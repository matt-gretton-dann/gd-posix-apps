/** Observable transaction boundaries for the opaque portable VM adapter.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <catch2/catch.hpp>

#include <array>
#include <atomic>
#include <memory>
#include <string>

#include "portable-scalar-vm.hh"

namespace {
using namespace GD::Bc;
using namespace GD::Bc::Details;

auto slot(char letter) -> unsigned { return static_cast<unsigned>(Variable(letter).get()); }

struct Poll
{
  mutable unsigned calls{};
  unsigned stop{};
};

auto interrupted(void const* context) noexcept -> bool
{
  auto const& poll = *static_cast<Poll const*>(context);
  return ++poll.calls == poll.stop;
}

auto code(std::string const& source) -> Instructions
{
  Parser parser(std::make_unique<Lexer>(std::make_unique<StringReader>(source)), false);
  auto result = parser.parse();
  REQUIRE(result);
  if (!result->empty() && result->back().opcode() == Instruction::Opcode::eof)
    result->pop_back();
  return *result;
}

struct State
{
  std::array<Number, Letter::count_> variables{};
  Poll poll;

  State() { variables[0] = Number(7); }
  auto context() -> PortableScalarVmContext { return {variables, 10, false, interrupted, &poll}; }
  void unchanged() const
  {
    for (unsigned index = 0; index < Letter::count_; ++index)
      CHECK(variables[index] == Number(index == 0 ? 7U : 0U));
  }
};
}  // namespace

TEST_CASE("Portable transaction interruption never publishes a subset of writes",
          "[bc][vm][portable-transaction]")
{
  auto const instructions = code("for(i=0;i<5;i++)s+=i\n");
  for (unsigned stop = 1; stop <= 5; ++stop) {
    State state;
    state.poll.stop = stop;
    INFO(stop);
    CHECK_FALSE(try_portable_scalar(instructions, state.context()));
    CHECK(state.poll.calls == stop);
    state.unchanged();
    // The same state and owned code remain usable after every declined boundary.
    state.poll = {};
    REQUIRE(try_portable_scalar(instructions, state.context()));
    CHECK(state.poll.calls == 5);
    CHECK(state.variables[slot('i')] == Number(5));
    CHECK(state.variables[slot('s')] == Number(10));
    CHECK(state.variables[0] == Number(7));
  }
}

TEST_CASE("Portable transaction retains full checks base and initial-scale guards",
          "[bc][vm][portable-transaction]")
{
  auto const instructions = code("for(i=0;i<5;i++)s+=i\n");
  State state;
  auto context = state.context();
  context.full_checks = true;
  CHECK_FALSE(try_portable_scalar(instructions, context));
  CHECK(state.poll.calls == 0);
  state.unchanged();
  context.full_checks = false;
  context.input_base = 2;
  CHECK_FALSE(try_portable_scalar(instructions, context));
  CHECK(state.poll.calls == 0);
  state.unchanged();
  context.input_base = 10;
  state.variables[slot('s')] = Number("0.0", 10);
  CHECK_FALSE(try_portable_scalar(instructions, context));
  CHECK(state.variables[slot('s')].scale() == 1);
  CHECK(state.variables[slot('i')] == Number(0));
  state.variables[slot('s')] = Number(7);
  CHECK_FALSE(try_portable_scalar(instructions, context));
  CHECK(state.variables[slot('s')] == Number(7));
  CHECK(state.variables[slot('i')] == Number(0));
}

TEST_CASE("Portable transaction overflow and unsupported output retain original state",
          "[bc][vm][portable-transaction]")
{
  State state;
  auto const overflow = code("for(i=0;i<2;i++)s+=18446744073709551615\n");
  CHECK_FALSE(try_portable_scalar(overflow, state.context()));
  state.unchanged();
  auto const output = code("for(i=0;i<2;i++){s+=i;i;}\n");
  CHECK_FALSE(try_portable_scalar(output, state.context()));
  state.unchanged();
}

TEST_CASE("Portable transaction uses its explicit atomic interrupt context",
          "[bc][vm][portable-transaction]")
{
  State state;
  std::atomic_flag flag = ATOMIC_FLAG_INIT;
  auto context = state.context();
  context.interrupted = portable_scalar_flag_interrupted;
  context.interrupt_context = &flag;
  (void)flag.test_and_set();
  auto const instructions = code("for(i=0;i<5;i++)s+=i\n");
  CHECK_FALSE(try_portable_scalar(instructions, context));
  state.unchanged();
  flag.clear();
  REQUIRE(try_portable_scalar(instructions, context));
  CHECK(state.variables[slot('s')] == Number(10));
  CHECK(state.variables[slot('i')] == Number(5));
  // A caller may explicitly omit interruption, without hidden global/TLS state.
  state.variables[slot('s')] = Number(0);
  state.variables[slot('i')] = Number(0);
  context.interrupted = nullptr;
  context.interrupt_context = nullptr;
  REQUIRE(try_portable_scalar(instructions, context));
  CHECK(state.variables[slot('s')] == Number(10));
}
