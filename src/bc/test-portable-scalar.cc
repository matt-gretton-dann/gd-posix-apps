/** Independent eligibility, integer bounds and rollback tests for scalar regions.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <catch2/catch.hpp>

#include <limits>
#include <memory>
#include <string>

#include "portable-scalar.hh"

namespace {
using I = GD::Bc::Instruction;
using O = I::Opcode;
using D = I::Offset;
using GD::Bc::Instructions;
using GD::Bc::Variable;
using GD::Bc::Details::plan_portable_scalar;

auto sum_loop(unsigned bound, std::optional<std::string> increment = {}) -> Instructions
{
  // Slots 6 and 11 load i independently. The condition's value remains live
  // through the increment; assignment stores themselves produce no value.
  return {I(O::variable, Variable('i')),
          I(O::number, "0"),
          I(O::store, D(-2), D(-1)),
          I(O::variable, Variable('s')),
          I(O::number, "0"),
          I(O::store, D(-2), D(-1)),
          I(O::load, D(-6)),
          I(O::number, std::to_string(bound)),
          I(O::less_than, D(-2), D(-1)),
          I(O::branch_zero, D(-1), D(9)),
          I(O::load, D(-7)),
          increment ? I(O::number, *increment) : I(O::load, D(-11)),
          I(O::add, D(-2), D(-1)),
          I(O::store, D(-10), D(-1)),
          I(O::number, "1"),
          I(O::add, D(-9), D(-1)),
          I(O::store, D(-16), D(-1)),
          I(O::branch, D(-11))};
}

auto parsed(std::string const& program) -> Instructions
{
  GD::Bc::Parser parser(
    std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(program)), false);
  auto instructions = parser.parse();
  REQUIRE(instructions);
  if (!instructions->empty() && instructions->back().opcode() == O::eof)
    instructions->pop_back();
  return *instructions;
}

unsigned polls = 0;
unsigned interrupt_at = 0;
auto interrupted() noexcept -> bool { return ++polls == interrupt_at; }
}  // namespace

TEST_CASE("Portable integer loops preserve exact sums and zero-trip state",
          "[bc][vm][portable-scalar]")
{
  for (unsigned bound = 0; bound < 150; ++bound) {
    auto const plan = plan_portable_scalar(sum_loop(bound));
    REQUIRE(plan);
    CHECK(plan->variables().count() == 2);
    auto const result = plan->run();
    REQUIRE(result);
    CHECK((*result)['i' - 'a'] == bound);
    CHECK((*result)['s' - 'a'] == (bound == 0 ? 0 : bound * (bound - 1) / 2));
    CHECK((*result)['z' - 'a'] == 0);
  }
}

TEST_CASE("Portable scalar plans own their lowering independently of source mutations",
          "[bc][vm][portable-scalar]")
{
  auto code = sum_loop(10);
  auto const plan = plan_portable_scalar(code);
  REQUIRE(plan);
  code[7].op1(std::string("99"));
  code.clear();
  auto const result = plan->run();
  REQUIRE(result);
  CHECK((*result)['s' - 'a'] == 45);
  CHECK((*result)['i' - 'a'] == 10);
}

TEST_CASE("Portable scalar addition admits uint64 maximum and declines overflow",
          "[bc][vm][portable-scalar]")
{
  auto const maximum = std::to_string(std::numeric_limits<std::uint64_t>::max());
  auto const once = plan_portable_scalar(sum_loop(1, maximum));
  REQUIRE(once);
  auto const exact = once->run();
  REQUIRE(exact);
  CHECK((*exact)['s' - 'a'] == std::numeric_limits<std::uint64_t>::max());
  auto const twice = plan_portable_scalar(sum_loop(2, maximum));
  REQUIRE(twice);
  CHECK_FALSE(twice->run());
  CHECK_FALSE(twice->run());  // No partial stores or persistent scratch escape.
  auto const half = std::numeric_limits<std::uint64_t>::max() / 2;
  auto const below = plan_portable_scalar(sum_loop(2, std::to_string(half)));
  REQUIRE(below);
  auto const bounded = below->run();
  REQUIRE(bounded);
  CHECK((*bounded)['s' - 'a'] == std::numeric_limits<std::uint64_t>::max() - 1);
  auto const above = plan_portable_scalar(sum_loop(2, std::to_string(half + 1)));
  REQUIRE(above);
  CHECK_FALSE(above->run());
  for (auto const* text :
       {"18446744073709551616", "000000000000000000000", "-1", "+1", "1.0", "A", ""})
    CHECK_FALSE(plan_portable_scalar(sum_loop(2, text)));
}

TEST_CASE("Portable scalar comparisons retain unsigned uint64 ordering",
          "[bc][vm][portable-scalar]")
{
  auto const maximum = std::numeric_limits<std::uint64_t>::max();
  auto code = sum_loop(1);
  code[1].op1(std::to_string(maximum - 1));
  code[7].op1(std::to_string(maximum));
  auto const plan = plan_portable_scalar(code);
  REQUIRE(plan);
  auto const result = plan->run();
  REQUIRE(result);
  CHECK((*result)['i' - 'a'] == maximum);
  CHECK((*result)['s' - 'a'] == maximum - 1);
  code[1].op1(std::to_string(maximum));
  code[7].op1(std::string("1"));
  auto const zero_trip = plan_portable_scalar(code);
  REQUIRE(zero_trip);
  auto const unchanged = zero_trip->run(nullptr, 0);
  REQUIRE(unchanged);
  CHECK((*unchanged)['i' - 'a'] == maximum);
  CHECK((*unchanged)['s' - 'a'] == 0);
}

TEST_CASE("Portable scalar fuel exhaustion never returns partial loop state",
          "[bc][vm][portable-scalar]")
{
  auto const plan = plan_portable_scalar(sum_loop(4));
  REQUIRE(plan);
  CHECK_FALSE(plan->run(nullptr, 0));
  CHECK_FALSE(plan->run(nullptr, 3));
  auto const result = plan->run(nullptr, 4);
  REQUIRE(result);
  CHECK((*result)['s' - 'a'] == 6);
  CHECK((*result)['i' - 'a'] == 4);
  auto const zero = plan_portable_scalar(sum_loop(0));
  REQUIRE(zero);
  CHECK(zero->run(nullptr, 0));
}

TEST_CASE("Portable scalar interruption polls entry backedges and final publication",
          "[bc][vm][portable-scalar]")
{
  for (unsigned stop : {1U, 2U, 3U}) {
    auto const plan = plan_portable_scalar(sum_loop(1024));
    REQUIRE(plan);
    polls = 0;
    interrupt_at = stop;
    CHECK_FALSE(plan->run(interrupted));
    CHECK(polls == stop);
    // A later uninterrupted attempt starts from fresh zero scratch state.
    auto const result = plan->run();
    REQUIRE(result);
    CHECK((*result)['s' - 'a'] == 523776);
  }
  polls = 0;
  interrupt_at = 99;
  auto const plan = plan_portable_scalar(sum_loop(1023));
  REQUIRE(plan);
  REQUIRE(plan->run(interrupted));
  CHECK(polls == 2);  // Entry and final check; no 1,024th backedge yet.
}

TEST_CASE("Portable scalar caller fuel cannot exceed the fixed backedge limit",
          "[bc][vm][portable-scalar]")
{
  Instructions code(16, I(O::number, "0"));
  code[0] = I(O::variable, Variable('i'));
  code[1] = I(O::branch, D(0));
  auto const plan = plan_portable_scalar(code);
  REQUIRE(plan);
  polls = 0;
  interrupt_at = 3908;
  CHECK_FALSE(plan->run(interrupted, std::numeric_limits<std::uint64_t>::max()));
  CHECK(polls == 3907);  // Entry + floor(4,000,000 / 1,024); fuel wins.
}

TEST_CASE("Portable scalar lowering preserves parser branch joins and nested loops",
          "[bc][vm][portable-scalar]")
{
  auto const conditional_code = parsed("for(i=0;i<9;i++) { if(i<4) s+=i; if(3<i) s+=2; }\n");
  INFO(conditional_code);
  auto const conditional = plan_portable_scalar(conditional_code);
  REQUIRE(conditional);
  auto const first = conditional->run();
  REQUIRE(first);
  CHECK((*first)['s' - 'a'] == 16);
  CHECK((*first)['i' - 'a'] == 9);
  auto const nested_code = parsed("for(i=0;i<4;i++) for(j=0;j<3;j++) s=s+i+j\n");
  INFO(nested_code);
  auto const nested = plan_portable_scalar(nested_code);
  REQUIRE(nested);
  auto const second = nested->run();
  REQUIRE(second);
  CHECK((*second)['s' - 'a'] == 30);
  CHECK((*second)['i' - 'a'] == 4);
  CHECK((*second)['j' - 'a'] == 3);
}

TEST_CASE("Portable scalar eligibility rejects unsupported streams and size boundaries",
          "[bc][vm][portable-scalar]")
{
  CHECK_FALSE(plan_portable_scalar({}));
  Instructions minimal(15, I(O::number, "0"));
  minimal[0] = I(O::variable, Variable('i'));
  minimal[1] = I(O::branch, D(0));
  CHECK_FALSE(plan_portable_scalar(minimal));
  minimal.emplace_back(O::number, "0");
  REQUIRE(plan_portable_scalar(minimal));
  auto code = sum_loop(2);
  code.resize(64, I(O::number, "0"));
  REQUIRE(plan_portable_scalar(code));
  code.emplace_back(O::number, "0");
  CHECK_FALSE(plan_portable_scalar(code));
  for (O unsupported : {O::subtract, O::multiply, O::divide, O::modulo, O::call, O::print, O::quit,
                        O::array, O::function_begin}) {
    code = sum_loop(2);
    switch (unsupported) {
    case O::call:
      code[12] = I(unsupported, GD::Bc::Letter('f'), GD::Bc::Location("test", 1, 1));
      break;
    case O::print:
      code[12] = I(unsupported, D(-2), I::Stream::output);
      break;
    case O::quit:
      code[12] = I(unsupported, 0U);
      break;
    case O::array:
      code[12] = I(unsupported, GD::Bc::Array('a'));
      break;
    case O::function_begin:
      code[12] = I(unsupported, GD::Bc::VariableMask(), GD::Bc::Location("test", 1, 1));
      break;
    default:
      code[12] = I(unsupported, D(-2), D(-1));
      break;
    }
    CHECK_FALSE(plan_portable_scalar(code));
  }
  code = sum_loop(2);
  code.back() = I(O::branch, D(1));
  CHECK_FALSE(plan_portable_scalar(code));  // No backedge.
}

TEST_CASE("Portable scalar lowering rejects malformed schemas offsets and producer kinds",
          "[bc][vm][portable-scalar]")
{
  auto code = sum_loop(2);
#ifdef NDEBUG
  // Instruction's own constructor assertions prevent these representations
  // in Debug. Release still has to decline them before the graph verifier.
  code[7] = I(O::number, I::Stream::output);
  CHECK_FALSE(plan_portable_scalar(code));
  code = sum_loop(2);
  code[0] = I(O::variable, GD::Bc::Letter('i'));
  CHECK_FALSE(plan_portable_scalar(code));
  code = sum_loop(2);
  code[7] = I(O::number);
  CHECK_FALSE(plan_portable_scalar(code));
  code = sum_loop(2);
  code[12] = I(O::add, D(-2));
  CHECK_FALSE(plan_portable_scalar(code));
#endif
  for (D offset : {std::numeric_limits<D>::min(), std::numeric_limits<D>::max(), D(-9), D(10)}) {
    code = sum_loop(2);
    code[8].op1(offset);
    CHECK_FALSE(plan_portable_scalar(code));
  }
  code = sum_loop(2);
  code[12].op1(D(6));  // Exit is a branch target, never a result producer.
  CHECK_FALSE(plan_portable_scalar(code));
  code = sum_loop(2);
  code[10].op1(D(-3));  // A number is not a scalar location.
  CHECK_FALSE(plan_portable_scalar(code));
  code = sum_loop(2);
  code[12].op1(D(1));  // Stores have no result.
  CHECK_FALSE(plan_portable_scalar(code));
  code = sum_loop(2);
  code[9].op2(D(-3));  // Conditional backward edges are outside this subset.
  CHECK_FALSE(plan_portable_scalar(code));
}

TEST_CASE("Portable scalar graph proof rejects skipped and previous-iteration producers",
          "[bc][vm][portable-scalar]")
{
  Instructions skipped{
    I(O::variable, Variable('s')), I(O::branch, D(2)),        I(O::number, "1"),
    I(O::add, D(-1), D(-1)),       I(O::store, D(-4), D(-1)), I(O::branch, D(-2))};
  skipped.resize(16, I(O::number, "0"));
  CHECK_FALSE(plan_portable_scalar(skipped));
  skipped[1] = I(O::branch, D(3));  // Enter store before its first producer.
  skipped[5] = I(O::branch, D(-3));
  CHECK_FALSE(plan_portable_scalar(skipped));
}

TEST_CASE("Portable scalar lowering checks every operand role before graph analysis",
          "[bc][vm][portable-scalar]")
{
  auto code = sum_loop(2);
  code[10].op1(D(-11));  // Load location before the stream.
  CHECK_FALSE(plan_portable_scalar(code));
  code = sum_loop(2);
  code[13].op2(D(5));  // Store source at the exit, never a value.
  CHECK_FALSE(plan_portable_scalar(code));
  code = sum_loop(2);
  code[12].op2(D(1));  // Invalid second arithmetic operand independently.
  CHECK_FALSE(plan_portable_scalar(code));
  code = sum_loop(2);
  code[17].op1(D(2));  // Unconditional destination beyond the exit.
  CHECK_FALSE(plan_portable_scalar(code));
  code = sum_loop(2);
  code[9].op1(D(4));  // Conditional source names a store with no result.
  CHECK_FALSE(plan_portable_scalar(code));
  code = sum_loop(2);
  code[9].op2(D(10));  // Conditional destination beyond the exit.
  CHECK_FALSE(plan_portable_scalar(code));
  Instructions no_variables(16, I(O::number, "0"));
  no_variables.back() = I(O::branch, D(-1));
  CHECK_FALSE(plan_portable_scalar(no_variables));
#ifdef NDEBUG
  // Letter's constructor asserts for this invalid character in Debug. Its
  // Release representation still must not reach a private scalar subscript.
  code = sum_loop(2);
  code[0] = I(O::variable, Variable('?'));
  CHECK_FALSE(plan_portable_scalar(code));
  code = sum_loop(2);
  code[10] = I(O::load, I::Stream::output);
  CHECK_FALSE(plan_portable_scalar(code));
#endif
}
