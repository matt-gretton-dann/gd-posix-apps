/** Independent semantic and guard tests for the portable integer tier.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <catch2/catch.hpp>

#include <algorithm>
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

auto run_small(IntegerFunction const& code, std::int64_t argument, IntegerFunctionScratch& scratch,
               bool (*check)() noexcept) -> std::optional<std::int64_t>
{
  auto const result = run_integer_function(code, argument, scratch, check);
  return result ? result->to_machine_integer() : std::nullopt;
}

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

auto parsed_function(std::string const& definition) -> IntegerFunction
{
  Parser parser(std::make_unique<Lexer>(std::make_unique<StringReader>(definition)), false);
  auto source = parser.parse();
  REQUIRE(source);
  std::ostringstream diagnostics;
  for (auto const& instruction : *source) {
    if (instruction.has_op1()) {
      if (auto const* text = std::get_if<std::string>(&instruction.op1()))
        diagnostics << *text << '\n';
    }
  }
  INFO(definition);
  INFO(diagnostics.str());
  auto const begin = std::find_if(source->begin(), source->end(), [](auto const& instruction) {
    return instruction.opcode() == O::function_begin;
  });
  REQUIRE(begin != source->end());
  auto const end = std::find_if(begin, source->end(), [](auto const& instruction) {
    return instruction.opcode() == O::function_end;
  });
  REQUIRE(end != source->end());
  auto result = plan_integer_function(Instructions(begin + 1, end),
                                      std::get<VariableMask>(begin->op1()), Letter('f'));
  REQUIRE(result);
  return std::move(*result);
}
}  // namespace

#if GD_BC_PROFILE
TEST_CASE("Pure integer diagnostics own tracked scratch and plan storage", "[bc][integer-function]")
{
  auto const before = Profile::snapshot();
  auto const frames = static_cast<std::size_t>(Profile::AllocationKind::frames);
  auto const plans = static_cast<std::size_t>(Profile::AllocationKind::execution_plans);
  {
    auto plan = plan_integer_function(factorial(), VariableMask("x", ""), Letter('f'));
    REQUIRE(plan);
    IntegerFunctionScratch scratch;
    REQUIRE(run_integer_function(*plan, 50, scratch, running));
    auto const live = Profile::snapshot();
    // This direct engine call has no generic VM frames to hide an untracked
    // scratch pool. Cleared contents still own the reusable vector capacity.
    CHECK(live.allocations[frames].allocated_bytes > before.allocations[frames].allocated_bytes);
    CHECK(live.allocations[frames].live_bytes > before.allocations[frames].live_bytes);
    CHECK(live.allocations[plans].allocated_bytes > before.allocations[plans].allocated_bytes);
    CHECK(scratch.values.empty());
    CHECK(scratch.spills.empty());
  }
  auto const after = Profile::snapshot();
  CHECK(after.allocations[frames].live_bytes == before.allocations[frames].live_bytes);
  CHECK(after.allocations[plans].live_bytes == before.allocations[plans].live_bytes);
}
#endif

TEST_CASE("Pure integer promoted arithmetic preserves caller snapshots", "[bc][integer-function]")
{
  Number const left("9223372036854775807", 10);
  Number const right(2);
  struct Example
  {
    O opcode;
    char const* expected;
  };
  for (auto const& example :
       {Example{O::add, "9223372036854775809"}, Example{O::subtract, "9223372036854775805"},
        Example{O::multiply, "18446744073709551614"}, Example{O::less_than, "0"},
        Example{O::less_than_equals, "0"}, Example{O::equals, "0"}, Example{O::not_equals, "1"}}) {
    auto const result = integer_number_binary(example.opcode, left, right);
    REQUIRE(result);
    CHECK(*result == Number(example.expected, 10));
    CHECK(left == Number("9223372036854775807", 10));
    CHECK(right == Number(2));
  }
  Number negative("9223372036854775808", 10);
  negative.negate();
  auto const snapshot = negative;
  Number factor(2);
  factor.negate();
  auto const result = integer_number_binary(O::multiply, negative, factor);
  REQUIRE(result);
  CHECK(*result == Number("18446744073709551616", 10));
  CHECK(negative == snapshot);
  CHECK_FALSE(integer_number_binary(O::divide, left, right));
  CHECK(left == Number("9223372036854775807", 10));
  CHECK(right == Number(2));
}

TEST_CASE("Pure integer opaque banks preserve handles across moves and guards",
          "[bc][integer-function]")
{
  IntegerSpillBank bank;
  using Cell = IntegerFunctionValue;
  Cell const operand{std::numeric_limits<std::int64_t>::max(), false};
  Cell value;
  REQUIRE(bank.empty());
  CHECK(bank.logical_bytes() == 0);
  REQUIRE(bank.binary(O::multiply, operand, operand, value, 1024 * 1024));
  REQUIRE(value.big);
  auto const expected = bank.number(value);
  auto const charge = bank.logical_bytes();
  CHECK(charge > 0);
  // A refused publication cannot disturb an existing cell or bank record.
  Cell unchanged = value;
  CHECK_FALSE(bank.binary(O::multiply, value, value, unchanged, 0));
  CHECK(unchanged.payload == value.payload);
  CHECK(unchanged.big);
  CHECK(bank.logical_bytes() == charge);
  CHECK(bank.number(value) == expected);
  CHECK_FALSE(bank.binary(O::divide, operand, operand, unchanged, 1024 * 1024));
  CHECK(bank.number(value) == expected);

  IntegerSpillBank moved(std::move(bank));
  CHECK(bank.empty());
  CHECK(bank.logical_bytes() == 0);
  CHECK(moved.number(value) == expected);
  bank = std::move(moved);
  CHECK(moved.empty());
  CHECK(moved.logical_bytes() == 0);
  CHECK(bank.number(value) == expected);
  Cell zero;
  REQUIRE(bank.binary(O::subtract, value, value, zero, 0));
  CHECK_FALSE(zero.big);
  CHECK(zero.payload == 0);
  bank.clear();
  CHECK(bank.empty());
  CHECK(bank.logical_bytes() > 0);  // Retained opaque header stays charged.
  CHECK(bank.logical_bytes() < charge);
  CHECK(bank.number(zero) == Number(0));
  REQUIRE(bank.binary(O::multiply, operand, operand, value, 1024 * 1024));
  CHECK(bank.number(value) == expected);
}

TEST_CASE("Pure integer bank self moves preserve live cells", "[bc][integer-function]")
{
  IntegerSpillBank bank;
  using Cell = IntegerFunctionValue;
  Cell const operand{std::numeric_limits<std::int64_t>::max(), false};
  Cell positive, negative;
  REQUIRE(bank.binary(O::multiply, operand, operand, positive, 1024 * 1024));
  REQUIRE(bank.negate(positive, negative, 1024 * 1024));
  auto const expected_positive = bank.number(positive);
  auto const expected_negative = bank.number(negative);
  auto const charge = bank.logical_bytes();
  CHECK_FALSE(bank.empty());

  // A cell borrows its bank's record. Moving through an alias of the same
  // owner must preserve both records and their logical scratch charge.
  auto& same_bank = bank;
  auto const* result = &(bank = std::move(same_bank));
  CHECK(result == &bank);
  CHECK_FALSE(bank.empty());
  CHECK(bank.logical_bytes() == charge);
  CHECK(bank.number(positive) == expected_positive);
  CHECK(bank.number(negative) == expected_negative);
  Cell zero;
  REQUIRE(bank.binary(O::add, positive, negative, zero, 0));
  CHECK_FALSE(zero.big);
  CHECK(zero.payload == 0);
}

TEST_CASE("Pure integer continuations retain exact large results", "[bc][integer-function]")
{
  auto const plan = plan_integer_function(factorial(), VariableMask("x", ""), Letter('f'));
  REQUIRE(plan);
  IntegerFunctionScratch scratch;
  auto result = run_integer_function(*plan, 50, scratch, running);
  REQUIRE(result);
  CHECK(*result == Number("30414093201713378043612608166064768844377641568960512000000000000", 10));
  auto const cancellation = parsed_function("define f(x) {\nauto y\ny=x*x\nreturn (y-y)\n}\n");
  result =
    run_integer_function(cancellation, std::numeric_limits<std::int64_t>::max(), scratch, running);
  REQUIRE(result);
  CHECK(result->to_machine_integer() == 0);
  auto const negation = parsed_function("define f(x) {\nreturn (-x)\n}\n");
  result =
    run_integer_function(negation, std::numeric_limits<std::int64_t>::min(), scratch, running);
  REQUIRE(result);
  CHECK(*result == Number("9223372036854775808", 10));
  auto const bounded =
    parsed_function("define f(x) {\nauto y\ny=9\nwhile(x>0) {\ny=y*y\nx=x-1\n}\nreturn (y)\n}\n");
  CHECK_FALSE(run_integer_function(bounded, 14, scratch, running));
  CHECK(scratch.spills.empty());
  CHECK(scratch.values.empty());
  CHECK(scratch.frames.empty());
  CHECK(scratch.arguments.empty());
  CHECK(run_small(*plan, 5, scratch, running) == 120);
  auto const aliases = parsed_function("define f(x) {\nauto y,z,k\ny=x*x\nz=y\ny=0\n"
                                       "for(k=0;k<9;k++) y=z*z\nreturn (z+z)\n}\n");
  result =
    run_integer_function(aliases, std::numeric_limits<std::int64_t>::max(), scratch, running);
  REQUIRE(result);
  CHECK(*result == Number("170141183460469231694793815568465002498", 10));
  auto const dead_records =
    parsed_function("define f(x) {\nauto a,b,k\na=9\na=a*a\na=a*a\na=a*a\na=a*a\na=a*a\n"
                    "for(k=0;k<x;k++) b=a*a\nreturn (b)\n}\n");
  // Every coefficient is small enough, but the immutable bank must also
  // charge overwritten records instead of growing with an unbounded loop.
  CHECK_FALSE(run_integer_function(dead_records, 8000, scratch, running));
  CHECK(scratch.spills.empty());
  CHECK(scratch.values.empty());
  CHECK(run_small(*plan, 5, scratch, running) == 120);
}

TEST_CASE("Pure integer large values preserve branch and negation semantics",
          "[bc][integer-function]")
{
  auto const min = std::numeric_limits<std::int64_t>::min();
  auto const max = std::numeric_limits<std::int64_t>::max();
  auto const conditional =
    parsed_function("define f(x) {\nauto y\ny=x*x\nif(y) return(-y)\nreturn(0)\n}\n");
  auto const double_negation =
    parsed_function("define f(x) {\nauto y\ny=x*x\ny= -y\nreturn(-y)\n}\n");
  IntegerFunctionScratch scratch;
  for (auto input : {min, min + 1, std::int64_t(-2), std::int64_t(-1), std::int64_t(0),
                     std::int64_t(1), std::int64_t(2), max - 1, max}) {
    auto const operand = Number::from_machine_integer(input);
    auto square = operand;
    square.multiply(operand, 0);
    auto negative = square;
    negative.negate();
    for (auto const& plan : {&conditional, &double_negation}) {
      auto const result = run_integer_function(*plan, input, scratch, running);
      REQUIRE(result);
      CHECK(*result == (plan == &conditional ? negative : square));
      CHECK(scratch.values.empty());
      CHECK(scratch.spills.empty());
      CHECK(scratch.frames.empty());
      CHECK(scratch.arguments.empty());
    }
  }
}

TEST_CASE("Pure integer preparation accepts its exact instruction limit", "[bc][integer-function]")
{
  auto source = prologue();
  // Unused numeric results are still valid verified instructions. Boundaries
  // include the prologue and return, not only executed arithmetic operations.
  while (source.size() != 1023)
    source.emplace_back(O::number, "1");
  source.emplace_back(O::return_, D(-1));
  auto const plan = plan_integer_function(source, VariableMask("x", ""), Letter('f'));
  REQUIRE(plan);
  CHECK(plan->code.size() == 1024);
  IntegerFunctionScratch scratch;
  CHECK(run_small(*plan, 0, scratch, running) == 1);
  CHECK(scratch.values.empty());
  CHECK(scratch.spills.empty());
  CHECK(scratch.frames.empty());
  CHECK(scratch.arguments.empty());
  source.insert(source.end() - 1, I(O::number, "1"));
  CHECK_FALSE(plan_integer_function(source, VariableMask("x", ""), Letter('f')));
}

TEST_CASE("Pure integer functions use bounded iterative self calls", "[bc][integer-function]")
{
  auto const plan = plan_integer_function(factorial(), VariableMask("x", ""), Letter('f'));
  REQUIRE(plan);
  IntegerFunctionScratch scratch;
  std::int64_t expected = 1;
  for (std::int64_t n = 0; n <= 20; ++n) {
    if (n != 0)
      expected *= n;
    CHECK(run_small(*plan, n, scratch, running) == expected);
  }
  CHECK_FALSE(run_small(*plan, 21, scratch, running));
  CHECK_FALSE(run_small(*plan, 100000, scratch, running));
  CHECK_FALSE(run_small(*plan, 5, scratch, interrupted));
  CHECK(scratch.spills.empty());
  CHECK(scratch.values.empty());
  CHECK(scratch.frames.empty());
  CHECK(scratch.arguments.empty());
  CHECK(run_small(*plan, 5, scratch, running) == 120);
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
  CHECK(run_small(*plan, 7, scratch, running) == 4);
  // An explicit load is a snapshot even if the underlying scalar changes.
  source.insert(source.begin() + 5, I(O::load, D(-1)));
  source[9] = I(O::add, D(-4), D(-2));
  plan = plan_integer_function(source, VariableMask("x", ""), Letter('f'));
  REQUIRE(plan);
  CHECK(run_small(*plan, 7, scratch, running) == 9);
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
    CHECK(run_small(*plan, 7, scratch, running) == (opcode == O::add        ? 9
                                                    : opcode == O::subtract ? 5
                                                                            : 14));
    CHECK(run_small(*plan, min, scratch, running).has_value() == (opcode == O::add));
    CHECK(run_small(*plan, max, scratch, running).has_value() == (opcode == O::subtract));
  }
}

TEST_CASE("Pure integer signed edges agree with generic arithmetic", "[bc][integer-function]")
{
  auto const min = std::numeric_limits<std::int64_t>::min();
  auto const max = std::numeric_limits<std::int64_t>::max();
  std::int64_t const inputs[]{min, min + 1, min / 2, -2, -1, 0, 1, 2, max / 2, max - 1, max};
  IntegerFunctionScratch scratch;
  for (auto opcode : {O::add, O::subtract, O::multiply, O::less_than, O::less_than_equals,
                      O::equals, O::not_equals}) {
    // Fixed signed digits probe all operand signs and overflow boundaries.
    // The last plan uses x twice, including INT64_MIN as the right operand.
    for (int rhs = -9; rhs <= 10; ++rhs) {
      auto source = prologue();
      source.emplace_back(O::variable, Variable('x'));
      if (rhs == 10) {
        source.emplace_back(O::variable, Variable('x'));
      }
      else {
        source.emplace_back(O::number, std::to_string(rhs < 0 ? -rhs : rhs));
        if (rhs < 0)
          source.emplace_back(O::negate, D(-1));
      }
      source.emplace_back(opcode, D(4) - static_cast<D>(source.size()), D(-1));
      source.emplace_back(O::return_, D(-1));
      auto const plan = plan_integer_function(source, VariableMask("x", ""), Letter('f'));
      REQUIRE(plan);
      for (auto const input : inputs) {
        auto const right = rhs == 10 ? input : std::int64_t(rhs);
        auto expected = Number::from_machine_integer(input);
        auto const operand = Number::from_machine_integer(right);
        switch (opcode) {
        case O::add:
          expected.add(operand);
          break;
        case O::subtract:
          expected.sub(operand);
          break;
        case O::multiply:
          expected.multiply(operand, 0);
          break;
        case O::less_than:
          expected = Number(input < right ? 1 : 0);
          break;
        case O::less_than_equals:
          expected = Number(input <= right ? 1 : 0);
          break;
        case O::equals:
          expected = Number(input == right ? 1 : 0);
          break;
        case O::not_equals:
          expected = Number(input != right ? 1 : 0);
          break;
        default:
          FAIL("unexpected test operation");
        }
#if GD_BC_PROFILE
        auto const promotions =
          static_cast<std::size_t>(Profile::Event::integer_function_promotions);
        auto const demotions = static_cast<std::size_t>(Profile::Event::integer_function_demotions);
        auto const before = Profile::snapshot().events;
#endif
        INFO("opcode=" << static_cast<int>(opcode) << " left=" << input << " right=" << right);
        auto const result = run_integer_function(*plan, input, scratch, running);
        REQUIRE(result);
        CHECK(*result == expected);
        CHECK(scratch.values.empty());
        CHECK(scratch.spills.empty());
        CHECK(scratch.frames.empty());
        CHECK(scratch.arguments.empty());
#if GD_BC_PROFILE
        // A value oracle alone would also pass through unnecessary Number work.
        auto const after = Profile::snapshot().events;
        CHECK(after[promotions] == before[promotions] + !expected.to_machine_integer());
        CHECK(after[demotions] == before[demotions]);
#endif
      }
    }
  }
}

TEST_CASE("Pure integer large comparisons cover every relation", "[bc][integer-function]")
{
  Number const values[]{Number("18446744073709551616", 10), Number("18446744073709551617", 10)};
  for (unsigned left = 0; left != 2; ++left) {
    for (unsigned right = 0; right != 2; ++right) {
      for (auto opcode : {O::less_than, O::less_than_equals, O::equals, O::not_equals}) {
        auto const expected = opcode == O::less_than          ? left < right
                              : opcode == O::less_than_equals ? left <= right
                              : opcode == O::equals           ? left == right
                                                              : left != right;
        auto const result = integer_number_binary(opcode, values[left], values[right]);
        REQUIRE(result);
        CHECK(*result == Number(expected ? 1 : 0));
      }
    }
  }
  CHECK(values[0] == Number("18446744073709551616", 10));
  CHECK(values[1] == Number("18446744073709551617", 10));
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
    if (program.find("f(x-1)") != std::string::npos) {
      auto const position = program.find("scale=2");
      // Factorial exceeds int64: repeated failed speculation must retain
      // exact results and allow a later small argument to use the tier again.
      program.insert(position, "f(50)\nf(50)\nf(5)\n");
    }
    auto run = [&](VM::ExecutionChecks checks) {
      Parser parser(std::make_unique<Lexer>(std::make_unique<StringReader>(program)), false);
      auto source = parser.parse();
      REQUIRE(source);
      auto const begin = std::find_if(source->begin(), source->end(), [](auto const& instruction) {
        return instruction.opcode() == O::function_begin;
      });
      REQUIRE(begin != source->end());
      auto const end = std::find_if(begin, source->end(), [](auto const& instruction) {
        return instruction.opcode() == O::function_end;
      });
      REQUIRE(end != source->end());
      // Prove real parser output reaches the tier, rather than allowing the
      // differential check to pass solely through unsupported-code fallback.
      REQUIRE(plan_integer_function(Instructions(begin + 1, end),
                                    std::get<VariableMask>(begin->op1()), Letter('f')));
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

TEST_CASE("Pure integer products cover every sign and zero", "[bc][integer-function]")
{
  IntegerFunctionScratch scratch;
  for (int b : {-9, -1, 0, 1, 9}) {
    auto source = prologue();
    source.emplace_back(O::variable, Variable('x'));
    source.emplace_back(O::number, std::to_string(b < 0 ? -b : b));
    if (b < 0)
      source.emplace_back(O::negate, D(-1));
    source.emplace_back(O::multiply, D(4) - static_cast<D>(source.size()), D(-1));
    source.emplace_back(O::return_, D(-1));
    auto const plan = plan_integer_function(source, VariableMask("x", ""), Letter('f'));
    REQUIRE(plan);
    for (std::int64_t a = -128; a <= 127; ++a)
      CHECK(run_small(*plan, a, scratch, running) == a * b);
    if (b == -1) {
      CHECK_FALSE(run_small(*plan, std::numeric_limits<std::int64_t>::min(), scratch, running));
      CHECK(run_small(*plan, std::numeric_limits<std::int64_t>::max(), scratch, running) ==
            -std::numeric_limits<std::int64_t>::max());
    }
  }
  auto source = prologue();
  source.emplace_back(O::variable, Variable('x'));
  source.emplace_back(O::negate, D(-1));
  source.emplace_back(O::return_, D(-1));
  auto const plan = plan_integer_function(source, VariableMask("x", ""), Letter('f'));
  REQUIRE(plan);
  CHECK_FALSE(run_small(*plan, std::numeric_limits<std::int64_t>::min(), scratch, running));
  CHECK(run_small(*plan, 0, scratch, running) == 0);
  CHECK(run_small(*plan, -1, scratch, running) == 1);
}
