/** \file test-execution-plan.cc
 * Verified lowering, result-lane costs and arithmetic ownership obligations.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <catch2/catch.hpp>

#include <array>
#include <limits>

#include "execution-plan.hh"
#include "verification.hh"

namespace {
using I = GD::Bc::Instruction;
using O = I::Opcode;
using D = I::Offset;
auto plan(GD::Bc::Instructions const& code, bool persistent = true)
  -> GD::Bc::Details::ConsumptionPlan
{
  REQUIRE_FALSE(GD::Bc::Details::verify_control_flow(code, false).has_value());
  return GD::Bc::Details::plan_consumption(code, persistent);
}
}  // namespace

TEST_CASE("Consumption plans skip empty and tiny once-only streams", "[bc][vm][execution-plan]")
{
  GD::Bc::Instructions const code{I(O::number, "7"), I(O::number, "1"), I(O::add, D(-2), D(-1))};
  CHECK(GD::Bc::Details::plan_consumption({}, false).empty());
  CHECK(plan(code, false).empty());
  auto const persistent = plan(code);
  REQUIRE(persistent.size() == code.size());
  CHECK(persistent[2] == 1);
}

TEST_CASE("Repeated operands keep their producer available", "[bc][vm][execution-plan]")
{
  CHECK(plan({I(O::number, "7"), I(O::add, D(-1), D(-1))}).empty());
}

TEST_CASE("A backedge to the consumer prevents consuming an outer producer",
          "[bc][vm][execution-plan]")
{
  // 2 is reached again without re-executing 0 or 1. The sole static read
  // therefore cannot transfer 0, despite entry dominance.
  CHECK(plan({I(O::number, "7"), I(O::number, "1"), I(O::add, D(-2), D(-1)), I(O::branch, D(-1))})
          .empty());
}

TEST_CASE("A backedge that re-executes the producer permits consumption",
          "[bc][vm][execution-plan]")
{
  GD::Bc::Instructions const code{I(O::number, "7"), I(O::number, "1"), I(O::add, D(-2), D(-1)),
                                  I(O::branch, D(-3))};
  auto const flags = plan(code);
  REQUIRE(flags.size() == code.size());
  CHECK(flags[2] == 1);
  // The backward branch names a control target, not a second data use of 0.
}

TEST_CASE("Conditional entries divide consumption blocks", "[bc][vm][execution-plan]")
{
  CHECK(plan({I(O::number, "7"), I(O::number, "0"), I(O::branch_zero, D(-1), D(2)),
              I(O::number, "1"), I(O::number, "1"), I(O::add, D(-5), D(-1))})
          .empty());
}

TEST_CASE("A second later use prevents an arithmetic transfer", "[bc][vm][execution-plan]")
{
  GD::Bc::Instructions const code{I(O::number, "7"), I(O::number, "1"), I(O::add, D(-2), D(-1)),
                                  I(O::print, D(-3), I::Stream::output)};
  CHECK(plan(code).empty());
}

TEST_CASE("Outer definition registration does not prepare unexecuted arithmetic",
          "[bc][vm][execution-plan]")
{
  GD::Bc::Instructions code{
    I(O::function_begin, GD::Bc::VariableMask(), GD::Bc::Location("unused", 1, 1)),
    I(O::pop_param_mark), I(O::number, "7")};
  for (unsigned n = 0; n < 6; ++n) {
    code.emplace_back(O::number, "1");
    code.emplace_back(O::add, D(-2), D(-1));
  }
  code.emplace_back(O::return_, D(-1));
  auto const end = code.size();
  code.emplace_back(O::function_end, GD::Bc::Letter('f'), -static_cast<D>(end));
  code.emplace_back(O::eof);
  REQUIRE(code.size() >= 16);
  CHECK(plan(code, false).empty());
  CHECK(plan(code, true).empty());  // Caching never executes definition bodies.
  GD::Bc::Instructions const body(
    code.begin() + 1, code.begin() + static_cast<GD::Bc::Instructions::difference_type>(end));
  REQUIRE_FALSE(GD::Bc::Details::verify_control_flow(body, true).has_value());
  CHECK_FALSE(GD::Bc::Details::plan_consumption(body, true).empty());
}

TEST_CASE("Result-slot identity retains the full instruction-index domain",
          "[bc][vm][result-slots]")
{
  GD::Bc::Details::ResultSlotPlan const identity;
  auto const maximum = std::numeric_limits<I::Index>::max();
  CHECK(identity.index(maximum) == maximum);
  CHECK(identity.storage_size(maximum) == maximum);
  CHECK(GD::Bc::Details::plan_result_slots({}, 48).indices.empty());
  CHECK(GD::Bc::Details::plan_result_slots({I(O::number, "1")}, 48).indices.empty());
}

TEST_CASE("Result-slot planning charges the map and empty sentinel", "[bc][vm][result-slots]")
{
  GD::Bc::Instructions code;
  for (unsigned i = 0; i < 8; ++i)
    code.emplace_back(O::number, "1");
  for (unsigned i = 0; i < 8; ++i)
    code.emplace_back(O::print, D(-8), I::Stream::output);
  REQUIRE_FALSE(GD::Bc::Details::verify_control_flow(code, false).has_value());
  // Eight producers plus the sentinel and 64 map bytes lose at 8-byte slots,
  // but save payload at 16-byte slots. No compiler-specific slot size is used.
  CHECK(GD::Bc::Details::plan_result_slots(code, 0).indices.empty());
  CHECK(GD::Bc::Details::plan_result_slots(code, 4).indices.empty());
  CHECK(GD::Bc::Details::plan_result_slots(code, 8).indices.empty());
  auto const compact = GD::Bc::Details::plan_result_slots(code, 16);
  REQUIRE(compact.indices.size() == code.size());
  CHECK(compact.storage_size(code.size()) == 9);
  for (I::Index i = 0; i < 8; ++i) {
    CHECK(compact.index(i) == i + 1);
    CHECK(compact.index(i + 8) == 0);
  }
}

TEST_CASE("All-producing code keeps identity instead of adding map overhead",
          "[bc][vm][result-slots]")
{
  GD::Bc::Instructions code(16, I(O::number, "1"));
  CHECK(GD::Bc::Details::plan_result_slots(code, 48).indices.empty());
}

TEST_CASE("Producer slots retain forward data references and backedges", "[bc][vm][result-slots]")
{
  // Entry skips the print, then its forward producer executes before the
  // backedge reaches it. Registers are not reused at joins or last uses.
  GD::Bc::Instructions code{I(O::branch, D(2)), I(O::print, D(1), I::Stream::output),
                            I(O::number, "7"), I(O::branch, D(-2))};
  while (code.size() < 16)
    code.emplace_back(O::branch, D(1));
  REQUIRE_FALSE(GD::Bc::Details::verify_control_flow(code, false).has_value());
  auto const compact = GD::Bc::Details::plan_result_slots(code, 48);
  REQUIRE(compact.indices.size() == code.size());
  CHECK(compact.storage_size(code.size()) == 2);
  for (I::Index i = 0; i < code.size(); ++i)
    CHECK(compact.index(i) == (i == 2 ? 1 : 0));
}

TEST_CASE("Outer result slots exclude every unexecuted definition-body producer",
          "[bc][vm][result-slots]")
{
  GD::Bc::Instructions code{
    I(O::function_begin, GD::Bc::VariableMask(), GD::Bc::Location("unused", 1, 1)),
    I(O::pop_param_mark), I(O::number, "7")};
  for (unsigned n = 0; n < 20; ++n) {
    code.emplace_back(O::number, "1");
    code.emplace_back(O::add, D(-2), D(-1));
  }
  code.emplace_back(O::return_, D(-1));
  auto const end = code.size();
  code.emplace_back(O::function_end, GD::Bc::Letter('f'), -static_cast<D>(end));
  code.emplace_back(O::number, "2");
  code.emplace_back(O::print, D(-1), I::Stream::output);
  code.emplace_back(O::eof);
  REQUIRE_FALSE(GD::Bc::Details::verify_control_flow(code, false).has_value());
  auto const outer = GD::Bc::Details::plan_result_slots(code, 48);
  REQUIRE(outer.indices.size() == code.size());
  CHECK(outer.storage_size(code.size()) == 2);
  for (I::Index i = 0; i <= end; ++i)
    CHECK(outer.index(i) == 0);
  CHECK(outer.index(end + 1) == 1);
  CHECK(outer.index(end + 2) == 0);

  GD::Bc::Instructions const body(
    code.begin() + 1, code.begin() + static_cast<GD::Bc::Instructions::difference_type>(end));
  REQUIRE_FALSE(GD::Bc::Details::verify_control_flow(body, true).has_value());
  // This body is almost entirely producers, so the map cannot repay its cost.
  CHECK(GD::Bc::Details::plan_result_slots(body, 48).indices.empty());
}

TEST_CASE("Compact operands encode register references and control targets separately",
          "[bc][vm][compact-executable]")
{
  GD::Bc::Instructions code{I(O::variable, GD::Bc::Variable('x')),
                            I(O::number, "7"),
                            I(O::store, D(-2), D(-1)),
                            I(O::load, D(-3)),
                            I(O::array, GD::Bc::Array('a')),
                            I(O::load, D(-1)),
                            I(O::push_param_mark),
                            I(O::push_param, D(-2)),
                            I(O::call, GD::Bc::Letter('f'), GD::Bc::Location("lower", 1, 1)),
                            I(O::length, D(-5)),
                            I(O::number, "2"),
                            I(O::array_element, GD::Bc::Array('a'), D(-1)),
                            I(O::store, D(-1), D(-9)),
                            I(O::print, D(-10), I::Stream::output),
                            I(O::number, "0"),
                            I(O::branch_zero, D(-1), D(2)),
                            I(O::branch, D(1)),
                            I(O::eof)};
  REQUIRE_FALSE(GD::Bc::Details::verify_control_flow(code, false).has_value());
  auto const slots = GD::Bc::Details::plan_result_slots(code, 48);
  auto const lowered = GD::Bc::Details::lower_executable(code, slots);
  REQUIRE(lowered.size() == code.size());
  CHECK(lowered[0].first.letter == 'x');
  CHECK(lowered[3].first.index == slots.index(0));
  CHECK(lowered[7].first.index == slots.index(5));
  CHECK(lowered[11].first.letter == 'a');
  CHECK(lowered[11].second.index == slots.index(10));
  CHECK(lowered[12].first.index == slots.index(11));
  CHECK(lowered[12].second.index == slots.index(3));
  CHECK(lowered[13].first.index == slots.index(3));
  CHECK(lowered[13].second.index == static_cast<unsigned>(I::Stream::output));
  CHECK(lowered[15].first.index == slots.index(14));
  CHECK(lowered[15].second.index == 17);
  CHECK(lowered[16].first.index == 17);
  for (I::Index i = 0; i < code.size(); ++i) {
    CHECK(lowered[i].opcode == code[i].opcode());
    CHECK(lowered[i].result == slots.index(i));
  }
}

TEST_CASE("Compact lowering keeps tiny transient streams wide but lowers persistent functions",
          "[bc][vm][compact-executable]")
{
  GD::Bc::Instructions const code{I(O::pop_param_mark), I(O::number, "7"), I(O::return_, D(-1))};
  REQUIRE_FALSE(GD::Bc::Details::verify_control_flow(code, true).has_value());
  CHECK(GD::Bc::Details::lower_executable({}, {}, true).empty());
  CHECK(GD::Bc::Details::lower_executable(code, {}).empty());
  auto const lowered = GD::Bc::Details::lower_executable(code, {}, true);
  REQUIRE(lowered.size() == code.size());
  CHECK(lowered[2].first.index == 1);
}

TEST_CASE("Compact definitions skip cold bodies and lower them under their own index space",
          "[bc][vm][compact-executable]")
{
  GD::Bc::Instructions code{
    I(O::function_begin, GD::Bc::VariableMask(), GD::Bc::Location("lower", 1, 1)),
    I(O::pop_param_mark), I(O::number, "7"), I(O::return_, D(-1)),
    I(O::function_end, GD::Bc::Letter('f'), D(-4))};
  while (code.size() < 16)
    code.emplace_back(O::number, "0");
  code.emplace_back(O::eof);
  REQUIRE_FALSE(GD::Bc::Details::verify_control_flow(code, false).has_value());
  auto const slots = GD::Bc::Details::plan_result_slots(code, 48);
  auto const lowered = GD::Bc::Details::lower_executable(code, slots);
  REQUIRE(lowered.size() == code.size());
  CHECK(lowered[0].first.index == 5);
  for (I::Index i = 1; i < 4; ++i)
    CHECK(lowered[i].opcode == O::eof);  // These outer entries cannot execute.
  GD::Bc::Instructions body(code.begin() + 1, code.begin() + 4);
  REQUIRE_FALSE(GD::Bc::Details::verify_control_flow(body, true).has_value());
  auto const function = GD::Bc::Details::lower_executable(body, {}, true);
  REQUIRE(function.size() == 3);
  CHECK(function[1].opcode == O::number);
  CHECK(function[2].first.index == 1);
}

namespace {
// Deliberately explicit layout costs make profitability boundaries portable;
// these fixture values are not a claim about every compiler's actual layout.
auto typed_costs() -> GD::Bc::Details::TypedResultCosts { return {40, 24, 48, 80, 144, 16, 16}; }
auto typed_plan(GD::Bc::Instructions const& code, bool persistent = true)
  -> GD::Bc::Details::TypedResultPlan
{
  REQUIRE_FALSE(GD::Bc::Details::verify_control_flow(code, false).has_value());
  auto const generic = GD::Bc::Details::plan_result_slots(code, 48);
  return GD::Bc::Details::plan_typed_results(code, generic, typed_costs(), persistent);
}
}  // namespace

TEST_CASE("Typed planning charges both register lanes maps and fixed layout costs",
          "[bc][vm][typed-results]")
{
  using namespace GD::Bc::Details;
  CHECK(plan_typed_results({}, {}, typed_costs()).empty());
  GD::Bc::Instructions code(16, I(O::number, "7"));
  // Identity generic payload and typed payload tie at sixteen producers.
  // A seventeenth repays the complete map and extra fixed layout cost.
  CHECK(typed_plan(code).empty());
  code.emplace_back(O::number, "8");
  auto const result = typed_plan(code);
  REQUIRE(result.indices.size() == code.size());
  CHECK(result.number_count == 17);
  CHECK(result.other_count == 0);
  for (I::Index i = 0; i < code.size(); ++i)
    CHECK(result.indices[i] == i);  // Register zero is usable, not a sentinel.

  auto costs = typed_costs();
  costs.typed_fixed_bytes = std::numeric_limits<std::size_t>::max();
  CHECK(plan_typed_results(code, {}, costs, true).empty());
  costs = typed_costs();
  costs.generic_slot_bytes = std::numeric_limits<std::size_t>::max();
  CHECK(plan_typed_results(code, {}, costs, true).empty());
  costs = typed_costs();
  costs.number_slot_bytes = 0;
  CHECK(plan_typed_results(code, {}, costs, true).empty());
  GD::Bc::Instructions strings(17, I(O::string, "hello"));
  CHECK(typed_plan(strings).empty());
}

TEST_CASE("Typed lowering separates Number results from locations arrays and strings",
          "[bc][vm][typed-results]")
{
  using namespace GD::Bc::Details;
  GD::Bc::Instructions code{I(O::variable, GD::Bc::Variable('x')),
                            I(O::number, "7"),
                            I(O::store, D(-2), D(-1)),
                            I(O::load, D(-3)),
                            I(O::array, GD::Bc::Array('a')),
                            I(O::load, D(-1)),
                            I(O::length, D(-2)),
                            I(O::number, "2"),
                            I(O::array_element, GD::Bc::Array('a'), D(-1)),
                            I(O::load, D(-1)),
                            I(O::print, D(-9), I::Stream::output),
                            I(O::print, D(-11), I::Stream::output),
                            I(O::push_param_mark),
                            I(O::push_param, D(-8)),
                            I(O::push_param, D(-11)),
                            I(O::call, GD::Bc::Letter('f'), GD::Bc::Location("typed", 1, 1)),
                            I(O::store, D(-12), D(-11)),
                            I(O::length, D(-14)),
                            I(O::string, "hello"),
                            I(O::print, D(-1), I::Stream::output),
                            I(O::number, "0"),
                            I(O::branch_zero, D(-1), D(2)),
                            I(O::branch, D(1)),
                            I(O::eof)};
  auto const slots = typed_plan(code);
  REQUIRE_FALSE(slots.empty());
  CHECK(slots.number_count == 8);
  CHECK(slots.other_count == 5);
  std::array<I::Index, 8> const numbers{1, 3, 6, 7, 9, 15, 17, 20};
  std::array<I::Index, 5> const others{0, 4, 5, 8, 18};
  for (std::size_t i = 0; i < numbers.size(); ++i)
    CHECK(slots.indices[numbers[i]] == i);
  for (std::size_t i = 0; i < others.size(); ++i)
    CHECK(slots.indices[others[i]] == i);
  CHECK(slots.indices[2] == TypedResultPlan::no_register);
  auto const lowered = lower_typed_executable(code, slots);
  REQUIRE(lowered.size() == code.size());
  CHECK(lowered[0].first.letter == 'x');
  CHECK(lowered[2].opcode == TypedOpcode::store_number);
  CHECK(lowered[2].first.index == 0);   // Location lane.
  CHECK(lowered[2].second.index == 0);  // Independent Number lane.
  CHECK(lowered[3].opcode == TypedOpcode::load_number);
  CHECK(lowered[5].opcode == TypedOpcode::load_array);
  CHECK(lowered[6].opcode == TypedOpcode::length_array);
  CHECK(lowered[8].second.index == 3);
  CHECK(lowered[10].opcode == TypedOpcode::print_number);
  CHECK(lowered[11].opcode == TypedOpcode::print_other);
  CHECK(lowered[13].opcode == TypedOpcode::push_array);
  CHECK(lowered[14].opcode == TypedOpcode::push_number);
  CHECK(lowered[15].first.letter == 'f');
  CHECK(lowered[16].opcode == TypedOpcode::store_array);
  CHECK(lowered[17].opcode == TypedOpcode::length_number);
  CHECK(lowered[19].opcode == TypedOpcode::print_other);
  CHECK(lowered[21].first.index == 7);
  CHECK(lowered[21].second.index == 23);  // Absolute PC, not a register.
  CHECK(lowered[22].first.index == 23);

  auto invalid = slots;
  invalid.indices[1] = static_cast<TypedResultPlan::CompactIndex>(invalid.number_count);
  CHECK(lower_typed_executable(code, invalid).empty());
  invalid = slots;
  invalid.indices[2] = 0;  // A non-producer cannot claim a numeric register.
  CHECK(lower_typed_executable(code, invalid).empty());
  invalid = slots;
  invalid.number_count = TypedResultPlan::no_register;
  CHECK(lower_typed_executable(code, invalid).empty());
  invalid = slots;
  invalid.indices.pop_back();
  CHECK(lower_typed_executable(code, invalid).empty());
}

TEST_CASE("Typed forward references retain source control flow and independent indices",
          "[bc][vm][typed-results]")
{
  using namespace GD::Bc::Details;
  GD::Bc::Instructions code{I(O::number, "0"),  I(O::number, "0"),
                            I(O::branch, D(2)), I(O::print, D(1), I::Stream::output),
                            I(O::number, "7"),  I(O::branch, D(-2))};
  while (code.size() < 16)
    code.emplace_back(O::branch, D(1));
  auto const slots = typed_plan(code);
  REQUIRE_FALSE(slots.empty());
  CHECK(slots.number_count == 3);
  CHECK(slots.other_count == 0);
  CHECK(slots.indices[4] == 2);
  auto const lowered = lower_typed_executable(code, slots);
  REQUIRE(lowered.size() == code.size());
  CHECK(lowered[2].first.index == 4);
  CHECK(lowered[3].first.index == 2);
  CHECK(lowered[5].first.index == 3);
}

TEST_CASE("Typed outer plans exclude cold bodies and prepare functions independently",
          "[bc][vm][typed-results]")
{
  using namespace GD::Bc::Details;
  GD::Bc::Instructions code{
    I(O::function_begin, GD::Bc::VariableMask(), GD::Bc::Location("typed", 1, 1)),
    I(O::pop_param_mark)};
  for (unsigned n = 0; n != 20; ++n)
    code.emplace_back(O::number, "7");
  code.emplace_back(O::return_, D(-1));
  auto const end = code.size();
  code.emplace_back(O::function_end, GD::Bc::Letter('f'), -static_cast<D>(end));
  for (unsigned n = 0; n != 3; ++n)
    code.emplace_back(O::number, "2");
  code.emplace_back(O::eof);
  auto const outer = typed_plan(code);
  REQUIRE_FALSE(outer.empty());
  CHECK(outer.number_count == 3);
  for (I::Index i = 0; i <= end; ++i)
    CHECK(outer.indices[i] == TypedResultPlan::no_register);
  auto const lowered = lower_typed_executable(code, outer);
  REQUIRE(lowered.size() == code.size());
  CHECK(lowered[0].first.index == end + 1);
  for (I::Index i = 1; i < end; ++i)
    CHECK(lowered[i].opcode == TypedOpcode::eof);

  GD::Bc::Instructions body(code.begin() + 1,
                            code.begin() + static_cast<GD::Bc::Instructions::difference_type>(end));
  REQUIRE_FALSE(verify_control_flow(body, true).has_value());
  auto const generic = plan_result_slots(body, 48);
  auto const function = plan_typed_results(body, generic, typed_costs(), true);
  REQUIRE_FALSE(function.empty());
  CHECK(function.number_count == 20);
  auto const function_code = lower_typed_executable(body, function, true);
  REQUIRE(function_code.size() == body.size());
  CHECK(function_code.back().first.index == 19);

  GD::Bc::Instructions const tiny{I(O::number, "7"), I(O::print, D(-1), I::Stream::output)};
  auto cheap = typed_costs();
  cheap.typed_fixed_bytes = 0;
  CHECK(plan_typed_results(tiny, {}, cheap, false).empty());
  auto const persistent = plan_typed_results(tiny, {}, cheap, true);
  REQUIRE_FALSE(persistent.empty());
  CHECK(lower_typed_executable(tiny, persistent, false).empty());
  CHECK(lower_typed_executable(tiny, persistent, true).size() == tiny.size());
}
