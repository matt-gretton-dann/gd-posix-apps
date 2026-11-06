/** Contracts for CFG-aware dead-result coloring, independent of VM activation.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <catch2/catch.hpp>

#include <cstddef>
#include <cstdint>
#include <limits>
#include <random>
#include <set>
#include <utility>
#include <variant>
#include <vector>

#include "live-result-plan.hh"
#include "verification.hh"

namespace {
using GD::Bc::Instructions;
using GD::Bc::Details::LivenessBudget;
using GD::Bc::Details::ResultSlotPlan;
using I = GD::Bc::Instruction;
using O = I::Opcode;
using D = I::Offset;

auto dense(unsigned terms = 8) -> Instructions
{
  Instructions code{I(O::number, "7")};
  for (unsigned i = 0; i < terms; ++i) {
    code.emplace_back(O::number, "1");
    code.emplace_back(O::add, D(-2), D(-1));
  }
  code.emplace_back(O::print, D(-1), I::Stream::output);
  return code;
}

auto old_plan(Instructions const& code) -> ResultSlotPlan
{
  return GD::Bc::Details::plan_result_slots(code, 48,
                                            GD::Bc::Details::plan_consumption(code, true));
}

auto optimize(Instructions const& code, bool persistent = true, bool body = false,
              LivenessBudget budget = {}) -> ResultSlotPlan
{
  REQUIRE_FALSE(GD::Bc::Details::verify_control_flow(code, body).has_value());
  return GD::Bc::Details::plan_live_result_slots(code, old_plan(code), 48, persistent, budget);
}

// An independent bounded path oracle tracks versions in original logical
// registers and in proposed physical slots. It performs no liveness analysis
// or coloring, and never uses the planner's result classifier. Conditional
// edges deliberately explore both outcomes rather than trusting fixture values.
void check_paths(Instructions const& code, ResultSlotPlan const& slots)
{
  for (I::Index i = 0; i < code.size(); ++i)
    REQUIRE(slots.index(i) < slots.storage_size(code.size()));
  for (unsigned seed = 0; seed < 16; ++seed) {
    std::minstd_rand random(seed + 661);
    std::vector<std::uint64_t> logical(code.size());
    std::vector<std::uint64_t> physical(slots.storage_size(code.size()));
    std::uint64_t version = 0;
    I::Index pc = 0;
    for (std::size_t step = 0; step < code.size() * 8 + 64 && pc < code.size(); ++step) {
      auto target = [&](I::Operand const& operand) {
        return pc + static_cast<I::Index>(std::get<D>(operand));
      };
      auto read = [&](I::Operand const& operand) {
        auto const producer = target(operand);
        REQUIRE(producer < code.size());
        REQUIRE(logical[producer] != 0);
        CHECK(physical[slots.index(producer)] == logical[producer]);
      };
      bool produces = false;
      auto next = pc + 1;
      switch (code[pc].opcode()) {
      case O::number:
      case O::variable:
        produces = true;
        break;
      case O::load:
        read(code[pc].op1());
        produces = true;
        break;
      case O::store:
        read(code[pc].op1());
        read(code[pc].op2());
        break;
      case O::add:
        read(code[pc].op1());
        read(code[pc].op2());
        produces = true;
        break;
      case O::print:
        read(code[pc].op1());
        break;
      case O::branch:
        next = target(code[pc].op1());
        break;
      case O::branch_zero:
        read(code[pc].op1());
        if (random() % 2 != 0)
          next = target(code[pc].op2());
        break;
      case O::function_begin:
        while (code[next].opcode() != O::function_end)
          ++next;
        ++next;
        break;
      case O::pop_param_mark:
        break;
      case O::return_:
        read(code[pc].op1());
        next = code.size();
        break;
      case O::eof:
        next = code.size();
        break;
      default:
        FAIL("path oracle lacks a fixture opcode");
      }
      if (produces) {
        logical[pc] = ++version;
        physical[slots.index(pc)] = version;
      }
      pc = next;
    }
  }
}

using GD::Bc::Details::LiveResultPlans;
using GD::Bc::Details::TypedResultCosts;
using GD::Bc::Details::TypedResultPlan;

constexpr TypedResultCosts lane_costs{16, 8, 48, 0, 0, 16, 16};

auto fixture_variable(char name) -> GD::Bc::Variable
{
  auto const letter = GD::Bc::Letter(name);
  return GD::Bc::Variable(letter);
}

auto layout_fallback(Instructions const& code, TypedResultCosts costs = lane_costs)
  -> LiveResultPlans
{
  auto generic = old_plan(code);
  auto typed = GD::Bc::Details::plan_typed_results(code, generic, costs, true);
  return {std::move(generic), std::move(typed)};
}

auto optimize_layouts(Instructions const& code, bool body = false,
                      TypedResultCosts costs = lane_costs, LivenessBudget budget = {})
  -> LiveResultPlans
{
  REQUIRE_FALSE(GD::Bc::Details::verify_control_flow(code, body).has_value());
  return GD::Bc::Details::plan_live_result_layouts(code, layout_fallback(code, costs), costs, true,
                                                   budget);
}

// Independent fixture schemas flatten separate lanes only for the path oracle.
// Equal numeric/other indices denote disjoint storage, not an alias. Skipped
// bodies retain no_register; unreachable producers still need valid indices
// because the existing typed lowerer checks every instruction in the region.
void check_typed_paths(Instructions const& code, TypedResultPlan const& typed)
{
  REQUIRE_FALSE(typed.empty());
  REQUIRE(typed.indices.size() == code.size());
  ResultSlotPlan flattened;
  flattened.indices.resize(code.size(), 0);
  flattened.count = typed.number_count + typed.other_count + 1;
  bool body = false;
  for (I::Index i = 0; i < code.size(); ++i) {
    auto const opcode = code[i].opcode();
    if (opcode == O::function_begin) {
      body = true;
      CHECK(typed.indices[i] == TypedResultPlan::no_register);
    }
    else if (opcode == O::function_end) {
      body = false;
      CHECK(typed.indices[i] == TypedResultPlan::no_register);
    }
    else if (body) {
      CHECK(typed.indices[i] == TypedResultPlan::no_register);
    }
    else if (opcode == O::number || opcode == O::add || opcode == O::load) {
      REQUIRE(typed.indices[i] < typed.number_count);
      flattened.indices[i] = typed.indices[i] + 1;
    }
    else if (opcode == O::variable) {
      REQUIRE(typed.indices[i] < typed.other_count);
      flattened.indices[i] =
        static_cast<ResultSlotPlan::CompactIndex>(typed.number_count + typed.indices[i] + 1);
    }
    else {
      CHECK(typed.indices[i] == TypedResultPlan::no_register);
    }
  }
  check_paths(code, flattened);
  CHECK_FALSE(GD::Bc::Details::lower_typed_executable(code, typed, true).empty());
}
}  // namespace

TEST_CASE("Typed liveness colors numeric values without an empty numeric sentinel",
          "[bc][vm][live-results][live-typed]")
{
  auto const code = dense(80);
  auto const plans = optimize_layouts(code);
  REQUIRE(plans.generic.storage_size(code.size()) <= 4);
  REQUIRE(plans.typed.number_count <= 3);
  CHECK(plans.typed.other_count == 0);
  check_paths(code, plans.generic);
  check_typed_paths(code, plans.typed);
}

TEST_CASE("Numeric and location colors use compatible independent lanes",
          "[bc][vm][live-results][live-typed]")
{
  Instructions code;
  for (unsigned group = 0; group < 12; ++group) {
    code.emplace_back(O::variable, fixture_variable('x'));
    code.emplace_back(O::load, D(-1));
    code.emplace_back(O::number, "1");
    code.emplace_back(O::add, D(-2), D(-1));
    code.emplace_back(O::print, D(-1), I::Stream::output);
  }
  auto const plans = optimize_layouts(code);
  REQUIRE(plans.typed.number_count <= 3);
  REQUIRE(plans.typed.other_count == 1);
  check_paths(code, plans.generic);
  check_typed_paths(code, plans.typed);
}

TEST_CASE("Unreachable typed producers retain valid lowerer indices",
          "[bc][vm][live-results][live-typed]")
{
  Instructions const code{I(O::pop_param_mark),    I(O::number, "7"),
                          I(O::return_, D(-1)),    I(O::variable, fixture_variable('x')),
                          I(O::load, D(-1)),       I(O::number, "1"),
                          I(O::add, D(-2), D(-1)), I(O::print, D(-1), I::Stream::output)};
  auto const plans = optimize_layouts(code, true);
  REQUIRE(plans.typed.number_count == 1);
  REQUIRE(plans.typed.other_count == 1);
  check_typed_paths(code, plans.typed);
}

TEST_CASE("Typed coloring excludes skipped definition bodies", "[bc][vm][live-results][live-typed]")
{
  Instructions code(8, I(O::number, "99"));
  auto const begin = code.size();
  code.emplace_back(O::function_begin, GD::Bc::VariableMask(), GD::Bc::Location("typed", 1, 1));
  code.emplace_back(O::pop_param_mark);
  auto body = dense();
  body.back() = I(O::return_, D(-1));
  code.insert(code.end(), body.begin(), body.end());
  auto const end = code.size();
  code.emplace_back(O::function_end, GD::Bc::Letter('f'), -static_cast<D>(end - begin));
  code.emplace_back(O::number, "9");
  code.emplace_back(O::print, D(-1), I::Stream::output);
  code.emplace_back(O::eof);
  auto const plans = optimize_layouts(code);
  REQUIRE(plans.typed.number_count == 1);
  CHECK(plans.typed.other_count == 0);
  check_typed_paths(code, plans.typed);
}

TEST_CASE("Layout budget failure preserves both complete original candidates",
          "[bc][vm][live-results][live-typed]")
{
  auto const code = dense(80);
  auto const fallback = layout_fallback(code);
  auto const result = optimize_layouts(code, false, lane_costs, LivenessBudget{4096, 20});
  CHECK(result.generic.indices == fallback.generic.indices);
  CHECK(result.generic.count == fallback.generic.count);
  CHECK(result.typed.indices == fallback.typed.indices);
  CHECK(result.typed.number_count == fallback.typed.number_count);
  CHECK(result.typed.other_count == fallback.typed.other_count);
}

TEST_CASE("Typed payload ties prefer generic storage and one saved byte permits typed",
          "[bc][vm][live-results][live-typed]")
{
  auto const code = dense();
  auto costs = lane_costs;
  // Four generic slots at 48 bytes versus three numeric slots at 16 bytes.
  // Maps and instruction widths are equal, leaving a 144-byte fixed-cost tie.
  costs.typed_fixed_bytes = 144;
  auto const equal = optimize_layouts(code, false, costs);
  CHECK(equal.generic.storage_size(code.size()) == 4);
  CHECK(equal.typed.empty());
  costs.typed_fixed_bytes = 143;
  auto const smaller = optimize_layouts(code, false, costs);
  REQUIRE(smaller.typed.number_count == 3);
  check_typed_paths(code, smaller.typed);
}

TEST_CASE("Invalid and overflowing layout costs preserve both original candidates",
          "[bc][vm][live-results][live-typed]")
{
  auto const code = dense();
  for (unsigned variant = 0; variant < 4; ++variant) {
    auto costs = lane_costs;
    if (variant == 0)
      costs.typed_fixed_bytes = std::numeric_limits<std::size_t>::max();
    else if (variant == 1)
      costs.number_slot_bytes = std::numeric_limits<std::size_t>::max();
    else if (variant == 2)
      costs.generic_fixed_bytes = std::numeric_limits<std::size_t>::max();
    else
      costs.other_slot_bytes = 0;
    auto const original = layout_fallback(code, costs);
    auto const result = optimize_layouts(code, false, costs);
    CHECK(result.generic.indices == original.generic.indices);
    CHECK(result.generic.count == original.generic.count);
    CHECK(result.typed.indices == original.typed.indices);
    CHECK(result.typed.number_count == original.typed.number_count);
    CHECK(result.typed.other_count == original.typed.other_count);
  }
}

TEST_CASE("Nonnumeric-only regions do not invent a numeric lane",
          "[bc][vm][live-results][live-typed]")
{
  Instructions const code{I(O::string, "hello"), I(O::print, D(-1), I::Stream::output)};
  auto const plans = optimize_layouts(code);
  CHECK(plans.typed.empty());
  CHECK(plans.typed.number_count == 0);
}

TEST_CASE("Live result colors reuse dense dead literals without destination aliases",
          "[bc][vm][live-results]")
{
  auto const code = dense();
  auto const slots = optimize(code);
  REQUIRE(slots.storage_size(code.size()) <= 4);  // Three values and empty sentinel.
  CHECK(slots.storage_size(code.size()) < old_plan(code).storage_size(code.size()));
  for (I::Index i = 2; i < code.size() - 1; i += 2) {
    CHECK(slots.index(i) != slots.index(i - 1));
    CHECK(slots.index(i) != slots.index(i - 2));
  }
  check_paths(code, slots);
  auto const repeated = optimize(code);
  CHECK(slots.indices == repeated.indices);
  CHECK(slots.count == repeated.count);
}

TEST_CASE("Liveness preserves outer values across a repeated loop consumer",
          "[bc][vm][live-results]")
{
  Instructions code(8, I(O::number, "99"));
  code.emplace_back(O::number, "7");        // 8, live across the backedge.
  code.emplace_back(O::number, "1");        // 9, also live across the backedge.
  code.emplace_back(O::add, D(-2), D(-1));  // 10, repeatedly reads 8 and 9.
  code.emplace_back(O::number, "9");        // 11, cannot overwrite either outer value.
  code.emplace_back(O::branch, D(-2));      // 12 -> 10.
  while (code.size() < 16)
    code.emplace_back(O::branch, D(1));
  auto const slots = optimize(code);
  REQUIRE(slots.storage_size(code.size()) <= 4);
  CHECK(slots.index(8) != slots.index(11));
  CHECK(slots.index(9) != slots.index(11));
  check_paths(code, slots);
  auto const layouts = optimize_layouts(code);
  REQUIRE(layouts.typed.number_count <= 3);
  check_typed_paths(code, layouts.typed);
}

TEST_CASE("Disjoint diamond branches share colors without sharing live operands",
          "[bc][vm][live-results]")
{
  Instructions code(8, I(O::number, "99"));
  Instructions const tail{I(O::number, "1"),
                          I(O::branch_zero, D(-1), D(6)),
                          I(O::number, "2"),
                          I(O::number, "3"),
                          I(O::add, D(-2), D(-1)),
                          I(O::print, D(-1), I::Stream::output),
                          I(O::branch, D(5)),
                          I(O::number, "4"),
                          I(O::number, "5"),
                          I(O::add, D(-2), D(-1)),
                          I(O::print, D(-1), I::Stream::output),
                          I(O::number, "9"),
                          I(O::print, D(-1), I::Stream::output),
                          I(O::eof)};
  code.insert(code.end(), tail.begin(), tail.end());
  auto const slots = optimize(code);
  REQUIRE(slots.storage_size(code.size()) <= 4);
  std::set<I::Index> const first{slots.index(10), slots.index(11), slots.index(12)};
  std::set<I::Index> const second{slots.index(15), slots.index(16), slots.index(17)};
  CHECK(first.size() == 3);
  CHECK(first == second);
  check_paths(code, slots);
  auto const layouts = optimize_layouts(code);
  REQUIRE(layouts.typed.number_count <= 3);
  check_typed_paths(code, layouts.typed);
}

TEST_CASE("Forward producer references follow execution order rather than source order",
          "[bc][vm][live-results]")
{
  Instructions code(8, I(O::number, "99"));
  code.emplace_back(O::branch, D(3));                    // 8 -> 11.
  code.emplace_back(O::print, D(2), I::Stream::output);  // 9 reads 11.
  code.emplace_back(O::number, "99");                    // 10 dies before 11 redefines.
  code.emplace_back(O::number, "7");                     // 11 dominates 9.
  code.emplace_back(O::branch, D(-3));                   // 12 -> 9.
  while (code.size() < 16)
    code.emplace_back(O::branch, D(1));
  auto const slots = optimize(code);
  REQUIRE(slots.storage_size(code.size()) <= 2);
  CHECK(slots.index(11) != 0);
  check_paths(code, slots);
  auto const layouts = optimize_layouts(code);
  REQUIRE(layouts.typed.number_count == 1);
  check_typed_paths(code, layouts.typed);
}

TEST_CASE("Repeated operands stay intact until both reads complete", "[bc][vm][live-results]")
{
  Instructions code(8, I(O::number, "99"));
  code.emplace_back(O::number, "7");
  code.emplace_back(O::add, D(-1), D(-1));
  code.emplace_back(O::print, D(-1), I::Stream::output);
  while (code.size() < 16)
    code.emplace_back(O::branch, D(1));
  auto const slots = optimize(code);
  REQUIRE(slots.storage_size(code.size()) <= 3);
  CHECK(slots.index(8) != slots.index(9));
  check_paths(code, slots);
}

TEST_CASE("Definition bodies get independent liveness regions", "[bc][vm][live-results]")
{
  Instructions code(8, I(O::number, "99"));
  auto const begin = code.size();
  code.emplace_back(O::function_begin, GD::Bc::VariableMask(), GD::Bc::Location("live", 1, 1));
  code.emplace_back(O::pop_param_mark);
  auto body = dense();
  body.back() = I(O::return_, D(-1));
  code.insert(code.end(), body.begin(), body.end());
  auto const end = code.size();
  code.emplace_back(O::function_end, GD::Bc::Letter('f'), -static_cast<D>(end - begin));
  code.emplace_back(O::number, "9");
  code.emplace_back(O::print, D(-1), I::Stream::output);
  code.emplace_back(O::eof);
  auto const slots = optimize(code);
  REQUIRE(slots.storage_size(code.size()) <= 2);
  for (auto i = begin + 1; i <= end; ++i)
    CHECK(slots.index(i) == 0);
  check_paths(code, slots);
  Instructions const region(code.begin() + static_cast<Instructions::difference_type>(begin + 1),
                            code.begin() + static_cast<Instructions::difference_type>(end));
  auto const local = optimize(region, true, true);
  CHECK(local.storage_size(region.size()) <= 4);
  check_paths(region, local);
}

TEST_CASE("Warm tiny regions can improve while transient streams retain their fallback",
          "[bc][vm][live-results]")
{
  auto const code = dense(4);
  REQUIRE(code.size() < 16);
  auto const warm = optimize(code);
  CHECK(warm.storage_size(code.size()) <= 4);
  auto const cold = optimize(code, false);
  auto const fallback = old_plan(code);
  CHECK(cold.indices == fallback.indices);
  CHECK(cold.count == fallback.count);
  check_paths(code, warm);
  check_paths(code, cold);
}

TEST_CASE("Declined liveness budgets preserve the complete original plan", "[bc][vm][live-results]")
{
  auto const code = dense();
  auto const fallback = old_plan(code);
  for (auto budget : {LivenessBudget{code.size() - 1, 1024}, LivenessBudget{4096, 0}}) {
    auto const slots = optimize(code, true, false, budget);
    CHECK(slots.indices == fallback.indices);
    CHECK(slots.count == fallback.count);
    check_paths(code, slots);
  }
  for (auto bytes : {std::size_t(0), std::numeric_limits<std::size_t>::max()}) {
    auto const slots = GD::Bc::Details::plan_live_result_slots(code, fallback, bytes, true);
    CHECK(slots.indices == fallback.indices);
    CHECK(slots.count == fallback.count);
  }
}

TEST_CASE("Liveness crosses bitset words and abandons partially spent work safely",
          "[bc][vm][live-results]")
{
  auto const code = dense(80);
  REQUIRE(code.size() > 128);
  auto const slots = optimize(code);
  REQUIRE(slots.storage_size(code.size()) <= 4);
  check_paths(code, slots);
  auto const fallback = old_plan(code);
  auto const exhausted = optimize(code, true, false, LivenessBudget{4096, 20});
  CHECK(exhausted.indices == fallback.indices);
  CHECK(exhausted.count == fallback.count);
  check_paths(code, exhausted);
  auto const retry = optimize(code);
  CHECK(retry.indices == slots.indices);
}

TEST_CASE("Hard analysis limits and unprofitable interference retain the old layout",
          "[bc][vm][live-results]")
{
  Instructions const oversized(4097, I(O::number, "1"));
  auto const large = optimize(oversized, true, false, LivenessBudget{8192, 4 * 1024 * 1024});
  CHECK(large.indices.empty());
  CHECK(large.storage_size(oversized.size()) == oversized.size());

  Instructions simultaneous(20, I(O::number, "1"));
  for (I::Index i = 0; i < 20; ++i)
    simultaneous.emplace_back(O::print, static_cast<D>(i) - static_cast<D>(simultaneous.size()),
                              I::Stream::output);
  auto const fallback = old_plan(simultaneous);
  auto const unchanged = optimize(simultaneous);
  CHECK(unchanged.indices == fallback.indices);
  CHECK(unchanged.count == fallback.count);
  check_paths(simultaneous, unchanged);
}

TEST_CASE("Generated diamonds preserve an outer owner across disjoint lifetimes",
          "[bc][vm][live-results]")
{
  for (unsigned seed = 0; seed < 8; ++seed) {
    std::minstd_rand random(seed + 661197);
    Instructions code{I(O::number, "777")};
    for (unsigned group = 0; group < 12; ++group) {
      for (unsigned n = 0, count = static_cast<unsigned>(random() % 4); n < count; ++n)
        code.emplace_back(O::number, "99");
      code.emplace_back(O::number, "1");
      auto const conditional = code.size();
      code.emplace_back(O::branch_zero, D(-1), D(1));
      for (unsigned arm = 0; arm < 2; ++arm) {
        auto const first = code.size();
        if (arm == 1) {
          code[conditional] = I(O::branch_zero, D(-1), static_cast<D>(first - conditional));
          code[first - 1] = I(O::branch, D(5));  // Skip four right-arm instructions.
        }
        code.emplace_back(O::number, "2");
        code.emplace_back(O::number, "3");
        auto const second_operand = random() % 2 == 0 ? D(-1) : -static_cast<D>(code.size());
        code.emplace_back(O::add, D(-2), second_operand);
        code.emplace_back(O::print, D(-1), I::Stream::output);
        if (arm == 0)
          code.emplace_back(O::branch, D(1));
      }
    }
    code.emplace_back(O::print, -static_cast<D>(code.size()), I::Stream::output);
    code.emplace_back(O::eof);
    REQUIRE(code.size() > 128);
    auto const slots = optimize(code);
    REQUIRE(slots.storage_size(code.size()) <= 5);
    check_paths(code, slots);
    auto const layouts = optimize_layouts(code);
    REQUIRE(layouts.typed.number_count <= 4);
    check_typed_paths(code, layouts.typed);
  }
}
