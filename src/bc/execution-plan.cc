/** \file execution-plan.cc
 * Verified operand lowering and basic-block arithmetic ownership.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "execution-plan.hh"

#include <cassert>
#include <initializer_list>
#include <limits>
#include <optional>
#include <utility>

namespace GD::Bc::Details {
namespace {
auto arithmetic_lhs(Instruction::Opcode opcode) -> bool
{
  using O = Instruction::Opcode;
  switch (opcode) {
  case O::negate:
  case O::add:
  case O::subtract:
  case O::multiply:
  case O::divide:
  case O::modulo:
  case O::power:
  case O::sqrt:
  case O::abs:
  case O::scale_expr:
  case O::less_than:
  case O::less_than_equals:
  case O::equals:
  case O::not_equals:
    return true;
  default:
    return false;
  }
}
// A definition registers its body without executing those instructions in the
// outer pack. Persistent function packs receive a separate, verified body.
// Visit end metadata too, so the position after a definition starts a block.
template<typename Fn>
void visit_execution_region(Instructions const& code, Fn&& visit)
{
  bool in_function = false;
  for (Instruction::Index i = 0; i < code.size(); ++i) {
    auto const opcode = code[i].opcode();
    if (opcode == Instruction::Opcode::function_begin) {
      in_function = true;
      visit(i);
    }
    else if (opcode == Instruction::Opcode::function_end) {
      in_function = false;
      visit(i);
    }
    else if (!in_function) {
      visit(i);
    }
  }
}
}  // namespace

auto classify_result(Instructions const& code, Instruction::Index index) -> ResultKind
{
  using O = Instruction::Opcode;
  auto target = [](Instruction::Index pc, Instruction::Operand const& operand) {
    return pc + static_cast<Instruction::Index>(std::get<Instruction::Offset>(operand));
  };
  switch (code[index].opcode()) {
  case O::string:
    return ResultKind::string;
  case O::variable:
  case O::array_element:
  case O::ibase:
  case O::obase:
  case O::scale:
    return ResultKind::scalar_location;
  case O::array:
    return ResultKind::array_location;
  case O::pop_param_array:
    return ResultKind::array_values;
  case O::load:
    return code[target(index, code[index].op1())].opcode() == O::array ? ResultKind::array_values
                                                                       : ResultKind::number;
  case O::number:
  case O::add:
  case O::subtract:
  case O::negate:
  case O::multiply:
  case O::divide:
  case O::modulo:
  case O::power:
  case O::scale_expr:
  case O::sqrt:
  case O::abs:
  case O::length:
  case O::equals:
  case O::less_than_equals:
  case O::not_equals:
  case O::less_than:
  case O::call:
  case O::pop_param:
    return ResultKind::number;
  case O::eof:
  case O::print:
  case O::quit:
  case O::store:
  case O::branch:
  case O::branch_zero:
  case O::return_:
  case O::push_param_mark:
  case O::pop_param_mark:
  case O::push_param:
  case O::function_begin:
  case O::function_end:
    return ResultKind::none;
  }
  return ResultKind::none;
}

auto plan_result_slots(Instructions const& code, std::size_t slot_bytes) -> ResultSlotPlan
{
  using Compact = ResultSlotPlan::CompactIndex;
  auto const size = code.size();
  // Avoid per-item map work on tiny CLI streams and preserve the full index
  // domain through the existing representation when narrowing cannot be proved.
  if (size < 16 || size > std::numeric_limits<Compact>::max() ||
      size > std::numeric_limits<std::size_t>::max() / sizeof(Compact) ||
      slot_bytes <= sizeof(Compact)) {
    return {};
  }
  std::size_t producers = 0;
  visit_execution_region(code, [&](Instruction::Index i) {
    if (classify_result(code, i) != ResultKind::none)
      ++producers;
  });
  // Include the shared empty sentinel and the persistent map in the payload
  // comparison. Division avoids overflowing a saved-slots * slot-bytes product.
  if (producers >= size - 1 || size - producers - 1 <= size * sizeof(Compact) / slot_bytes) {
    return {};
  }
  ResultSlotPlan result;
  result.indices.resize(size, 0);
  Compact next = 1;
  visit_execution_region(code, [&](Instruction::Index i) {
    if (classify_result(code, i) != ResultKind::none)
      result.indices[i] = next++;
  });
  result.count = next;
  return result;
}

namespace {
// Both layouts preserve source-PC control flow and the same sidecar ownership.
// Only the opcode and data/result register maps differ.
template<typename Code, typename Opcode, typename ResultIndex, typename DataIndex>
auto lower_records(Instructions const& code, typename Code::value_type empty, bool persistent,
                   Opcode opcode, ResultIndex result_index, DataIndex data_index) -> Code
{
  using I = Instruction::Index;
  using O = Instruction::Opcode;
  using C = std::uint32_t;
  auto const size = code.size();
  if (size == 0 || (!persistent && size < 16) || size >= std::numeric_limits<C>::max())
    return {};
  Code result(size, empty);
  bool representable = true;
  auto encode = [&](I pc, Instruction::Operand const& operand, bool control) {
    ExecutableOperand encoded;
    std::visit(
      Overloaded{
        [&](Instruction::Offset offset) {
          auto const index = pc + static_cast<I>(offset);
          encoded = ExecutableOperand(static_cast<C>(control ? index : data_index(index)));
        },
        [&](Instruction::Stream stream) { encoded = ExecutableOperand(static_cast<C>(stream)); },
        [&](Letter letter) { encoded = ExecutableOperand(letter); },
        [&](Variable variable) { encoded = ExecutableOperand(variable.get()); },
        [&](Array array) { encoded = ExecutableOperand(array.get()); },
        [&](unsigned value) {
          if (value > std::numeric_limits<C>::max())
            representable = false;
          encoded = ExecutableOperand(static_cast<C>(value));
        },
        // Strings, locations and masks stay owned by the source sidecar. These
        // alternatives are consulted only by their corresponding opcode helpers.
        [](auto const&) {}},
      operand);
    return encoded;
  };
  I definition = 0;
  visit_execution_region(code, [&](I i) {
    auto const& source = code[i];
    auto& lowered = result[i];
    auto const operation = opcode(i);
    if (!operation) {
      representable = false;
      return;
    }
    lowered.opcode = *operation;
    lowered.result = static_cast<C>(result_index(i));
    if (source.opcode() == O::function_begin) {
      definition = i;
    }
    else if (source.opcode() == O::function_end) {
      // Matching definition metadata was established by verification. Bodies
      // are not executed by this outer pack and receive independent lowering.
      result[definition].first = ExecutableOperand(static_cast<C>(i + 1));
    }
    else {
      if (source.has_op1())
        lowered.first = encode(i, source.op1(), source.opcode() == O::branch);
      if (source.has_op2())
        lowered.second = encode(i, source.op2(), source.opcode() == O::branch_zero);
    }
  });
  if (!representable)
    return {};
  return result;
}
}  // namespace

auto lower_executable(Instructions const& code, ResultSlotPlan const& slots, bool persistent)
  -> ExecutableCode
{
  return lower_records<ExecutableCode>(
    code, {Instruction::Opcode::eof, 0, {}, {}}, persistent,
    [&](Instruction::Index i) { return std::optional<Instruction::Opcode>(code[i].opcode()); },
    [&](Instruction::Index i) { return slots.index(i); },
    [&](Instruction::Index i) { return slots.index(i); });
}

namespace {
auto payload_bytes(std::size_t fixed,
                   std::initializer_list<std::pair<std::size_t, std::size_t>> payloads)
  -> std::optional<std::size_t>
{
  auto total = fixed;
  for (auto const& [count, width] : payloads) {
    if (width != 0 && count > (std::numeric_limits<std::size_t>::max() - total) / width)
      return std::nullopt;
    total += count * width;
  }
  return total;
}

auto typed_operation(Instructions const& code, Instruction::Index index)
  -> std::optional<TypedOpcode>
{
  using O = Instruction::Opcode;
  using T = TypedOpcode;
  auto first_kind = [&] {
    auto const offset = std::get<Instruction::Offset>(code[index].op1());
    return classify_result(code, index + static_cast<Instruction::Index>(offset));
  };
  switch (code[index].opcode()) {
  case O::eof:
    return T::eof;
  case O::quit:
    return T::quit;
  case O::string:
    return T::string;
  case O::number:
    return T::number;
  case O::variable:
    return T::variable;
  case O::array_element:
    return T::array_element;
  case O::array:
    return T::array;
  case O::scale:
    return T::scale;
  case O::ibase:
    return T::ibase;
  case O::obase:
    return T::obase;
  case O::add:
    return T::add;
  case O::subtract:
    return T::subtract;
  case O::negate:
    return T::negate;
  case O::multiply:
    return T::multiply;
  case O::divide:
    return T::divide;
  case O::modulo:
    return T::modulo;
  case O::power:
    return T::power;
  case O::scale_expr:
    return T::scale_expr;
  case O::sqrt:
    return T::sqrt;
  case O::abs:
    return T::abs;
  case O::equals:
    return T::equals;
  case O::less_than_equals:
    return T::less_than_equals;
  case O::not_equals:
    return T::not_equals;
  case O::less_than:
    return T::less_than;
  case O::branch:
    return T::branch;
  case O::branch_zero:
    return T::branch_zero;
  case O::return_:
    return T::return_;
  case O::call:
    return T::call;
  case O::push_param_mark:
    return T::push_param_mark;
  case O::pop_param_mark:
    return T::pop_param_mark;
  case O::pop_param:
    return T::pop_param;
  case O::pop_param_array:
    return T::pop_param_array;
  case O::function_begin:
    return T::function_begin;
  case O::function_end:
    return T::function_end;
  case O::print:
    return first_kind() == ResultKind::number ? T::print_number : T::print_other;
  case O::load:
    return classify_result(code, index) == ResultKind::number ? T::load_number : T::load_array;
  case O::store:
    return first_kind() == ResultKind::array_location ? T::store_array : T::store_number;
  case O::length:
    return first_kind() == ResultKind::number ? T::length_number : T::length_array;
  case O::push_param:
    return first_kind() == ResultKind::number ? T::push_number : T::push_array;
  }
  // New source opcodes must acquire a typed contract before being enabled.
  return std::nullopt;
}
}  // namespace

auto plan_typed_results(Instructions const& code, ResultSlotPlan const& generic,
                        TypedResultCosts costs, bool persistent) -> TypedResultPlan
{
  using C = TypedResultPlan::CompactIndex;
  auto const size = code.size();
  if (size == 0 || (!persistent && size < 16) || size >= TypedResultPlan::no_register ||
      (!generic.indices.empty() && generic.indices.size() != size) ||
      costs.number_slot_bytes == 0 || costs.other_slot_bytes == 0 ||
      costs.generic_slot_bytes == 0 || costs.generic_instruction_bytes == 0 ||
      costs.typed_instruction_bytes == 0)
    return {};

  TypedResultPlan result;
  visit_execution_region(code, [&](Instruction::Index i) {
    auto const kind = classify_result(code, i);
    if (kind == ResultKind::number)
      ++result.number_count;
    else if (kind != ResultKind::none)
      ++result.other_count;
  });
  if (result.number_count == 0)
    return {};

  auto const generic_bytes = payload_bytes(costs.generic_fixed_bytes,
                                           {{generic.storage_size(size), costs.generic_slot_bytes},
                                            {generic.indices.size(), sizeof(C)},
                                            {size, costs.generic_instruction_bytes}});
  auto const typed_bytes =
    payload_bytes(costs.typed_fixed_bytes, {{result.number_count, costs.number_slot_bytes},
                                            {result.other_count, costs.other_slot_bytes},
                                            {size, sizeof(C)},
                                            {size, costs.typed_instruction_bytes}});
  if (!generic_bytes || !typed_bytes || *typed_bytes >= *generic_bytes)
    return {};

  result.indices.resize(size, TypedResultPlan::no_register);
  C next_number = 0;
  C next_other = 0;
  visit_execution_region(code, [&](Instruction::Index i) {
    auto const kind = classify_result(code, i);
    if (kind == ResultKind::number)
      result.indices[i] = next_number++;
    else if (kind != ResultKind::none)
      result.indices[i] = next_other++;
  });
  return result;
}

auto lower_typed_executable(Instructions const& code, TypedResultPlan const& slots, bool persistent)
  -> TypedExecutableCode
{
  if (slots.empty() || slots.indices.size() != code.size() || slots.number_count == 0 ||
      slots.number_count >= TypedResultPlan::no_register ||
      slots.other_count >= TypedResultPlan::no_register)
    return {};
  return lower_records<TypedExecutableCode>(
    code, {TypedOpcode::eof, 0, {}, {}}, persistent,
    [&](Instruction::Index i) -> std::optional<TypedOpcode> {
      auto const kind = classify_result(code, i);
      auto const index = slots.indices[i];
      if (kind == ResultKind::none) {
        if (index != TypedResultPlan::no_register)
          return std::nullopt;
      }
      else if (index >= (kind == ResultKind::number ? slots.number_count : slots.other_count))
        return std::nullopt;
      return typed_operation(code, i);
    },
    [&](Instruction::Index i) {
      return slots.indices[i] == TypedResultPlan::no_register ? 0 : slots.indices[i];
    },
    [&](Instruction::Index i) { return slots.indices[i]; });
}

auto plan_consumption(Instructions const& code, bool persistent) -> ConsumptionPlan
{
  using I = Instruction::Index;
  using O = Instruction::Opcode;
  auto const size = code.size();
  // No per-item analysis/allocation for common short, once-only CLI inputs.
  if (size == 0 || (!persistent && size < 16))
    return {};
  bool arithmetic = false;
  visit_execution_region(code,
                         [&](I i) { arithmetic = arithmetic || arithmetic_lhs(code[i].opcode()); });
  if (!arithmetic)
    return {};

  std::vector<I> uses(size, 0);
  std::vector<bool> leader(size, false);
  std::vector<I> block(size, 0);
  leader[0] = true;
  auto target = [](I pc, Instruction::Operand const& operand) {
    // Range and signed-offset safety are established by entry verification.
    return pc + static_cast<I>(std::get<Instruction::Offset>(operand));
  };
  auto begin_block = [&](I index) {
    if (index < size)
      leader[index] = true;
  };
  visit_execution_region(code, [&](I i) {
    auto const opcode = code[i].opcode();
    if (opcode == O::branch) {
      begin_block(target(i, code[i].op1()));
      begin_block(i + 1);
    }
    else if (opcode == O::branch_zero) {
      begin_block(target(i, code[i].op2()));
      begin_block(i + 1);
    }
    else if (opcode == O::function_begin || opcode == O::function_end || opcode == O::return_ ||
             opcode == O::quit || opcode == O::eof) {
      begin_block(i + 1);
    }
    auto count = [&](Instruction::Operand const& operand) {
      if (std::holds_alternative<Instruction::Offset>(operand)) {
        auto const producer = target(i, operand);
        assert(producer < size);  // NOLINT: verified data operands name results.
        ++uses[producer];
      }
    };
    // Branch/definition offsets are control metadata, never data uses.
    if (opcode != O::branch && opcode != O::function_end) {
      if (code[i].has_op1())
        count(code[i].op1());
      if (code[i].has_op2() && opcode != O::branch_zero)
        count(code[i].op2());
    }
  });
  I current = 0;
  for (I i = 0; i < size; ++i) {
    if (leader[i])
      current = i;
    block[i] = current;
  }
  ConsumptionPlan result(size, 0);
  bool any = false;
  visit_execution_region(code, [&](I i) {
    if (!arithmetic_lhs(code[i].opcode()))
      return;
    auto const producer = target(i, code[i].op1());
    if (producer < i && uses[producer] == 1 && block[producer] == block[i]) {
      result[i] = 1;
      any = true;
    }
  });
  // Checked execution and plans with no proven transfer use the copy fallback.
  if (!any)
    return {};
  return result;
}
}  // namespace GD::Bc::Details
