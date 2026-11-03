/** \file execution-plan.cc
 * Verified operand lowering and basic-block arithmetic ownership.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "execution-plan.hh"

#include <cassert>
#include <cstdlib>
#include <limits>

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

auto executable_opcode(Instruction::Opcode opcode) -> ExecutableOpcode
{
  switch (opcode) {
  case Instruction::Opcode::eof:
    return ExecutableOpcode::eof;
  case Instruction::Opcode::print:
    return ExecutableOpcode::print;
  case Instruction::Opcode::quit:
    return ExecutableOpcode::quit;
  case Instruction::Opcode::string:
    return ExecutableOpcode::string;
  case Instruction::Opcode::number:
    return ExecutableOpcode::number;
  case Instruction::Opcode::variable:
    return ExecutableOpcode::variable;
  case Instruction::Opcode::array_element:
    return ExecutableOpcode::array_element;
  case Instruction::Opcode::array:
    return ExecutableOpcode::array;
  case Instruction::Opcode::scale:
    return ExecutableOpcode::scale;
  case Instruction::Opcode::ibase:
    return ExecutableOpcode::ibase;
  case Instruction::Opcode::obase:
    return ExecutableOpcode::obase;
  case Instruction::Opcode::add:
    return ExecutableOpcode::add;
  case Instruction::Opcode::subtract:
    return ExecutableOpcode::subtract;
  case Instruction::Opcode::negate:
    return ExecutableOpcode::negate;
  case Instruction::Opcode::multiply:
    return ExecutableOpcode::multiply;
  case Instruction::Opcode::divide:
    return ExecutableOpcode::divide;
  case Instruction::Opcode::modulo:
    return ExecutableOpcode::modulo;
  case Instruction::Opcode::power:
    return ExecutableOpcode::power;
  case Instruction::Opcode::load:
    return ExecutableOpcode::load;
  case Instruction::Opcode::store:
    return ExecutableOpcode::store;
  case Instruction::Opcode::scale_expr:
    return ExecutableOpcode::scale_expr;
  case Instruction::Opcode::sqrt:
    return ExecutableOpcode::sqrt;
  case Instruction::Opcode::abs:
    return ExecutableOpcode::abs;
  case Instruction::Opcode::length:
    return ExecutableOpcode::length;
  case Instruction::Opcode::equals:
    return ExecutableOpcode::equals;
  case Instruction::Opcode::less_than_equals:
    return ExecutableOpcode::less_than_equals;
  case Instruction::Opcode::not_equals:
    return ExecutableOpcode::not_equals;
  case Instruction::Opcode::less_than:
    return ExecutableOpcode::less_than;
  case Instruction::Opcode::branch:
    return ExecutableOpcode::branch;
  case Instruction::Opcode::branch_zero:
    return ExecutableOpcode::branch_zero;
  case Instruction::Opcode::return_:
    return ExecutableOpcode::return_;
  case Instruction::Opcode::call:
    return ExecutableOpcode::call;
  case Instruction::Opcode::push_param_mark:
    return ExecutableOpcode::push_param_mark;
  case Instruction::Opcode::pop_param_mark:
    return ExecutableOpcode::pop_param_mark;
  case Instruction::Opcode::push_param:
    return ExecutableOpcode::push_param;
  case Instruction::Opcode::pop_param:
    return ExecutableOpcode::pop_param;
  case Instruction::Opcode::pop_param_array:
    return ExecutableOpcode::pop_param_array;
  case Instruction::Opcode::function_begin:
    return ExecutableOpcode::function_begin;
  case Instruction::Opcode::function_end:
    return ExecutableOpcode::function_end;
  }
  std::abort();
}

auto ExecutableInstruction::source_opcode() const -> Instruction::Opcode
{
  switch (opcode) {
  case ExecutableOpcode::eof:
    return Instruction::Opcode::eof;
  case ExecutableOpcode::print:
    return Instruction::Opcode::print;
  case ExecutableOpcode::quit:
    return Instruction::Opcode::quit;
  case ExecutableOpcode::string:
    return Instruction::Opcode::string;
  case ExecutableOpcode::number:
    return Instruction::Opcode::number;
  case ExecutableOpcode::variable:
    return Instruction::Opcode::variable;
  case ExecutableOpcode::array_element:
    return Instruction::Opcode::array_element;
  case ExecutableOpcode::array:
    return Instruction::Opcode::array;
  case ExecutableOpcode::scale:
    return Instruction::Opcode::scale;
  case ExecutableOpcode::ibase:
    return Instruction::Opcode::ibase;
  case ExecutableOpcode::obase:
    return Instruction::Opcode::obase;
  case ExecutableOpcode::add:
    return Instruction::Opcode::add;
  case ExecutableOpcode::subtract:
    return Instruction::Opcode::subtract;
  case ExecutableOpcode::negate:
    return Instruction::Opcode::negate;
  case ExecutableOpcode::multiply:
    return Instruction::Opcode::multiply;
  case ExecutableOpcode::divide:
    return Instruction::Opcode::divide;
  case ExecutableOpcode::modulo:
    return Instruction::Opcode::modulo;
  case ExecutableOpcode::power:
    return Instruction::Opcode::power;
  case ExecutableOpcode::load:
    return Instruction::Opcode::load;
  case ExecutableOpcode::store:
    return Instruction::Opcode::store;
  case ExecutableOpcode::scale_expr:
    return Instruction::Opcode::scale_expr;
  case ExecutableOpcode::sqrt:
    return Instruction::Opcode::sqrt;
  case ExecutableOpcode::abs:
    return Instruction::Opcode::abs;
  case ExecutableOpcode::length:
    return Instruction::Opcode::length;
  case ExecutableOpcode::equals:
    return Instruction::Opcode::equals;
  case ExecutableOpcode::less_than_equals:
    return Instruction::Opcode::less_than_equals;
  case ExecutableOpcode::not_equals:
    return Instruction::Opcode::not_equals;
  case ExecutableOpcode::less_than:
    return Instruction::Opcode::less_than;
  case ExecutableOpcode::branch:
    return Instruction::Opcode::branch;
  case ExecutableOpcode::branch_zero:
    return Instruction::Opcode::branch_zero;
  case ExecutableOpcode::return_:
    return Instruction::Opcode::return_;
  case ExecutableOpcode::call:
    return Instruction::Opcode::call;
  case ExecutableOpcode::push_param_mark:
    return Instruction::Opcode::push_param_mark;
  case ExecutableOpcode::pop_param_mark:
    return Instruction::Opcode::pop_param_mark;
  case ExecutableOpcode::push_param:
    return Instruction::Opcode::push_param;
  case ExecutableOpcode::pop_param:
    return Instruction::Opcode::pop_param;
  case ExecutableOpcode::pop_param_array:
    return Instruction::Opcode::pop_param_array;
  case ExecutableOpcode::function_begin:
    return Instruction::Opcode::function_begin;
  case ExecutableOpcode::function_end:
    return Instruction::Opcode::function_end;
  case ExecutableOpcode::load_variable:
    return Instruction::Opcode::variable;
  case ExecutableOpcode::load_array:
    return Instruction::Opcode::array;
  case ExecutableOpcode::load_ibase:
    return Instruction::Opcode::ibase;
  case ExecutableOpcode::load_obase:
    return Instruction::Opcode::obase;
  case ExecutableOpcode::load_scale:
    return Instruction::Opcode::scale;
  }
  std::abort();
}

auto lower_executable(Instructions const& code, ResultSlotPlan const& slots, bool persistent,
                      bool fuse_loads) -> ExecutableCode
{
  using I = Instruction::Index;
  using O = Instruction::Opcode;
  using C = std::uint32_t;
  auto const size = code.size();
  if (size == 0 || (!persistent && size < 16) || size >= std::numeric_limits<C>::max())
    return {};
  ExecutableCode result(size, ExecutableInstruction{O::eof, 0, {}, {}});
  bool representable = true;
  auto encode = [&](I pc, Instruction::Operand const& operand, bool control) {
    ExecutableOperand encoded;
    std::visit(
      Overloaded{
        [&](Instruction::Offset offset) {
          auto const index = pc + static_cast<I>(offset);
          encoded = ExecutableOperand(static_cast<C>(control ? index : slots.index(index)));
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
    lowered.opcode = executable_opcode(source.opcode());
    lowered.result = static_cast<C>(slots.index(i));
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
  if (fuse_loads) {
    for (I i = 0; i + 1 < size; ++i) {
      if (result[i + 1].source_opcode() != O::load ||
          std::get<Instruction::Offset>(code[i + 1].op1()) != -1)
        continue;
      auto& current = result[i];
      switch (current.source_opcode()) {
      case O::variable:
        current.opcode = ExecutableOpcode::load_variable;
        break;
      case O::array:
        current.opcode = ExecutableOpcode::load_array;
        break;
      case O::ibase:
        current.opcode = ExecutableOpcode::load_ibase;
        break;
      case O::obase:
        current.opcode = ExecutableOpcode::load_obase;
        break;
      case O::scale:
        current.opcode = ExecutableOpcode::load_scale;
        break;
      default:
        continue;
      }
      // These location opcodes have no second operand. Keep the location's
      // register and use the spare word for the following load's destination.
      // The load record itself stays unchanged for independent suffix entry.
      current.second = ExecutableOperand(result[i + 1].result);
    }
  }
  return result;
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
