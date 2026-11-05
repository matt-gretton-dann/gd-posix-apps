/** Experimental operand-stack lowering for verified scalar streams.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "stack-execution.hh"

#include <algorithm>
#include <limits>

namespace GD::Bc::Details {
auto plan_stack_execution(Instructions const& source, bool function_body)
  -> std::optional<StackCode>
{
  using Op = Instruction::Opcode;
  using Index = Instruction::Index;
  auto const size = source.size();
  if (size == 0 || (!function_body && size < stack_statement_min_size) ||
      size >= std::numeric_limits<std::uint16_t>::max())
    return {};

  auto const relative = [&source](Index pc, bool second) {
    auto const& operand = second ? source[pc].op2() : source[pc].op1();
    return pc + static_cast<Index>(std::get<Instruction::Offset>(operand));
  };
  std::vector<bool> entries(size + 1, false);
  for (Index pc = 0; pc < size; ++pc) {
    auto const op = source[pc].opcode();
    if (op == Op::branch || op == Op::branch_zero) {
      entries[relative(pc, op == Op::branch_zero)] = true;
      entries[pc + 1] = true;
    }
  }

  StackCode code;
  code.instructions.reserve(size);
  std::vector<Index> stack;
  std::vector<std::uint16_t> lowered(size + 1);
  std::vector<Index> jumps;
  for (Index pc = 0; pc < size; ++pc) {
    if (entries[pc] && !stack.empty())
      return {};
    lowered[pc] = static_cast<std::uint16_t>(code.instructions.size());
    auto const op = source[pc].opcode();
    StackInstruction record{0, static_cast<std::uint8_t>(op), false};
    auto const pop = [&stack](Index expected) {
      if (stack.empty() || stack.back() != expected)
        return false;
      stack.pop_back();
      return true;
    };
    switch (op) {
    case Op::variable:
      continue;  // Scalar locations are encoded directly in load/store.
    case Op::number:
    case Op::call:
      record.operand = static_cast<std::uint16_t>(pc);
      stack.push_back(pc);
      break;
    case Op::load:
    case Op::store: {
      auto const& location = source[relative(pc, false)];
      if (location.opcode() != Op::variable)
        return {};
      record.operand =
        static_cast<std::uint16_t>(static_cast<unsigned>(std::get<Variable>(location.op1()).get()));
      if (op == Op::load)
        stack.push_back(pc);
      else if (!pop(relative(pc, true)))
        return {};
      break;
    }
    case Op::negate:
    case Op::abs:
    case Op::length:
    case Op::scale_expr:
    case Op::sqrt:
      if (!pop(relative(pc, false)))
        return {};
      stack.push_back(pc);
      break;
    case Op::add:
    case Op::subtract:
    case Op::multiply:
    case Op::divide:
    case Op::modulo:
    case Op::power:
    case Op::less_than:
    case Op::less_than_equals:
    case Op::equals:
    case Op::not_equals: {
      if (stack.size() < 2)
        return {};
      auto const lhs = relative(pc, false);
      auto const rhs = relative(pc, true);
      record.reversed = stack.back() == lhs;
      if (record.reversed ? (!pop(lhs) || !pop(rhs)) : (!pop(rhs) || !pop(lhs)))
        return {};
      stack.push_back(pc);
      break;
    }
    case Op::pop_param:
      if (!function_body)
        return {};
      stack.push_back(pc);
      break;
    case Op::return_:
      if (!function_body || !pop(relative(pc, false)) || !stack.empty())
        return {};
      break;
    case Op::push_param_mark:
    case Op::pop_param_mark:
      break;
    case Op::push_param:
      if (!pop(relative(pc, false)))
        return {};
      break;
    case Op::print:
      if (!pop(relative(pc, false)))
        return {};
      record.operand = static_cast<std::uint16_t>(std::get<Instruction::Stream>(source[pc].op2()));
      break;
    case Op::branch_zero: {
      if (!pop(relative(pc, false)))
        return {};
      if (!stack.empty())
        return {};
      record.operand = static_cast<std::uint16_t>(relative(pc, true));
      // A verified adjacent comparison has a single consumer here. Keep
      // ordinary branches for any independently reachable instruction.
      if (pc != 0 && !entries[pc] && relative(pc, false) == pc - 1) {
        switch (source[pc - 1].opcode()) {
        case Op::less_than:
        case Op::less_than_equals:
        case Op::equals:
        case Op::not_equals:
          code.instructions.back().operand = record.operand;
          code.instructions.back().branch_on_false = true;
          jumps.push_back(code.instructions.size() - 1);
          continue;
        default:
          break;
        }
      }
      jumps.push_back(code.instructions.size());
      break;
    }
    case Op::branch:
      if (!stack.empty())
        return {};
      record.operand = static_cast<std::uint16_t>(relative(pc, false));
      jumps.push_back(code.instructions.size());
      break;
    case Op::eof:
      if (!stack.empty())
        return {};
      break;
    default:
      return {};
    }
    code.depth = std::max(code.depth, stack.size());
    code.instructions.push_back(record);
  }
  if (!stack.empty())
    return {};
  lowered[size] = static_cast<std::uint16_t>(code.instructions.size());
  for (auto const jump : jumps)
    code.instructions[jump].operand = lowered[code.instructions[jump].operand];
  return code;
}
}  // namespace GD::Bc::Details
