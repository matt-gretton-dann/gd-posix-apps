/** Shared eligibility proof for bounded native scalar backends.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "native-scalar.hh"
namespace GD::Bc::Details {
namespace {
using Op = Instruction::Opcode;
using Index = Instruction::Index;
constexpr Index result_slots = native_scalar_result_slots;
auto relative(Index pc, Instruction::Operand const& operand) -> Index
{
  return pc + static_cast<Index>(std::get<Instruction::Offset>(operand));
}
auto scalar(Instruction const& instruction) -> unsigned
{
  return static_cast<unsigned>(std::get<Variable>(instruction.op1()).get());
}
}  // namespace
auto native_scalar_variables(Instructions const& code) -> std::bitset<Letter::count_>
{
  if (code.size() < 16 || code.size() > result_slots)
    return {};
  std::bitset<Letter::count_> variables;
  bool backedge = false;
  for (Index pc = 0; pc < code.size(); ++pc) {
    auto const& instruction = code[pc];
    switch (instruction.opcode()) {
    case Op::variable:
      variables.set(scalar(instruction));
      break;
    case Op::number:
    case Op::add:
    case Op::less_than:
      break;
    case Op::load:
    case Op::store:
      if (code[relative(pc, instruction.op1())].opcode() != Op::variable)
        return {};
      break;
    case Op::branch: {
      auto const target = relative(pc, instruction.op1());
      if (target > code.size())
        return {};
      backedge |= target <= pc;
      break;
    }
    case Op::branch_zero: {
      auto const target = relative(pc, instruction.op2());
      if (target <= pc || target > code.size())
        return {};
      break;
    }
    default:
      return {};
    }
  }
  return backedge ? variables : std::bitset<Letter::count_>{};
}

}  // namespace GD::Bc::Details
