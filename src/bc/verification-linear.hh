/** \file verification-linear.hh
 * Shared straight-line proof for the inline and outlined verifier paths.
 * SPDX-License-Identifier: Apache-2.0
 */

#ifndef GD_BC_VERIFICATION_LINEAR_HH_INCLUDED
#define GD_BC_VERIFICATION_LINEAR_HH_INCLUDED

#include "verification.hh"

namespace GD::Bc::Details { namespace {
// In an outer, straight-line region, each preceding instruction dominates
// the current one. Fall back to the graph verifier for all other inputs,
// including invalid ones, so its existing diagnostics retain their ordering.
auto verify_linear_stream(Instructions const& code) -> bool
{
  using O = Instruction::Opcode;
  auto is_previous = [](Instruction::Operand const& operand) {
    auto const* offset = std::get_if<Instruction::Offset>(&operand);
    return offset == nullptr || *offset < 0;
  };
  for (Instruction::Index i = 0; i < code.size(); ++i) {
    auto const& instruction = code[i];
    switch (instruction.opcode()) {
    case O::eof:
      return i + 1 == code.size();
    case O::branch:
    case O::branch_zero:
    case O::function_begin:
    case O::function_end:
    case O::return_:
    case O::quit:
    case O::push_param_mark:
    case O::push_param:
    case O::call:
    case O::pop_param:
    case O::pop_param_array:
    case O::pop_param_mark:
      return false;
    default:
      break;
    }
    if ((instruction.has_op1() && !is_previous(instruction.op1())) ||
        (instruction.has_op2() && !is_previous(instruction.op2()))) {
      return false;
    }
  }
  return true;
}
}}  // namespace GD::Bc::Details
#endif
