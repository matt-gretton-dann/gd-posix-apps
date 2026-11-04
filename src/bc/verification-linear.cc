/** \file verification-linear.cc
 * Verify simple function definitions without allocating a graph.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "verification.hh"

#if !defined(_WIN32)
// A single definition has two disjoint regions. The outer region skips its
// body; the body has one incoming marker and no owned packs or branch joins.
// Only accept this simple shape when every offset stays inside the body.
auto GD::Bc::Details::is_verified_linear_definition(Instructions const& code) -> bool
{
  using O = Instruction::Opcode;
  using I = Instruction::Index;
  if (code.size() < 3 || code.front().opcode() != O::function_begin ||
      code.back().opcode() != O::function_end) {
    return false;
  }
  auto const end = code.size() - 1;
  if (end + static_cast<I>(std::get<Instruction::Offset>(code.back().op2())) != 0) {
    return false;
  }
  bool incoming = true;
  for (I i = 1; i < end; ++i) {
    auto const& instruction = code[i];
    switch (instruction.opcode()) {
    case O::pop_param:
    case O::pop_param_array:
      if (!incoming) {
        return false;
      }
      break;
    case O::pop_param_mark:
      if (!incoming) {
        return false;
      }
      incoming = false;
      break;
    case O::return_:
      if (incoming) {
        return false;
      }
      break;
    case O::branch:
    case O::branch_zero:
    case O::function_begin:
    case O::function_end:
    case O::quit:
    case O::eof:
    case O::push_param_mark:
    case O::push_param:
    case O::call:
      return false;
    default:
      break;
    }
    auto const is_previous_body_instruction = [i](Instruction::Operand const& operand) {
      auto const* offset = std::get_if<Instruction::Offset>(&operand);
      if (offset == nullptr) {
        return true;
      }
      auto const producer = i + static_cast<I>(*offset);
      return producer > 0 && producer < i;
    };
    if ((instruction.has_op1() && !is_previous_body_instruction(instruction.op1())) ||
        (instruction.has_op2() && !is_previous_body_instruction(instruction.op2()))) {
      return false;
    }
  }
  return !incoming;
}
#endif
