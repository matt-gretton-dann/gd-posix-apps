/** \file execution-plan.cc
 * Basic-block proof for transferring arithmetic temporaries.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "execution-plan.hh"

#include <cassert>

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
