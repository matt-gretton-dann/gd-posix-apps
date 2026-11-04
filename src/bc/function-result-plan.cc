/** \file function-result-plan.cc
 * Block-local storage for verified function results.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <limits>
#include <vector>

#include "execution-plan.hh"

namespace GD::Bc::Details {
namespace {
template<typename Fn>
void visit_data_uses(Instruction const& instruction, Instruction::Index pc, Fn&& visit)
{
  using O = Instruction::Opcode;
  auto const opcode = instruction.opcode();
  if (opcode == O::branch || opcode == O::function_end)
    return;
  auto const operand = [&](Instruction::Operand const& value) {
    if (auto const* offset = std::get_if<Instruction::Offset>(&value))
      visit(pc + static_cast<Instruction::Index>(*offset));
  };
  if (instruction.has_op1())
    operand(instruction.op1());
  if (instruction.has_op2() && opcode != O::branch_zero)
    operand(instruction.op2());
}
}  // namespace

auto plan_function_result_slots(Instructions const& code, std::size_t slot_bytes,
                                std::span<std::uint8_t const> consumption) -> ResultSlotPlan
{
  using I = Instruction::Index;
  using O = Instruction::Opcode;
  using Compact = ResultSlotPlan::CompactIndex;
  auto const size = code.size();
  if (size < 64 || size >= std::numeric_limits<Compact>::max() ||
      size > std::numeric_limits<std::size_t>::max() / sizeof(Compact) ||
      slot_bytes <= sizeof(Compact))
    return plan_result_slots(code, slot_bytes, consumption);

  // Persistent bodies have no nested definitions. Keep the existing plan
  // for any stream that contains definition metadata.
  for (auto const& instruction : code) {
    if (instruction.opcode() == O::function_begin || instruction.opcode() == O::function_end)
      return plan_result_slots(code, slot_bytes, consumption);
  }
  auto const visit_body = [&](auto&& visit) {
    for (I index = 0; index < size; ++index)
      visit(index);
  };
  std::vector<bool> leader(size, false);
  leader[0] = true;
  auto const mark = [&](I index) {
    if (index < size)
      leader[index] = true;
  };
  visit_body([&](I i) {
    auto const opcode = code[i].opcode();
    if (opcode == O::branch || opcode == O::branch_zero) {
      auto const& operand = opcode == O::branch ? code[i].op1() : code[i].op2();
      mark(i + static_cast<I>(std::get<Instruction::Offset>(operand)));
      mark(i + 1);
    }
    else if (opcode == O::return_ || opcode == O::quit || opcode == O::eof) {
      mark(i + 1);
    }
  });

  std::vector<I> block(size);
  I current = 0;
  for (I i = 0; i < size; ++i) {
    if (leader[i])
      current = i;
    block[i] = current;
  }
  // A sentinel marks producers that need storage for the entire activation.
  // Otherwise last[i] is the last static use, or the producer if unused.
  std::vector<I> last(size, size);
  visit_body([&](I i) {
    if (classify_result(code, i) == ResultKind::number)
      last[i] = i;
  });
  visit_body([&](I i) {
    visit_data_uses(code[i], i, [&](I producer) {
      if (last[producer] != size)
        last[producer] = producer < i && block[producer] == block[i] ? i : size;
    });
  });

  ResultSlotPlan result;
  result.indices.resize(size, 0);
  std::vector<Compact> available;
  Compact next = 1;
  std::size_t chain_count = 1;
  visit_body([&](I i) {
    if (classify_result(code, i) != ResultKind::none) {
      if (consumption.empty() || consumption[i] == 0)
        ++chain_count;
      if (last[i] == size || available.empty()) {
        result.indices[i] = next++;
      }
      else {
        result.indices[i] = available.back();
        available.pop_back();
      }
    }
    // Allocate the destination first. An opcode can keep references to its
    // operands until the destination assignment, even at their last use.
    auto const retire = [&](I producer) {
      if (last[producer] == i) {
        available.push_back(result.indices[producer]);
        last[producer] = size;
      }
    };
    visit_data_uses(code[i], i, retire);
    retire(i);
  });
  if (next >= chain_count || next >= size || size - next <= size * sizeof(Compact) / slot_bytes)
    return plan_result_slots(code, slot_bytes, consumption);
  result.count = next;
  return result;
}
}  // namespace GD::Bc::Details
