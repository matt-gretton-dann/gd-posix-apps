/** \file execution-plan.hh
 * Verified executable lowering and conservative temporary ownership.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_EXECUTION_PLAN_HH_INCLUDED
#define GD_BC_EXECUTION_PLAN_HH_INCLUDED

#include <cstdint>
#include <vector>

#include "bc.hh"
#include "profiling.hh"

namespace GD::Bc::Details {
// Static value contracts shared by verification and executable preparation.
enum class ResultKind { none, number, string, scalar_location, array_location, array_values };

// Operand schemas and all data-offset bounds must already be checked. This
// classifies producer types; it does not establish availability or dominance.
auto classify_result(Instructions const& code, Instruction::Index index) -> ResultKind;

// A compact map assigns slots only to value-producing instructions in this
// execution region. Zero is a shared empty sentinel for non-producers and
// skipped definition bodies. Producers never share slots with one another;
// no liveness or control-flow-dependent reuse is inferred here.
struct ResultSlotPlan
{
  using CompactIndex = std::uint32_t;
  using Map =
    std::vector<CompactIndex,
                Profile::Allocator<CompactIndex, Profile::AllocationKind::execution_plans>>;
  Map indices;
  std::size_t count{0};

  // Empty metadata retains the original instruction-indexed representation.
  auto storage_size(std::size_t instruction_count) const -> std::size_t
  {
    return indices.empty() ? instruction_count : count;
  }
  auto index(Instruction::Index instruction) const -> Instruction::Index
  {
    return indices.empty() ? instruction : indices[instruction];
  }
};

// Call after complete verification. Small streams, unrepresentable compact
// indices and layouts without a net slot/map payload saving keep the generic
// identity layout. slot_bytes is the caller's actual result-slot sizeof.
auto plan_result_slots(Instructions const& code, std::size_t slot_bytes) -> ResultSlotPlan;

// Opcode-specific operands in the verified execution stream. Only the member
// selected during lowering may be read: data/control operands use index, while
// variable/array/function immediates use letter. No discriminant is needed
// because entry verification and the opcode establish the operand schema.
union ExecutableOperand
{
  std::uint32_t index{0};
  Letter letter;
  ExecutableOperand() : index(0) {}
  explicit ExecutableOperand(std::uint32_t value) : index(value) {}
  explicit ExecutableOperand(Letter value) : letter(value) {}
};

// The executable owns its dispatch opcode space, including pair operations.
// Keep ordinary opcodes first and fused operations last. Exhaustive source and
// reverse conversion switches make schema additions explicit in both formats.
enum class ExecutableOpcode : std::uint32_t {
  eof,
  print,
  quit,
  string,
  number,
  variable,
  array_element,
  array,
  scale,
  ibase,
  obase,
  add,
  subtract,
  negate,
  multiply,
  divide,
  modulo,
  power,
  load,
  store,
  scale_expr,
  sqrt,
  abs,
  length,
  equals,
  less_than_equals,
  not_equals,
  less_than,
  branch,
  branch_zero,
  return_,
  call,
  push_param_mark,
  pop_param_mark,
  push_param,
  pop_param,
  pop_param_array,
  function_begin,
  function_end,
  load_variable,
  load_array,
  load_ibase,
  load_obase,
  load_scale,
};
auto executable_opcode(Instruction::Opcode opcode) -> ExecutableOpcode;

// Common targets use 16 bytes per executable instruction. The source bytecode
// remains the owner of strings, literal caches and diagnostic/definition data.
// Indices are absolute register numbers for data, absolute PCs for branches.
struct ExecutableInstruction
{
  ExecutableOpcode opcode;
  std::uint32_t result;
  ExecutableOperand first;
  ExecutableOperand second;

  ExecutableInstruction(Instruction::Opcode operation, std::uint32_t destination,
                        ExecutableOperand lhs = {}, ExecutableOperand rhs = {})
      : opcode(executable_opcode(operation)), result(destination), first(lhs), second(rhs)
  {
  }
  auto source_opcode() const -> Instruction::Opcode;
  auto is_fused_load() const -> bool { return opcode >= ExecutableOpcode::load_variable; }
};
using ExecutableCode =
  std::vector<ExecutableInstruction,
              Profile::Allocator<ExecutableInstruction, Profile::AllocationKind::execution_plans>>;

// Complete verification and result-slot planning must precede lowering. Empty
// code means use the existing interpreter: tiny transient streams and indices/immediates
// outside the compact domain retain that generic fallback. Function bodies
// skipped by an outer definition are lowered only when registered separately.
auto lower_executable(Instructions const& code, ResultSlotPlan const& slots,
                      bool persistent = false, bool fuse_loads = false) -> ExecutableCode;

// One byte per instruction: nonzero permits transferring its first arithmetic
// operand. An empty plan keeps ordinary copies without a per-instruction table.
using ConsumptionPlan =
  std::vector<std::uint8_t,
              Profile::Allocator<std::uint8_t, Profile::AllocationKind::execution_plans>>;

// Call only after complete entry verification. A transfer needs one static use
// and a producer earlier in the same straight-line basic block. In particular,
// a producer outside a loop cannot be consumed merely because its consumer is
// the only static use. Definition bodies are planned separately from the pack
// that registers them. Function plans persist; tiny outer streams skip planning.
auto plan_consumption(Instructions const& code, bool persistent) -> ConsumptionPlan;
}  // namespace GD::Bc::Details
#endif
