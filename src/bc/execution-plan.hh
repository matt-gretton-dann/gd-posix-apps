/** \file execution-plan.hh
 * Verified executable lowering and conservative temporary ownership.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_EXECUTION_PLAN_HH_INCLUDED
#define GD_BC_EXECUTION_PLAN_HH_INCLUDED

#include <cstddef>
#include <cstdint>
#include <limits>
#include <span>
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
// skipped definition bodies. Verified arithmetic may reuse the slot of a
// first operand whose sole use is in the same straight-line block.
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
// Optional consumption flags must come from plan_consumption for this code.
auto plan_result_slots(Instructions const& code, std::size_t slot_bytes,
                       std::span<std::uint8_t const> consumption = {}) -> ResultSlotPlan;

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

// Common targets use 16 bytes per executable instruction. The source bytecode
// remains the owner of strings, literal caches and diagnostic/definition data.
// Indices are absolute register numbers for data, absolute PCs for branches.
struct ExecutableInstruction
{
  Instruction::Opcode opcode;
  std::uint32_t result;
  ExecutableOperand first;
  ExecutableOperand second;
};
using ExecutableCode =
  std::vector<ExecutableInstruction,
              Profile::Allocator<ExecutableInstruction, Profile::AllocationKind::execution_plans>>;

// Complete verification and result-slot planning must precede lowering. Empty
// code means use the existing interpreter: tiny transient streams and indices/immediates
// outside the compact domain retain that generic fallback. Function bodies
// skipped by an outer definition are lowered only when registered separately.
auto lower_executable(Instructions const& code, ResultSlotPlan const& slots,
                      bool persistent = false) -> ExecutableCode;

// Typed registers have separate numeric and nonnumeric index spaces. Every
// producer owns a distinct slot in its lane; there is no shared empty sentinel
// or liveness reuse. Non-producers and skipped bodies retain no_register.
struct TypedResultPlan
{
  using CompactIndex = ResultSlotPlan::CompactIndex;
  static constexpr auto no_register = std::numeric_limits<CompactIndex>::max();
  ResultSlotPlan::Map indices;
  std::size_t number_count = 0;
  std::size_t other_count = 0;
  auto empty() const noexcept -> bool { return indices.empty(); }
};

// Charge actual caller layouts, including buffer/metadata objects and executable
// records. Fixed costs exclude separately counted vector payloads. This avoids
// assuming a particular standard-library vector/variant representation.
struct TypedResultCosts
{
  std::size_t number_slot_bytes;
  std::size_t other_slot_bytes;
  std::size_t generic_slot_bytes;
  std::size_t generic_fixed_bytes;
  std::size_t typed_fixed_bytes;
  std::size_t generic_instruction_bytes;
  std::size_t typed_instruction_bytes;
};

// Complete verification and the existing generic slot plan precede this call.
// An empty result preserves generic execution for tiny, nonnumeric,
// unrepresentable or unprofitable streams. Cost overflow also preserves it.
auto plan_typed_results(Instructions const& code, ResultSlotPlan const& generic,
                        TypedResultCosts costs, bool persistent = false) -> TypedResultPlan;

// Polymorphic source operations acquire a lane-specific opcode during lowering.
// Arithmetic then addresses Number registers directly; binding/string/array
// values keep a distinct variant lane. Diagnostic print supports every old
// nonnumeric alternative, not just source-language string expressions.
enum class TypedOpcode : std::uint32_t {
  eof,
  print_number,
  print_other,
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
  load_number,
  load_array,
  store_number,
  store_array,
  scale_expr,
  sqrt,
  abs,
  length_number,
  length_array,
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
  push_number,
  push_array,
  pop_param,
  pop_param_array,
  function_begin,
  function_end,
  load_scalar,
  store_scalar
};

struct TypedExecutableInstruction
{
  TypedOpcode opcode;
  std::uint32_t result;
  ExecutableOperand first;
  ExecutableOperand second;
};
using TypedExecutableCode = std::vector<
  TypedExecutableInstruction,
  Profile::Allocator<TypedExecutableInstruction, Profile::AllocationKind::execution_plans>>;

// Data operands use their producer's lane index; branches retain absolute PCs.
// Non-producing result fields are ignored. Strings, call locations and function
// metadata stay in the original source sidecar, as with generic compact code.
auto lower_typed_executable(Instructions const& code, TypedResultPlan const& slots,
                            bool persistent = false) -> TypedExecutableCode;

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
