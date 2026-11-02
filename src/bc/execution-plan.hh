/** \file execution-plan.hh
 * Conservative last-use arithmetic ownership after entry verification.
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
// execution region. Zero is a shared empty variant sentinel for non-producers
// and skipped definition bodies. A tagged numeric map selects a separate pool.
// Producers never share a (pool,index) pair; no liveness reuse is inferred.
struct ResultSlotPlan
{
  using CompactIndex = std::uint32_t;
  using Map =
    std::vector<CompactIndex,
                Profile::Allocator<CompactIndex, Profile::AllocationKind::execution_plans>>;
  Map indices;
  std::size_t count{0};
  std::size_t number_count{0};
  static constexpr CompactIndex number_tag = CompactIndex{1} << 31;

  auto is_number(Instruction::Index instruction) const -> bool
  {
    return number_count != 0 && (indices[instruction] & number_tag) != 0;
  }

  // Empty metadata retains the original instruction-indexed representation.
  auto storage_size(std::size_t instruction_count) const -> std::size_t
  {
    return indices.empty() ? instruction_count : count;
  }
  auto index(Instruction::Index instruction) const -> Instruction::Index
  {
    if (indices.empty())
      return instruction;
    return number_count == 0 ? indices[instruction] : indices[instruction] & ~number_tag;
  }
};

// Call after complete verification. Small streams, unrepresentable compact
// indices and layouts without a net slot/map payload saving keep the generic
// identity layout. slot_bytes is the caller's actual result-slot sizeof.
auto plan_result_slots(Instructions const& code, std::size_t slot_bytes) -> ResultSlotPlan;

// Separate Number storage only when both index pools fit the tagged domain and
// the complete map/pool/header payload is smaller than the best existing
// producer-map or identity layout. Other streams keep that generic fallback.
// Verification establishes every producer's fixed type and availability first.
auto plan_typed_result_slots(Instructions const& code, std::size_t slot_bytes,
                             std::size_t number_bytes, std::size_t extra_buffer_bytes)
  -> ResultSlotPlan;

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
