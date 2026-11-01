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
// One byte per instruction: nonzero permits transferring its first arithmetic
// operand. An empty plan keeps ordinary copies without a per-instruction table.
using ConsumptionPlan =
  std::vector<std::uint8_t,
              Profile::Allocator<std::uint8_t, Profile::AllocationKind::execution_plans>>;

// Call only after complete entry verification. A transfer needs one static use
// and a producer earlier in the same straight-line basic block. In particular,
// a producer outside a loop cannot be consumed merely because its consumer is
// the only static use. Function plans persist; tiny outer streams skip planning.
auto plan_consumption(Instructions const& code, bool persistent) -> ConsumptionPlan;
}  // namespace GD::Bc::Details
#endif
