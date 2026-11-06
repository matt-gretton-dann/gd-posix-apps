/** Bounded, verified control-flow result liveness experiments.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_LIVE_RESULT_PLAN_HH_INCLUDED
#define GD_BC_LIVE_RESULT_PLAN_HH_INCLUDED

#include <cstddef>

#include "execution-plan.hh"

namespace GD::Bc::Details {
struct LivenessBudget
{
  std::size_t max_instructions = 4096;
  std::size_t max_word_operations = 4 * 1024 * 1024;
};

// Standalone experiment, not selected by the VM. Complete entry verification
// must precede this call. Retain the supplied producer/identity plan whenever
// analysis is declined; no partially constructed map may escape. New colors
// must keep a destination distinct from its instruction's borrowed operands.
auto plan_live_result_slots(Instructions const& code, ResultSlotPlan fallback,
                            std::size_t slot_bytes, bool persistent,
                            LivenessBudget budget = {}) -> ResultSlotPlan;
}  // namespace GD::Bc::Details
#endif
