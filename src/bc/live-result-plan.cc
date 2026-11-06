/** Standalone liveness planner contract scaffold.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "live-result-plan.hh"

namespace GD::Bc::Details {
auto plan_live_result_slots(Instructions const&, ResultSlotPlan fallback,
                            std::size_t, bool, LivenessBudget) -> ResultSlotPlan
{
  // Preserve correct producer-only execution until the liveness proof exists.
  return fallback;
}
}  // namespace GD::Bc::Details
