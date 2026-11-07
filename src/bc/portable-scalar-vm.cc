/** Private execution, conversion and publication for portable integer regions.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "portable-scalar-vm.hh"

#include <new>

#include <array>
#include <string>
#include <type_traits>
#include <utility>

namespace GD::Bc::Details {
auto portable_scalar_flag_interrupted(void const* flag) noexcept -> bool
{
  return static_cast<std::atomic_flag const*>(flag)->test(std::memory_order_relaxed);
}

auto try_portable_scalar(Instructions const& code, PortableScalarVmContext const& context) noexcept
  -> bool
try {
  auto const interrupted = [&context]() noexcept {
    return context.interrupted && context.interrupted(context.interrupt_context);
  };
  if (context.full_checks || context.input_base != 10 || interrupted())
    return false;
  auto const plan = plan_portable_scalar(code);
  if (!plan)
    return false;
  auto const variables = plan->variables();
  for (unsigned index = 0; index < Letter::count_; ++index) {
    if (variables.test(index)) {
      auto const& value = context.variables[index];
      if (!value.is_zero() || value.scale() != 0)
        return false;
    }
  }
  GD_BC_PROFILE_EVENT(Profile::Event::portable_scalar_attempts, 1);
  auto const values = plan->run_with_context(context.interrupted, context.interrupt_context);
  if (values && !interrupted()) {
    try {
      std::array<Number, Letter::count_> pending;
      for (unsigned index = 0; index < Letter::count_; ++index) {
        if (variables.test(index))
          pending[index] = Number(std::to_string((*values)[index]), 10);
      }
      if (!interrupted()) {
        // No allocation or throwing copy remains after this commit point.
        // Indices already use Letter's encoding, not character arithmetic.
        static_assert(std::is_nothrow_move_assignable_v<Number>);
        for (unsigned index = 0; index < Letter::count_; ++index) {
          if (variables.test(index))
            context.variables[index] = std::move(pending[index]);
        }
        GD_BC_PROFILE_EVENT(Profile::Event::portable_scalar_successes, 1);
        return true;
      }
    }
    catch (std::bad_alloc const&) {
      // Pending conversions still own every new value until publication.
    }
  }
  GD_BC_PROFILE_EVENT(Profile::Event::portable_scalar_fallbacks, 1);
  return false;
}
catch (std::bad_alloc const&) {
  // Even diagnostic construction for declined analysis is optional storage.
  return false;
}
}  // namespace GD::Bc::Details
