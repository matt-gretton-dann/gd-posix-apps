/** Opaque portable scalar transaction boundary, independent of VM frame layout.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_PORTABLE_SCALAR_VM_HH_INCLUDED
#define GD_BC_PORTABLE_SCALAR_VM_HH_INCLUDED

#include <atomic>
#include <span>

#include "portable-scalar.hh"

namespace GD::Bc::Details {
// The existing execution-scoped signal handler owns this flag. The backend
// receives its address explicitly; it installs no handlers and has no TLS state.
extern std::atomic_flag have_been_interrupted;
auto portable_scalar_flag_interrupted(void const* flag) noexcept -> bool;

struct PortableScalarVmContext
{
  std::span<Number, Letter::count_> variables;
  Number::NumType input_base;
  bool full_checks;
  ScalarContextInterruptCheck interrupted;
  void const* interrupt_context;
};

// The entire optional transaction lives outside the interpreter's central
// execution function. False leaves every public scalar untouched. Context and
// any interrupt callback storage must remain valid for this synchronous call.
auto try_portable_scalar(Instructions const& code, PortableScalarVmContext const& context) noexcept
  -> bool;
}  // namespace GD::Bc::Details

#endif
