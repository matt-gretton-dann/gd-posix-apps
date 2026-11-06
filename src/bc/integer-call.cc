/** Portable, ownership-free call entry boundary.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "integer-call.hh"

#include <type_traits>

// Explicitly suppress the two ownership members rather than only their
// enclosing classes: MSVC still emitted defaulted destructors with class-level
// extern declarations. The ordinary library supplies any required definition.
using RouterNumberTraits =
  std::conditional_t<sizeof(void*) == 8, GD::Bc::NumberTraits32, GD::Bc::NumberTraits16>;
namespace GD::Bc {
extern template BasicNumber<RouterNumberTraits>::~BasicNumber() noexcept;
namespace Details {
extern template BasicDigits<RouterNumberTraits>::~BasicDigits();
}  // namespace Details
}  // namespace GD::Bc

auto GD::Bc::Details::dispatch_integer_call(IntegerCallEntry const* entry, void* state,
                                            Letter function, Location const& location,
                                            Number const* argument) -> Number
{
  // Guaranteed prvalue elision preserves the caller's return destination.
  // A machine tail transfer is an inspected optimization, not a requirement
  // for correctness on a platform without a matching code-generation adapter.
  return entry->function(entry, state, function, location, argument);
}
