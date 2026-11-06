/** Portable, ownership-free call entry boundary.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "integer-call.hh"

auto GD::Bc::Details::dispatch_integer_call(IntegerCallEntry entry, void* state, Letter function,
                                            Location const& location, Number const* argument)
  -> Number
{
  // Guaranteed prvalue elision preserves the caller's return destination.
  // A machine tail transfer is an inspected optimization, not a requirement
  // for correctness on a platform without a matching code-generation adapter.
  return entry(state, function, location, argument);
}
