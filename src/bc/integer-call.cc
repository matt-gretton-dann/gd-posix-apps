/** Portable, ownership-free call entry boundary.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "integer-call.hh"

void GD::Bc::Details::dispatch_integer_call(IntegerCallEntry const* entry, void* state,
                                            Letter function, Location const& location,
                                            void const* argument, void* result)
{
  entry->function(entry, state, function, location, argument, result);
}
