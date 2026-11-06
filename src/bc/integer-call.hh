/** Non-owning call routing with no numeric ownership dependencies.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_INTEGER_CALL_HH_INCLUDED
#define GD_BC_INTEGER_CALL_HH_INCLUDED

#include <array>

#include "letter.hh"

namespace GD::Bc {
class Location;
namespace Details {
// Matching signatures forward borrowed storage on supported ABIs. Numeric
// arguments and results belong entirely to the ordinary compiler context.
struct IntegerCallEntry
{
  using Function = void (*)(IntegerCallEntry const*, void*, Letter, Location const&, void const*,
                            void*);
  Function function;
};
using IntegerCallEntries = std::array<IntegerCallEntry, Letter::count_>;

void dispatch_integer_call(IntegerCallEntry const* entry, void* state, Letter function,
                           Location const& location, void const* argument, void* result);
}  // namespace Details
}  // namespace GD::Bc
#endif
