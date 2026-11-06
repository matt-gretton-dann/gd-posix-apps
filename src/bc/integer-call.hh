/** Non-owning call routing for the optional integer tier.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_INTEGER_CALL_HH_INCLUDED
#define GD_BC_INTEGER_CALL_HH_INCLUDED

#include "bc.hh"

namespace GD::Bc::Details {
// The router and callbacks have identical signatures, including the borrowed
// entry context. This permits forwarding the return slot without reshuffling
// the ABI's argument positions. A machine tail transfer still needs inspection.
struct IntegerCallEntry
{
  using Function = auto (*)(IntegerCallEntry const*, void*, Letter, Location const&, Number const*)
    -> Number;
  Function function;
};
using IntegerCallEntries = std::array<IntegerCallEntry, Letter::count_>;

auto dispatch_integer_call(IntegerCallEntry const* entry, void* state, Letter function,
                           Location const& location, Number const* argument) -> Number;
}  // namespace GD::Bc::Details
#endif
