/** Non-owning call routing for the optional integer tier.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_INTEGER_CALL_HH_INCLUDED
#define GD_BC_INTEGER_CALL_HH_INCLUDED

#include "bc.hh"

namespace GD::Bc::Details {
// All entries use the same free-function ABI. The router forwards the return
// slot directly; it neither creates nor destroys an intermediate Number.
using IntegerCallEntry = auto (*)(void*, Letter, Location const&, Number const*) -> Number;
using IntegerCallEntries = std::array<IntegerCallEntry, Letter::count_>;

auto dispatch_integer_call(IntegerCallEntry entry, void* state, Letter function,
                           Location const& location, Number const* argument) -> Number;
}  // namespace GD::Bc::Details
#endif
