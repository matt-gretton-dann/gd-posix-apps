/** Portable, ownership-free call entry boundary.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "integer-call.hh"

#include <type_traits>

// Keep MSVC's eagerly emitted ownership definitions in the ordinary library
// context as well. These declarations suppress this TU's instantiations; they
// do not change Number's representation or the callback's return contract.
extern template class GD::Bc::BasicNumber<
  std::conditional_t<sizeof(void*) == 8, GD::Bc::NumberTraits32, GD::Bc::NumberTraits16>>;
extern template class GD::Bc::Details::BasicDigits<
  std::conditional_t<sizeof(void*) == 8, GD::Bc::NumberTraits32, GD::Bc::NumberTraits16>>;

auto GD::Bc::Details::dispatch_integer_call(IntegerCallEntry const* entry, void* state,
                                            Letter function, Location const& location,
                                            Number const* argument) -> Number
{
  // Guaranteed prvalue elision preserves the caller's return destination.
  // A machine tail transfer is an inspected optimization, not a requirement
  // for correctness on a platform without a matching code-generation adapter.
  return entry->function(entry, state, function, location, argument);
}
