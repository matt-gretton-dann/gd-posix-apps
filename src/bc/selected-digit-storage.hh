/** \file selected-digit-storage.hh
 * Select the experimental coefficient layout only when it saves object space.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_SELECTED_DIGIT_STORAGE_HH
#define GD_BC_SELECTED_DIGIT_STORAGE_HH

#include <type_traits>

#include "digit-storage.hh"

#if GD_BC_COMPACT_DIGITS
#  include "compact-digit-storage.hh"
#endif

namespace GD::Bc::Details {
#if GD_BC_COMPACT_DIGITS
// Keep dependent layout queries inside a class-template instantiation. Native
// MSVC rejects these sizeof arguments directly in the alias template.
template<typename Limb>
struct DigitStorageSelection
{
  using Type = std::conditional_t<(sizeof(CompactDigitStorage<Limb>) < sizeof(DigitStorage<Limb>)),
                                  CompactDigitStorage<Limb>, DigitStorage<Limb>>;
};

template<typename Limb>
using SelectedDigitStorage = typename DigitStorageSelection<Limb>::Type;
#else
template<typename Limb>
using SelectedDigitStorage = DigitStorage<Limb>;
#endif
}  // namespace GD::Bc::Details
#endif
