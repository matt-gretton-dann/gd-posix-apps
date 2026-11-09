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
template<typename Limb>
using SelectedDigitStorage =
#if GD_BC_COMPACT_DIGITS
  std::conditional_t<(sizeof(CompactDigitStorage<Limb>) < sizeof(DigitStorage<Limb>)),
                     CompactDigitStorage<Limb>, DigitStorage<Limb>>;
#else
  DigitStorage<Limb>;
#endif
}  // namespace GD::Bc::Details
#endif
