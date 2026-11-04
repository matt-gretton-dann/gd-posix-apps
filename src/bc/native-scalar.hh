/** Experimental native lowering for bounded verified scalar streams.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_NATIVE_SCALAR_HH_INCLUDED
#define GD_BC_NATIVE_SCALAR_HH_INCLUDED

#include <array>
#include <bitset>
#include <cstdint>
#include <optional>

#include "bc.hh"

namespace GD::Bc::Details {
using NativeInterruptCheck = bool (*)() noexcept;
using NativeScalarResult = std::array<std::uint64_t, Letter::count_>;

// Accepts only fully verified top-level streams with zero-initialized
// referenced scalars. A failed attempt never changes VM state or emits output.
auto native_scalar_variables(Instructions const& code) -> std::bitset<Letter::count_>;
auto run_native_scalar(Instructions const& code, std::bitset<Letter::count_> variables,
                       NativeInterruptCheck interrupted) -> std::optional<NativeScalarResult>;
}  // namespace GD::Bc::Details

#endif
