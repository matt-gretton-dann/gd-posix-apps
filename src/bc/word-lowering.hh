/** Ordinary-context lowering to the Number-independent execution interface.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_WORD_LOWERING_HH
#define GD_BC_WORD_LOWERING_HH

#include "bc.hh"
#include "word-function.hh"

namespace GD::Bc::Details {
// The caller has already verified the source operand schemas/types. Prove a
// one-scalar prologue and parameter/auto-only effects, rejecting all other
// operations even in unreachable code. The returned program owns its data.
auto lower_word_function(Instructions const& source, VariableMask locals, Letter self)
  -> std::optional<Word::Program>;
}  // namespace GD::Bc::Details
#endif
