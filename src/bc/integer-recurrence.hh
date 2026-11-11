/** Exact reduction of closed, pure integer self recurrences.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_INTEGER_RECURRENCE_HH
#define GD_BC_INTEGER_RECURRENCE_HH

#include <cstdint>
#include <optional>
#include <span>

#include "bc.hh"

namespace GD::Bc::Details {
/** Recognize a single decreasing parameter and one additive/multiplicative
 * self call. All literals are single decimal digits, independent of ibase.
 * Integer operands retain scale zero at every caller scale, allowing exact
 * reassociation with Number arithmetic. Other definitions/inputs decline.
 */
struct IntegerRecurrence
{
  Letter self;
  int stop;
  int base;
  unsigned step;
  bool parameter_base;
  bool multiply;

  static auto match(std::span<Instruction const> instructions, VariableMask locals, Letter self)
    -> std::optional<IntegerRecurrence>;
  auto admits(std::uint64_t argument) const noexcept -> bool;
  // No caller-visible state changes until the complete result is published.
  // The callback is checked once per exact arithmetic step.
  auto execute(std::uint64_t argument, Number& result, bool (*interrupted)()) const -> bool;
};
}  // namespace GD::Bc::Details
#endif
