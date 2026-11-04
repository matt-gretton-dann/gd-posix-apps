/** Experimental operand-stack lowering for verified scalar streams.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_STACK_EXECUTION_HH_INCLUDED
#define GD_BC_STACK_EXECUTION_HH_INCLUDED

#include <cstdint>
#include <limits>
#include <optional>
#include <vector>

#include "bc.hh"

namespace GD::Bc::Details {
static_assert(static_cast<unsigned>(Instruction::Opcode::function_end) <=
              std::numeric_limits<std::uint8_t>::max());

struct StackInstruction
{
  std::uint16_t operand{};
  std::uint8_t opcode{};
  bool reversed{};
};

struct StackCode
{
  std::vector<StackInstruction> instructions;
  std::size_t depth{};
};

// All operand bounds and control flow must already have been verified.
// A stack plan additionally proves exact operand order and empty joins.
auto plan_stack_execution(Instructions const& source, bool function_body = false)
  -> std::optional<StackCode>;
}  // namespace GD::Bc::Details

#endif
