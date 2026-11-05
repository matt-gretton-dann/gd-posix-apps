/** Portable checked integer execution for pure verified functions.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_INTEGER_FUNCTION_HH_INCLUDED
#define GD_BC_INTEGER_FUNCTION_HH_INCLUDED

#include <cstdint>
#include <optional>
#include <vector>

#include "bc.hh"

namespace GD::Bc::Details {
struct IntegerFunctionInstruction
{
  Instruction::Opcode opcode;
  std::uint16_t left{};
  std::uint16_t right{};
};

struct IntegerFunction
{
  std::vector<IntegerFunctionInstruction> code;
  unsigned parameter{};
};

// Entry validation must already have proved result availability and branches.
// Only parameter/auto scalars and self-calls are accepted; all other effects
// reject the plan, including unreachable instructions. No literal cache mutates.
auto plan_integer_function(Instructions const& source, VariableMask locals, Letter self)
  -> std::optional<IntegerFunction>;

struct IntegerFunctionScratch
{
  struct Frame
  {
    std::size_t pc;
    std::size_t marks;
  };
  struct Argument
  {
    std::int64_t value{};
    bool present = false;
  };
  std::vector<std::int64_t> values;
  std::vector<Frame> frames;
  std::vector<Argument> arguments;
};

// A guard failure leaves every caller-owned value intact. Execution uses at
// most 1 MiB of logical scratch and polls interrupts before each instruction.
// Capacities can be retained between attempts; all live state is cleared.
auto run_integer_function(IntegerFunction const& code, std::int64_t argument,
                          IntegerFunctionScratch& scratch, bool (*interrupted)() noexcept)
  -> std::optional<std::int64_t>;
}  // namespace GD::Bc::Details
#endif
