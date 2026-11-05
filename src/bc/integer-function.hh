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
// Instantiate promoted arithmetic in the ordinary library compiler context,
// rather than exporting shared arithmetic definitions from the native engine.
// The left operand is an owned snapshot; unsupported operations return empty.
auto integer_number_binary(Instruction::Opcode opcode, Number left, Number const& right)
  -> std::optional<Number>;

struct IntegerFunctionInstruction
{
  Instruction::Opcode opcode;
  std::uint16_t left{};
  std::uint16_t right{};
};

struct IntegerFunction
{
  std::vector<
    IntegerFunctionInstruction,
    Profile::Allocator<IntegerFunctionInstruction, Profile::AllocationKind::execution_plans>>
    code;
  unsigned parameter{};
};

// Entry validation must already have proved result availability and branches.
// Only parameter/auto scalars and self-calls are accepted; all other effects
// reject the plan, including unreachable instructions. No literal cache mutates.
auto plan_integer_function(Instructions const& source, VariableMask locals, Letter self)
  -> std::optional<IntegerFunction>;

struct IntegerFunctionScratch
{
  struct Value
  {
    std::int64_t payload{};
    bool big = false;
  };
  struct Spill
  {
    Number value;
    std::size_t charge;
  };
  struct Frame
  {
    std::size_t pc;
    std::size_t marks;
  };
  struct Argument
  {
    Value value{};
    bool present = false;
  };
  std::vector<Value, Profile::Allocator<Value, Profile::AllocationKind::frames>> values;
  std::vector<Spill, Profile::Allocator<Spill, Profile::AllocationKind::frames>> spills;
  std::vector<Frame, Profile::Allocator<Frame, Profile::AllocationKind::frames>> frames;
  std::vector<Argument, Profile::Allocator<Argument, Profile::AllocationKind::frames>> arguments;
};

// A guard failure leaves every caller-owned value intact. Execution uses at
// most 1 MiB of logical scratch, charging decimal digits times
// limb size for each owned big value, even when coefficients are shared. Each
// value is limited to 4096 decimal digits; bounded arithmetic temporaries and
// retained coefficient/vector capacity are separate from that logical bound.
// Spill records are
// immutable until the attempt ends, so copied cells retain stable indices.
// Dead spill records remain charged. Poll every instruction.
// Capacities can be retained between attempts; all live state is cleared.
auto run_integer_function(IntegerFunction const& code, std::int64_t argument,
                          IntegerFunctionScratch& scratch, bool (*interrupted)() noexcept)
  -> std::optional<Number>;
}  // namespace GD::Bc::Details
#endif
