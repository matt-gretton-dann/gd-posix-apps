/** Number-independent execution of bounded, pure recursive integer plans.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_WORD_FUNCTION_HH
#define GD_BC_WORD_FUNCTION_HH

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include "profiling.hh"

namespace GD::Bc::Details::Word {
enum class Opcode : std::uint8_t {
  location,
  constant,
  load,
  store,
  negate,
  add,
  subtract,
  multiply,
  less,
  less_equal,
  equal,
  not_equal,
  branch,
  branch_zero,
  mark,
  push,
  call,
  return_
};

struct Instruction
{
  Opcode opcode;
  std::uint16_t left = 0;
  std::uint16_t right = 0;
};

// Decoded operands below local_tag name producer instructions. Tagged operands
// name one of 26 scalar locals. Preparation remaps used names to dense private
// slots; every activation starts those slots at zero. Loads snapshot a local.
inline constexpr std::uint16_t local_tag = 0x8000;
inline constexpr std::size_t local_count = 26;
inline constexpr std::size_t instruction_limit = 1024;
inline constexpr std::size_t scratch_limit = 1024 * 1024;
inline constexpr std::size_t fuel_limit = 4 * 1024 * 1024;

template<typename T, Profile::AllocationKind Kind>
using Storage = std::vector<T, Profile::Allocator<T, Kind>>;

class Program
{
public:
  // Own and validate decoded instructions. Joins require definite producer
  // availability and identical parameter-mark state, independently of the VM
  // verifier. The source-to-plan compiler must additionally prove purity and
  // the meaning of each lowered instruction. No source storage is borrowed.
  static auto prepare(std::span<Instruction const> code, unsigned parameter)
    -> std::optional<Program>;
  [[nodiscard]] auto instructions() const noexcept -> std::span<Instruction const> { return code_; }
  // Prepared instructions and the parameter use dense activation-slot indices.
  [[nodiscard]] auto parameter() const noexcept -> unsigned { return parameter_; }
  [[nodiscard]] auto local_slots() const noexcept -> unsigned { return local_slots_; }

private:
  Program(std::span<Instruction const> code, unsigned parameter)
      : code_(code.begin(), code.end()), parameter_(parameter)
  {
  }
  Storage<Instruction, Profile::AllocationKind::execution_plans> code_;
  unsigned parameter_;
  unsigned local_slots_ = local_count;
};

struct Value
{
  std::int64_t payload = 0;
  bool big = false;
};

struct Scratch
{
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
  Storage<Value, Profile::AllocationKind::frames> values;
  Storage<Frame, Profile::AllocationKind::frames> frames;
  Storage<Argument, Profile::AllocationKind::frames> arguments;
  void clear() noexcept;
};

// The ordinary compiler context owns immutable large-integer records. A big
// cell borrows an index into that bank. Callbacks must normalize numeric zero
// to a small cell, preserve both input values and report all logical live bank
// storage. They may throw; the caller owns bank cleanup and result boxing.
// Requested/retained capacity, coefficient temporaries and process RSS are
// separate measurements from the per-attempt logical scratch limit.
struct Callbacks
{
  void* context = nullptr;
  bool (*interrupted)(void*) noexcept = nullptr;
  std::size_t (*spill_bytes)(void*) noexcept = nullptr;
  bool (*binary)(void*, Opcode, Value, Value, Value&, std::size_t) = nullptr;
  bool (*negate)(void*, Value, Value&, std::size_t) = nullptr;
};

struct Counters
{
  std::uint64_t instructions = 0;
  std::uint64_t self_calls = 0;
};

// Scratch live state clears on entry and every exit, including exceptions.
// A successful big result remains valid only while the caller retains its
// spill bank. Fuel bounds all instruction dispatch, including forward paths;
// callbacks and final publication must preserve the same interrupt contract.
auto execute(Program const& program, std::int64_t argument, Scratch& scratch,
             Callbacks const& callbacks, Counters& counters, std::size_t budget = scratch_limit,
             std::size_t fuel = fuel_limit) -> std::optional<Value>;
}  // namespace GD::Bc::Details::Word
#endif
