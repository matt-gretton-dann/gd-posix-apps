/** Shared bounded SSA validation and volatile-register allocation.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_NATIVE_EXPRESSION_PLAN_HH
#define GD_BC_NATIVE_EXPRESSION_PLAN_HH

#include <array>
#include <limits>
#include <optional>
#include <span>
#include <utility>

#include "native-expression.hh"

namespace GD::Bc::Details {
// Both instruction-set backends use the same domain proof and last-use plan.
// Only encoding and executable-page publication belong to the architecture.
template<typename Emitter, unsigned argument_register>
auto emit_native_expression(std::span<NativeExpressionOp const> operations, std::uint8_t result,
                            std::span<unsigned const> temporary_registers) -> std::optional<Emitter>
{
  static_assert(argument_register < 16);
  for (auto reg : temporary_registers)
    if (reg >= 16 || reg == argument_register)
      return std::nullopt;
  using Kind = NativeExpressionOp::Kind;
  constexpr unsigned maximum_operations = 64;
  if (operations.empty() || operations.size() > maximum_operations || result >= operations.size())
    return std::nullopt;
  std::array<unsigned, maximum_operations> last_use{};
  for (unsigned i = 0; i != operations.size(); ++i) {
    auto const& op = operations[i];
    switch (op.kind) {
    case Kind::argument:
      break;
    case Kind::constant:
      if (op.constant > static_cast<std::uint64_t>(std::numeric_limits<std::int64_t>::max()))
        return std::nullopt;
      break;
    case Kind::add:
    case Kind::multiply:
      if (op.left >= i || op.right >= i)
        return std::nullopt;
      last_use[op.left] = i;
      last_use[op.right] = i;
      break;
    default:
      return std::nullopt;
    }
  }
  last_use[result] = static_cast<unsigned>(operations.size());
  Emitter emit;
  emit.start(argument_register);
  std::array<int, maximum_operations> registers;
  registers.fill(-1);
  std::array<bool, 16> occupied{};
  auto free_register = [&]() -> int {
    for (auto reg : temporary_registers)
      if (!occupied[reg])
        return static_cast<int>(reg);
    return -1;
  };
  for (unsigned i = 0; i != operations.size(); ++i) {
    auto const& op = operations[i];
    int destination = -1;
    if (op.kind == Kind::argument) {
      destination = static_cast<int>(argument_register);
    }
    else if (op.kind == Kind::constant) {
      destination = free_register();
      if (destination < 0)
        return std::nullopt;
      emit.constant(static_cast<unsigned>(destination), op.constant);
    }
    else {
      auto left = registers[op.left];
      auto right = registers[op.right];
      if (left < 0 || right < 0)
        return std::nullopt;
      if (left != static_cast<int>(argument_register) && last_use[op.left] == i)
        destination = left;
      else if (right != static_cast<int>(argument_register) && last_use[op.right] == i) {
        destination = right;
        std::swap(left, right);  // Both supported operations are commutative.
      }
      else
        destination = free_register();
      if (destination < 0)
        return std::nullopt;
      emit.move(static_cast<unsigned>(destination), static_cast<unsigned>(left));
      emit.binary(op.kind, static_cast<unsigned>(destination), static_cast<unsigned>(right));
    }
    for (unsigned old = 0; old < i; ++old) {
      if (last_use[old] == i && registers[old] != static_cast<int>(argument_register)) {
        if (registers[old] >= 0 && registers[old] != destination)
          occupied[static_cast<unsigned>(registers[old])] = false;
        registers[old] = -1;
      }
    }
    registers[i] = destination;
    if (destination != static_cast<int>(argument_register))
      occupied[static_cast<unsigned>(destination)] = last_use[i] > i;
  }
  if (registers[result] < 0)
    return std::nullopt;
  emit.finish(static_cast<unsigned>(registers[result]));
  return std::optional<Emitter>(std::move(emit));
}
}  // namespace GD::Bc::Details
#endif
