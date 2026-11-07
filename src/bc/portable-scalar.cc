/** Portable checked integer execution; no executable memory or platform assembly.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "portable-scalar.hh"

#include <new>

#include <charconv>
#include <limits>
#include <stdexcept>
#include <system_error>

#include "verification.hh"

namespace GD::Bc::Details {
auto plan_portable_scalar(Instructions const& code) -> std::optional<PortableScalarPlan>
{
  using I = Instruction;
  using O = I::Opcode;
  using P = PortableScalarPlan;
  using K = P::Op;
  if (code.size() < 16 || code.size() > P::instruction_limit)
    return {};

  P plan;
  plan.size_ = static_cast<unsigned>(code.size());
  // Check every opcode/schema before examining producer types. In particular,
  // no malformed offset or variable reaches an unchecked array subscript.
  for (unsigned pc = 0; pc < plan.size_; ++pc) {
    auto const& instruction = code[pc];
    auto& operation = plan.code_[pc];
    I::Operand const* first = nullptr;
    try {
      first = &instruction.op1();
    }
    catch (std::logic_error const&) {
      return {};
    }
    switch (instruction.opcode()) {
    case O::number: {
      auto const* text = std::get_if<std::string>(first);
      // Bound optional decoding work as well as instruction/graph storage.
      // Longer literals, including redundant leading zeroes, use Number.
      if (!text || text->empty() || text->size() > 20)
        return {};
      auto const [end, error] =
        std::from_chars(text->data(), text->data() + text->size(), operation.value);
      if (error != std::errc{} || end != text->data() + text->size())
        return {};
      operation.opcode = K::number;
      break;
    }
    case O::variable: {
      auto const* variable = std::get_if<Variable>(first);
      if (!variable || static_cast<unsigned>(variable->get()) >= Letter::count_)
        return {};
      operation.opcode = K::location;
      operation.first = static_cast<std::uint8_t>(static_cast<unsigned>(variable->get()));
      plan.variables_.set(operation.first);
      break;
    }
    case O::load:
      operation.opcode = K::load;
      break;
    case O::store:
      operation.opcode = K::store;
      break;
    case O::add:
      operation.opcode = K::add;
      break;
    case O::less_than:
      operation.opcode = K::less;
      break;
    case O::branch:
      operation.opcode = K::branch;
      break;
    case O::branch_zero:
      operation.opcode = K::branch_zero;
      break;
    default:
      return {};
    }
    if (instruction.has_op2()) {
      try {
        static_cast<void>(instruction.op2());
      }
      catch (std::logic_error const&) {
        return {};
      }
    }
  }

  bool backedge = false;
  for (unsigned pc = 0; pc < plan.size_; ++pc) {
    auto const& instruction = code[pc];
    auto& operation = plan.code_[pc];
    auto target = [&](I::Operand const& operand, bool exit_allowed) -> std::optional<unsigned> {
      auto const* offset = std::get_if<I::Offset>(&operand);
      if (!offset)
        return {};
      I::Index destination{};
      if (*offset < 0) {
        // Offset's minimum is representable here: never negate it directly.
        auto const magnitude = static_cast<I::Index>(-(*offset + 1)) + 1;
        if (magnitude > pc)
          return {};
        destination = pc - magnitude;
      }
      else {
        auto const distance = static_cast<I::Index>(*offset);
        if (distance > plan.size_ - pc)
          return {};
        destination = pc + distance;
      }
      if (destination == plan.size_ && !exit_allowed)
        return {};
      return static_cast<unsigned>(destination);
    };
    auto value = [&](I::Operand const& operand) -> std::optional<unsigned> {
      auto const producer = target(operand, false);
      if (!producer)
        return {};
      switch (plan.code_[*producer].opcode) {
      case K::number:
      case K::load:
      case K::add:
      case K::less:
        return producer;
      default:
        return {};
      }
    };
    switch (operation.opcode) {
    case K::number:
    case K::location:
      break;
    case K::load:
    case K::store: {
      auto const location = target(instruction.op1(), false);
      if (!location || plan.code_[*location].opcode != K::location)
        return {};
      operation.first = plan.code_[*location].first;
      if (operation.opcode == K::store) {
        auto const source = value(instruction.op2());
        if (!source)
          return {};
        operation.second = static_cast<std::uint8_t>(*source);
      }
      break;
    }
    case K::add:
    case K::less: {
      auto const first = value(instruction.op1());
      auto const second = value(instruction.op2());
      if (!first || !second)
        return {};
      operation.first = static_cast<std::uint8_t>(*first);
      operation.second = static_cast<std::uint8_t>(*second);
      break;
    }
    case K::branch: {
      auto const destination = target(instruction.op1(), true);
      if (!destination)
        return {};
      operation.first = static_cast<std::uint8_t>(*destination);
      backedge |= *destination <= pc;
      break;
    }
    case K::branch_zero: {
      auto const source = value(instruction.op1());
      auto const destination = target(instruction.op2(), true);
      if (!source || !destination || *destination <= pc)
        return {};
      operation.first = static_cast<std::uint8_t>(*source);
      operation.second = static_cast<std::uint8_t>(*destination);
      break;
    }
    }
  }
  if (!backedge || plan.variables_.none())
    return {};
  try {
    // Schemas, producer kinds and bounds are now safe for the ordinary graph
    // proof. Reject skipped producers and reads relying on earlier iterations.
    if (verify_control_flow(code, false))
      return {};
  }
  catch (std::bad_alloc const&) {
    return {};  // This optional optimization must not require graph storage.
  }
  return plan;
}

auto PortableScalarPlan::run(ScalarInterruptCheck interrupted, std::uint64_t fuel) const noexcept
  -> std::optional<ScalarValues>
{
  auto const call = [](void const* context) noexcept {
    return (*static_cast<ScalarInterruptCheck const*>(context))();
  };
  return run_with_context(interrupted ? +call : nullptr, &interrupted, fuel);
}

auto PortableScalarPlan::run_with_context(ScalarContextInterruptCheck interrupted,
                                          void const* context, std::uint64_t fuel) const noexcept
  -> std::optional<ScalarValues>
{
  if (interrupted && interrupted(context))
    return {};
  if (fuel > backedge_limit)
    fuel = backedge_limit;
  ScalarValues variables{};
  std::array<std::uint64_t, instruction_limit> results{};
  unsigned until_poll = 1024;
  unsigned pc = 0;
  while (pc < size_) {
    auto const& instruction = code_[pc];
    switch (instruction.opcode) {
    case Op::number:
      results[pc] = instruction.value;
      break;
    case Op::location:
      break;
    case Op::load:
      results[pc] = variables[instruction.first];
      break;
    case Op::store:
      variables[instruction.first] = results[instruction.second];
      break;
    case Op::add: {
      auto const first = results[instruction.first];
      auto const second = results[instruction.second];
      if (first > std::numeric_limits<std::uint64_t>::max() - second)
        return {};
      results[pc] = first + second;
      break;
    }
    case Op::less:
      results[pc] = results[instruction.first] < results[instruction.second] ? 1 : 0;
      break;
    case Op::branch:
      if (instruction.first <= pc) {
        if (fuel == 0)
          return {};
        --fuel;
        if (--until_poll == 0) {
          if (interrupted && interrupted(context))
            return {};
          until_poll = 1024;
        }
      }
      pc = instruction.first;
      continue;
    case Op::branch_zero:
      if (results[instruction.first] == 0) {
        pc = instruction.second;
        continue;
      }
      break;
    }
    ++pc;
  }
  if (interrupted && interrupted(context))
    return {};
  return variables;
}
}  // namespace GD::Bc::Details
