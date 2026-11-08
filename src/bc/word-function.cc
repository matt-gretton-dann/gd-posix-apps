/** Checked word arithmetic and explicit continuations, without Number types.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "word-function.hh"

#include <array>
#include <bitset>
#include <limits>
#include <utility>

// This translation unit is deliberately outside the Number compiler context.
// Keep the dependency guard active even when native object tools are absent.
#if defined(SRC_BC_BC_HH_INCLUDED) || defined(SRC_INCLUDE_UTIL_NUMBER_HH_INCLUDED) ||              \
  defined(GD_BC_DIGIT_STORAGE_HH)
#  error "The word executor must not include Number, Digits or VM declarations"
#endif

namespace GD::Bc::Details::Word {
namespace {
using O = Opcode;
using V = std::int64_t;
auto produces(O op) noexcept -> bool
{
  return op == O::constant || op == O::load || op == O::negate || op == O::add ||
         op == O::subtract || op == O::multiply || op == O::less || op == O::less_equal ||
         op == O::equal || op == O::not_equal || op == O::call;
}

auto checked_binary(O op, V a, V b) noexcept -> std::optional<V>
{
  constexpr auto min = std::numeric_limits<V>::min();
  constexpr auto max = std::numeric_limits<V>::max();
  switch (op) {
  case O::add:
    if ((b > 0 && a > max - b) || (b < 0 && a < min - b))
      return {};
    return a + b;
  case O::subtract:
    if ((b < 0 && a > max + b) || (b > 0 && a < min + b))
      return {};
    return a - b;
  case O::multiply:
    if (a > 0 ? (b > 0 ? a > max / b : b < min / a)
              : (a < 0 && (b > 0 ? a < min / b : b < 0 && a < max / b)))
      return {};
    return a * b;
  case O::less:
    return a < b;
  case O::less_equal:
    return a <= b;
  case O::equal:
    return a == b;
  case O::not_equal:
    return a != b;
  default:
    return {};
  }
}
}  // namespace

auto Program::prepare(std::span<Instruction const> code, unsigned parameter)
  -> std::optional<Program>
{
  auto const size = code.size();
  if (size == 0 || size > instruction_limit || parameter >= local_count)
    return {};
  auto operand = [&](std::uint16_t value, std::size_t pc) {
    if ((value & local_tag) != 0)
      return (value & ~local_tag) < local_count;
    return value < pc && produces(code[value].opcode);
  };
  for (std::size_t pc = 0; pc != size; ++pc) {
    auto const& i = code[pc];
    switch (i.opcode) {
    case O::location:
    case O::mark:
    case O::call:
      if (i.left != 0 || i.right != 0)
        return {};
      break;
    case O::constant:
      if (i.left > 9 || i.right != 0)
        return {};
      break;
    case O::load:
    case O::negate:
    case O::push:
    case O::return_:
      if (!operand(i.left, pc) || i.right != 0)
        return {};
      break;
    case O::store:
      if ((i.left & local_tag) == 0 || !operand(i.left, pc) || !operand(i.right, pc))
        return {};
      break;
    case O::add:
    case O::subtract:
    case O::multiply:
    case O::less:
    case O::less_equal:
    case O::equal:
    case O::not_equal:
      if (!operand(i.left, pc) || !operand(i.right, pc))
        return {};
      break;
    case O::branch:
      if (i.left >= size || i.right != 0)
        return {};
      break;
    case O::branch_zero:
      if (!operand(i.left, pc) || i.right >= size)
        return {};
      break;
    default:
      return {};
    }
    if (i.opcode != O::branch && i.opcode != O::return_ && pc + 1 == size)
      return {};
  }

  // A decreasing intersection reaches a fixed point. The entry always has
  // an empty producer set, including when a backedge targets instruction 0.
  using Bits = std::bitset<instruction_limit>;
  Storage<Bits, Profile::AllocationKind::execution_plans> available(size);
  Storage<bool, Profile::AllocationKind::execution_plans> reached(size, false);
  Storage<std::vector<bool>, Profile::AllocationKind::execution_plans> marks(size);
  reached[0] = true;
  bool changed = true;
  while (changed) {
    changed = false;
    for (std::size_t pc = 0; pc != size; ++pc) {
      if (!reached[pc])
        continue;
      auto bits = available[pc];
      auto stack = marks[pc];
      auto const& i = code[pc];
      if (i.opcode == O::mark) {
        if (stack.size() >= instruction_limit)
          return {};
        stack.push_back(false);
      }
      if (i.opcode == O::push) {
        if (stack.empty() || stack.back())
          return {};
        stack.back() = true;
      }
      if (i.opcode == O::call) {
        if (stack.empty() || !stack.back())
          return {};
        stack.pop_back();
      }
      if (i.opcode == O::return_) {
        if (!stack.empty())
          return {};
        continue;
      }
      if (produces(i.opcode))
        bits.set(pc);
      auto join = [&](std::size_t target) {
        if (!reached[target]) {
          reached[target] = true;
          available[target] = bits;
          marks[target] = stack;
          changed = true;
          return true;
        }
        if (marks[target] != stack)
          return false;
        auto common = available[target] & bits;
        if (common != available[target]) {
          available[target] = common;
          changed = true;
        }
        return true;
      };
      if (i.opcode == O::branch) {
        if (!join(i.left))
          return {};
      }
      else {
        if (!join(pc + 1))
          return {};
        if (i.opcode == O::branch_zero && !join(i.right))
          return {};
      }
    }
  }
  // Bounds/kinds were checked even for unreachable instructions. Availability
  // is a property of reachable consumers and must hold on every incoming edge.
  for (std::size_t pc = 0; pc != size; ++pc) {
    if (!reached[pc])
      continue;
    auto const& i = code[pc];
    auto ready = [&](std::uint16_t value) {
      return (value & local_tag) != 0 || available[pc].test(value);
    };
    switch (i.opcode) {
    case O::load:
    case O::negate:
    case O::push:
    case O::return_:
    case O::branch_zero:
      if (!ready(i.left))
        return {};
      break;
    case O::store:
    case O::add:
    case O::subtract:
    case O::multiply:
    case O::less:
    case O::less_equal:
    case O::equal:
    case O::not_equal:
      if (!ready(i.left) || !ready(i.right))
        return {};
      break;
    default:
      break;
    }
  }
  // Validation above still uses the decoded letter namespace, including
  // unreachable instructions. Rename only the owned, validated copy so a
  // sparse source name does not force unused locals into every recursive frame.
  std::bitset<local_count> used_locals;
  used_locals.set(parameter);
  auto use_local = [&](std::uint16_t value) {
    if ((value & local_tag) != 0)
      used_locals.set(value & ~local_tag);
  };
  for (auto const& i : code) {
    use_local(i.left);
    use_local(i.right);
  }
  std::array<unsigned, local_count> local_indices{};
  unsigned slots = 0;
  for (unsigned name = 0; name != local_count; ++name) {
    if (used_locals.test(name))
      local_indices[name] = slots++;
  }
  Program result(code, local_indices[parameter]);
  result.local_slots_ = slots;
  auto rename_local = [&](std::uint16_t& value) {
    if ((value & local_tag) != 0)
      value = static_cast<std::uint16_t>(local_tag | local_indices[value & ~local_tag]);
  };
  for (auto& i : result.code_) {
    rename_local(i.left);
    rename_local(i.right);
  }
  return result;
}

void Scratch::clear() noexcept
{
  values.clear();
  frames.clear();
  arguments.clear();
}

auto execute(Program const& program, V argument, Scratch& scratch, Callbacks const& callbacks,
             Counters& counters, std::size_t budget, std::size_t fuel) -> std::optional<Value>
{
  scratch.clear();
  struct Clear
  {
    Scratch& scratch;
    ~Clear() { scratch.clear(); }
  } clear{scratch};
  if (budget > scratch_limit || fuel > fuel_limit ||
      (callbacks.interrupted && callbacks.interrupted(callbacks.context)))
    return {};
  auto& values = scratch.values;
  auto& frames = scratch.frames;
  auto& arguments = scratch.arguments;
  auto code = program.instructions();
  // A moved-from owner is valid C++, but no longer names an executable plan.
  if (code.empty())
    return {};
  auto const locals = program.local_slots();
  auto const stride = locals + code.size();
  auto used = [&]() {
    return values.size() * sizeof(Value) + frames.size() * sizeof(Scratch::Frame) +
           arguments.size() * sizeof(Scratch::Argument);
  };
  auto remaining = [&]() {
    auto const bank = callbacks.spill_bytes ? callbacks.spill_bytes(callbacks.context) : 0;
    auto const live = used();
    return live <= budget && bank <= budget - live ? budget - live - bank : std::size_t(0);
  };
  auto bank_fits = [&]() {
    auto const bank = callbacks.spill_bytes ? callbacks.spill_bytes(callbacks.context) : 0;
    return used() <= budget && bank <= budget - used();
  };
  auto enter = [&](Value value) {
    if (stride * sizeof(Value) + sizeof(Scratch::Frame) > remaining())
      return false;
    auto const base = values.size();
    values.resize(base + stride);
    frames.push_back({0, arguments.size()});
    values[base + program.parameter()] = value;
    return true;
  };
  if (!enter({argument, false}))
    return {};
  while (!frames.empty()) {
    if (fuel == 0 || (callbacks.interrupted && callbacks.interrupted(callbacks.context)))
      return {};
    --fuel;
    auto& frame = frames.back();
    auto const pc = frame.pc++;
    auto const base = (frames.size() - 1) * stride;
    auto const& i = code[pc];
    auto read = [&](std::uint16_t operand) -> Value {
      return values[base + ((operand & local_tag) != 0 ? operand & ~local_tag : locals + operand)];
    };
    auto& destination = values[base + locals + pc];
    ++counters.instructions;
    switch (i.opcode) {
    case O::location:
      break;
    case O::constant:
      destination = {V(i.left), false};
      break;
    case O::load:
      destination = read(i.left);
      break;
    case O::store:
      values[base + (i.left & ~local_tag)] = read(i.right);
      break;
    case O::negate: {
      auto const input = read(i.left);
      if (!input.big && input.payload != std::numeric_limits<V>::min())
        destination = {-input.payload, false};
      else if (!callbacks.spill_bytes || !callbacks.negate ||
               !callbacks.negate(callbacks.context, input, destination, remaining()))
        return {};
      if (!bank_fits())
        return {};
      break;
    }
    case O::add:
    case O::subtract:
    case O::multiply:
    case O::less:
    case O::less_equal:
    case O::equal:
    case O::not_equal: {
      auto const left = read(i.left), right = read(i.right);
      if (!left.big && !right.big) {
        if (auto small = checked_binary(i.opcode, left.payload, right.payload)) {
          destination = {*small, false};
          break;
        }
      }
      if (!callbacks.spill_bytes || !callbacks.binary ||
          !callbacks.binary(callbacks.context, i.opcode, left, right, destination, remaining()))
        return {};
      if (!bank_fits())
        return {};
      break;
    }
    case O::branch:
      frame.pc = i.left;
      break;
    case O::branch_zero:
      if (auto value = read(i.left); !value.big && value.payload == 0)
        frame.pc = i.right;
      break;
    case O::mark:
      if (sizeof(Scratch::Argument) > remaining())
        return {};
      arguments.emplace_back();
      break;
    case O::push:
      arguments.back() = {read(i.left), true};
      break;
    case O::call: {
      auto const value = arguments.back().value;
      arguments.pop_back();
      ++counters.self_calls;
      if (!enter(value))
        return {};
      break;
    }
    case O::return_: {
      auto const value = read(i.left);
      frames.pop_back();
      values.resize(base);
      if (frames.empty())
        return value;
      values[(frames.size() - 1) * stride + locals + frames.back().pc - 1] = value;
      break;
    }
    }
  }
  return {};
}
}  // namespace GD::Bc::Details::Word
