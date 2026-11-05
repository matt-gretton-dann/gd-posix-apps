/** Portable checked integer execution for pure verified functions.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "integer-function.hh"

#include <limits>

namespace GD::Bc::Details {
namespace {
using O = Instruction::Opcode;
using D = Instruction::Offset;
using V = std::int64_t;
constexpr std::size_t locals_size = Letter::count_;
constexpr std::uint16_t local_tag = 0x8000;
constexpr std::size_t scratch_limit = 1024 * 1024;

auto checked_binary(O op, V a, V b) -> std::optional<V>
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
  case O::less_than:
    return a < b;
  case O::less_than_equals:
    return a <= b;
  case O::equals:
    return a == b;
  case O::not_equals:
    return a != b;
  default:
    return {};
  }
}
}  // namespace

auto plan_integer_function(Instructions const& source, VariableMask locals, Letter self)
  -> std::optional<IntegerFunction>
{
  // Bound preparation and every frame independently of hostile source size.
  if (source.size() <= 4 || source.size() > 1024 || source[0].opcode() != O::pop_param ||
      source[1].opcode() != O::variable || source[2].opcode() != O::store ||
      source[3].opcode() != O::pop_param_mark || std::get<D>(source[2].op1()) != -1 ||
      std::get<D>(source[2].op2()) != -2)
    return {};
  auto const parameter = std::get<Variable>(source[1].op1());
  if (!locals.contains(parameter))
    return {};
  bool arrays = false;
  locals.for_each_array([&](Letter) { arrays = true; });
  if (arrays)
    return {};

  IntegerFunction result{{}, static_cast<unsigned>(parameter.get())};
  result.code.resize(source.size(), {O::variable, 0, 0});
  for (std::size_t pc = 4; pc < source.size(); ++pc) {
    auto const& instruction = source[pc];
    auto& out = result.code[pc];
    out.opcode = instruction.opcode();
    auto relative = [&](bool second, bool branch = false) -> std::optional<std::uint16_t> {
      auto const* offset = std::get_if<D>(second ? &instruction.op2() : &instruction.op1());
      if (offset == nullptr)
        return {};
      auto const index = static_cast<D>(pc) + *offset;
      if (index < 4 || static_cast<std::size_t>(index) >= source.size())
        return {};
      auto const target = static_cast<std::size_t>(index);
      if (!branch && target >= pc)
        return {};
      if (!branch && source[target].opcode() == O::variable) {
        auto const variable = std::get<Variable>(source[target].op1());
        if (!locals.contains(variable))
          return {};
        return static_cast<std::uint16_t>(local_tag | static_cast<unsigned>(variable.get()));
      }
      return static_cast<std::uint16_t>(target);
    };
    auto operand = [&](bool second, bool branch = false) {
      auto const index = relative(second, branch);
      if (!index)
        return false;
      (second ? out.right : out.left) = *index;
      return true;
    };
    switch (out.opcode) {
    case O::variable:
      if (!locals.contains(std::get<Variable>(instruction.op1())))
        return {};
      break;
    case O::number: {
      auto const& text = std::get<std::string>(instruction.op1());
      // One decimal digit has the same value at every bc input base.
      if (text.size() != 1 || text[0] < '0' || text[0] > '9')
        return {};
      out.left = static_cast<std::uint16_t>(text[0] - '0');
      break;
    }
    case O::load:
    case O::negate:
    case O::return_:
    case O::push_param:
      if (!operand(false))
        return {};
      break;
    case O::store:
      if (!operand(false) || !operand(true) || (out.left & local_tag) == 0)
        return {};
      break;
    case O::add:
    case O::subtract:
    case O::multiply:
    case O::less_than:
    case O::less_than_equals:
    case O::equals:
    case O::not_equals:
      if (!operand(false) || !operand(true))
        return {};
      break;
    case O::branch:
      if (!operand(false, true))
        return {};
      break;
    case O::branch_zero:
      if (!operand(false) || !operand(true, true))
        return {};
      break;
    case O::call:
      if (!(std::get<Letter>(instruction.op1()) == self))
        return {};
      break;
    case O::push_param_mark:
      break;
    default:
      return {};
    }
  }
  return result;
}

auto run_integer_function(IntegerFunction const& function, V argument,
                          IntegerFunctionScratch& scratch, bool (*interrupted)() noexcept)
  -> std::optional<V>
{
  auto& values = scratch.values;
  auto& frames = scratch.frames;
  auto& arguments = scratch.arguments;
  values.clear();
  frames.clear();
  arguments.clear();
  struct Clear
  {
    IntegerFunctionScratch& scratch;
    ~Clear()
    {
      scratch.values.clear();
      scratch.frames.clear();
      scratch.arguments.clear();
    }
  } clear{scratch};
  auto const stride = locals_size + function.code.size();
  auto const max_depth =
    scratch_limit / (stride * sizeof(V) + sizeof(IntegerFunctionScratch::Frame));
  auto enter = [&](V value) {
    if (frames.size() >= max_depth ||
        (frames.size() + 1) * (stride * sizeof(V) + sizeof(IntegerFunctionScratch::Frame)) +
            arguments.size() * sizeof(IntegerFunctionScratch::Argument) >
          scratch_limit)
      return false;
    auto const base = values.size();
    values.resize(base + stride, 0);
    values[base + function.parameter] = value;
    frames.push_back({4, arguments.size()});
    return true;
  };
  if (!enter(argument))
    return {};
  while (!frames.empty()) {
    if (interrupted())
      return {};
    auto& frame = frames.back();
    if (frame.pc >= function.code.size())
      return {};  // Falling off a function belongs to the generic interpreter.
    auto const pc = frame.pc;
    auto const base = (frames.size() - 1) * stride;
    auto const& instruction = function.code[pc];
    auto read = [&](std::uint16_t operand) {
      return values[base +
                    ((operand & local_tag) != 0 ? operand & ~local_tag : locals_size + operand)];
    };
    auto& destination = values[base + locals_size + pc];
    ++frame.pc;
    switch (instruction.opcode) {
    case O::variable:
      break;
    case O::number:
      destination = instruction.left;
      break;
    case O::load:
      destination = read(instruction.left);
      break;
    case O::store:
      values[base + (instruction.left & ~local_tag)] = read(instruction.right);
      break;
    case O::negate:
      if (read(instruction.left) == std::numeric_limits<V>::min())
        return {};
      destination = -read(instruction.left);
      break;
    case O::add:
    case O::subtract:
    case O::multiply:
    case O::less_than:
    case O::less_than_equals:
    case O::equals:
    case O::not_equals: {
      auto const value =
        checked_binary(instruction.opcode, read(instruction.left), read(instruction.right));
      if (!value)
        return {};
      destination = *value;
      break;
    }
    case O::branch:
      frame.pc = instruction.left;
      break;
    case O::branch_zero:
      if (read(instruction.left) == 0)
        frame.pc = instruction.right;
      break;
    case O::push_param_mark:
      if (frames.size() * (stride * sizeof(V) + sizeof(IntegerFunctionScratch::Frame)) +
            (arguments.size() + 1) * sizeof(IntegerFunctionScratch::Argument) >
          scratch_limit)
        return {};
      arguments.emplace_back();
      break;
    case O::push_param:
      if (arguments.size() <= frame.marks || arguments.back().present)
        return {};
      arguments.back() = {read(instruction.left), true};
      break;
    case O::call: {
      if (arguments.size() <= frame.marks || !arguments.back().present)
        return {};
      auto const value = arguments.back().value;
      arguments.pop_back();
      if (!enter(value))
        return {};
      break;
    }
    case O::return_: {
      auto const value = read(instruction.left);
      if (arguments.size() != frame.marks)
        return {};
      frames.pop_back();
      values.resize(base);
      if (frames.empty())
        return value;
      values[(frames.size() - 1) * stride + locals_size + frames.back().pc - 1] = value;
      break;
    }
    default:
      return {};
    }
  }
  return {};
}
}  // namespace GD::Bc::Details
