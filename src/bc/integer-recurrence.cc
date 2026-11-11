/** Recognize and evaluate bounded pure integer recurrences.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "integer-recurrence.hh"

#include <array>
#include <utility>

namespace GD::Bc::Details {
namespace {
using O = Instruction::Opcode;
using D = Instruction::Offset;
constexpr std::size_t limit = 132;
enum class Kind {
  invalid,
  location,
  parameter,
  constant,
  decrement,
  recursive,
  sum,
  product,
  test
};
struct Value
{
  Kind kind = Kind::invalid;
  int value = 0;
  bool base_on_true = false;
};

struct Path
{
  std::array<Value, limit> values{};
  bool mark = false;
  bool called = false;
  int step = 0;

  auto operand(Instruction::Operand const& input, std::size_t i) const -> Value
  {
    auto offset = std::get_if<D>(&input);
    if (offset == nullptr || *offset >= 0 || *offset < -static_cast<D>(i - 4))
      return {};
    return values[static_cast<std::size_t>(static_cast<D>(i) + *offset)];
  }

  auto append(Instruction const& instruction, std::size_t i, Variable parameter, Letter self)
    -> bool
  {
    auto left = instruction.has_op1() ? operand(instruction.op1(), i) : Value{};
    auto right = instruction.has_op2() ? operand(instruction.op2(), i) : Value{};
    auto& output = values[i];
    switch (instruction.opcode()) {
    case O::variable: {
      auto variable = std::get_if<Variable>(&instruction.op1());
      if (variable == nullptr || *variable != parameter)
        return false;
      output.kind = Kind::location;
      return true;
    }
    case O::load:
      if (left.kind != Kind::location)
        return false;
      output.kind = Kind::parameter;
      return true;
    case O::number: {
      auto literal = std::get_if<std::string>(&instruction.op1());
      if (literal == nullptr || literal->size() != 1 || (*literal)[0] < '0' || (*literal)[0] > '9')
        return false;
      output = {Kind::constant, (*literal)[0] - '0'};
      return true;
    }
    case O::negate:
      if (left.kind != Kind::constant)
        return false;
      output = {Kind::constant, -left.value};
      return true;
    case O::subtract:
      if (left.kind != Kind::parameter || right.kind != Kind::constant || right.value <= 0)
        return false;
      output = {Kind::decrement, right.value};
      return true;
    case O::less_than:
    case O::less_than_equals: {
      auto const inclusive = instruction.opcode() == O::less_than_equals;
      if (left.kind == Kind::parameter && right.kind == Kind::constant)
        output = {Kind::test, right.value - (inclusive ? 0 : 1), true};
      else if (left.kind == Kind::constant && right.kind == Kind::parameter)
        output = {Kind::test, left.value - (inclusive ? 1 : 0), false};
      else
        return false;
      return true;
    }
    case O::push_param_mark:
      if (mark || called)
        return false;
      mark = true;
      return true;
    case O::push_param:
      if (!mark || step != 0 || left.kind != Kind::decrement)
        return false;
      step = left.value;
      return true;
    case O::call: {
      auto callee = std::get_if<Letter>(&instruction.op1());
      if (!mark || step == 0 || called || callee == nullptr || *callee != self)
        return false;
      mark = false;
      called = true;
      output = {Kind::recursive, step};
      return true;
    }
    case O::add:
    case O::multiply:
      if (left.kind == Kind::recursive && right.kind == Kind::parameter)
        std::swap(left, right);
      if (left.kind != Kind::parameter || right.kind != Kind::recursive)
        return false;
      output = {instruction.opcode() == O::add ? Kind::sum : Kind::product, right.value};
      return true;
    default:
      return false;
    }
  }
};

auto returned(std::span<Instruction const> code, std::size_t start, Path path, Variable parameter,
              Letter self) -> Value
{
  for (auto i = start; i < code.size(); ++i) {
    if (code[i].opcode() == O::return_) {
      auto result = path.operand(code[i].op1(), i);
      if (path.mark)
        return {};
      if (!path.called && (result.kind == Kind::constant || result.kind == Kind::parameter))
        return result;
      if (path.called && (result.kind == Kind::sum || result.kind == Kind::product))
        return result;
      return {};
    }
    if (!path.append(code[i], i, parameter, self))
      return {};
  }
  return {};
}

auto decimal_digits(std::uint64_t value) noexcept -> std::uint64_t
{
  std::uint64_t digits = 1;
  while (value >= 10) {
    value /= 10;
    ++digits;
  }
  return digits;
}
}  // namespace

auto IntegerRecurrence::match(std::span<Instruction const> code, VariableMask locals, Letter self)
  -> std::optional<IntegerRecurrence>
{
  if (code.size() <= 4 || code.size() > limit || code[0].opcode() != O::pop_param ||
      code[1].opcode() != O::variable || code[2].opcode() != O::store ||
      code[3].opcode() != O::pop_param_mark)
    return std::nullopt;
  auto parameter = std::get_if<Variable>(&code[1].op1());
  auto destination = std::get_if<D>(&code[2].op1());
  auto input = std::get_if<D>(&code[2].op2());
  if (parameter == nullptr || !locals.contains(*parameter) || destination == nullptr ||
      input == nullptr || *destination != -1 || *input != -2)
    return std::nullopt;
  Path prefix;
  for (std::size_t i = 4; i < code.size(); ++i) {
    if (code[i].opcode() == O::branch_zero) {
      auto condition = prefix.operand(code[i].op1(), i);
      auto target = std::get_if<D>(&code[i].op2());
      if (prefix.mark || prefix.called || condition.kind != Kind::test || target == nullptr ||
          *target <= 0 || static_cast<std::size_t>(*target) >= code.size() - i)
        return std::nullopt;
      auto yes = returned(code, i + 1, prefix, *parameter, self);
      auto no = returned(code, i + static_cast<std::size_t>(*target), prefix, *parameter, self);
      auto base_value = condition.base_on_true ? yes : no;
      auto reduction = condition.base_on_true ? no : yes;
      if ((base_value.kind != Kind::constant && base_value.kind != Kind::parameter) ||
          (reduction.kind != Kind::sum && reduction.kind != Kind::product))
        return std::nullopt;
      return IntegerRecurrence{self,
                               condition.value,
                               base_value.value,
                               static_cast<unsigned>(reduction.value),
                               base_value.kind == Kind::parameter,
                               reduction.kind == Kind::product};
    }
    if (!prefix.append(code[i], i, *parameter, self) || prefix.mark || prefix.called)
      return std::nullopt;
  }
  return std::nullopt;
}

auto IntegerRecurrence::admits(std::uint64_t argument) const noexcept -> bool
{
  if (argument > static_cast<std::uint64_t>(Number::base_ - 1) || step == 0)
    return false;
  auto distance = static_cast<std::int64_t>(argument) - stop;
  auto steps = distance > 0 ? (static_cast<std::uint64_t>(distance) + step - 1) / step : 0;
  if (steps > 65536)
    return false;
  auto digits = multiply ? steps * decimal_digits(argument) + 2
                         : decimal_digits(argument) + decimal_digits(steps) + 2;
  // Bound total decimal-digit work before allocating or consuming arguments.
  // This is a resource bound for the whole family, not a factorial cutoff.
  return steps * digits <= 4 * 1024 * 1024;
}

auto IntegerRecurrence::execute(std::uint64_t argument, Number& result, bool (*interrupted)()) const
  -> bool
{
  if (!admits(argument) || interrupted())
    return false;
  auto distance = static_cast<std::int64_t>(argument) - stop;
  auto steps = distance > 0 ? (static_cast<std::uint64_t>(distance) + step - 1) / step : 0;
  auto final_argument =
    static_cast<std::int64_t>(argument) - static_cast<std::int64_t>(steps * step);
  auto initial = parameter_base ? final_argument : base;
  Number value(static_cast<Number::NumType>(initial < 0 ? -initial : initial));
  if (initial < 0)
    value.negate();
  // A negative threshold can leave negative factors before the terminal call.
  // Admitted arguments and single-digit bounds keep this signed range small.
  auto current = static_cast<std::int64_t>(argument);
  for (std::uint64_t i = 0; i < steps; ++i) {
    if (interrupted())
      return false;
    auto magnitude = static_cast<std::uint64_t>(current < 0 ? -current : current);
    auto factor = Number::from_unsigned_word(magnitude);
    if (current < 0)
      factor.negate();
    if (multiply)
      value.multiply(factor, 0);
    else
      value.add(factor);
    current -= step;
  }
  if (interrupted())
    return false;
  result = std::move(value);
  return true;
}
}  // namespace GD::Bc::Details
