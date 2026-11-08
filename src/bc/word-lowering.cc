/** Purity and source correspondence stay in the ordinary VM compiler context.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "word-lowering.hh"

namespace GD::Bc::Details {
auto lower_word_function(Instructions const& source, VariableMask locals, Letter self)
  -> std::optional<Word::Program>
{
  using O = Instruction::Opcode;
  using D = Instruction::Offset;
  using W = Word::Opcode;
  constexpr std::size_t body = 4;
  if (source.size() <= body || source.size() > body + Word::instruction_limit ||
      source[0].opcode() != O::pop_param || source[1].opcode() != O::variable ||
      source[2].opcode() != O::store || source[3].opcode() != O::pop_param_mark ||
      std::get<D>(source[2].op1()) != -1 || std::get<D>(source[2].op2()) != -2)
    return {};
  auto parameter = std::get<Variable>(source[1].op1());
  if (!locals.contains(parameter))
    return {};
  bool arrays = false;
  locals.for_each_array([&](Letter) { arrays = true; });
  if (arrays)
    return {};

  Word::Storage<Word::Instruction, Profile::AllocationKind::execution_plans> code;
  code.reserve(source.size() - body);
  for (std::size_t pc = body; pc != source.size(); ++pc) {
    auto const& input = source[pc];
    Word::Instruction output{W::location};
    auto relative = [&](bool second, bool branch = false) -> std::optional<std::uint16_t> {
      auto const& operand = second ? input.op2() : input.op1();
      auto const* offset = std::get_if<D>(&operand);
      if (!offset)
        return {};
      // Compare before adding: even a hostile signed offset cannot overflow.
      auto const lower = static_cast<D>(body) - static_cast<D>(pc);
      auto const upper = static_cast<D>(source.size() - 1) - static_cast<D>(pc);
      if (*offset < lower || *offset > upper || (!branch && *offset >= 0))
        return {};
      auto target = static_cast<std::size_t>(static_cast<D>(pc) + *offset);
      if (!branch && source[target].opcode() == O::variable) {
        auto const variable = std::get<Variable>(source[target].op1());
        if (!locals.contains(variable))
          return {};
        return static_cast<std::uint16_t>(Word::local_tag | static_cast<unsigned>(variable.get()));
      }
      return static_cast<std::uint16_t>(target - body);
    };
    auto operand = [&](bool second, bool branch = false) {
      auto index = relative(second, branch);
      if (!index)
        return false;
      (second ? output.right : output.left) = *index;
      return true;
    };
    switch (input.opcode()) {
    case O::variable:
      if (!locals.contains(std::get<Variable>(input.op1())))
        return {};
      break;
    case O::number: {
      auto const& text = std::get<std::string>(input.op1());
      // These decimal digits have identical values at every runtime ibase.
      if (text.size() != 1 || text[0] < '0' || text[0] > '9')
        return {};
      output = {W::constant, static_cast<std::uint16_t>(text[0] - '0')};
      break;
    }
    case O::load:
    case O::negate:
    case O::return_:
    case O::push_param:
      output.opcode = input.opcode() == O::load      ? W::load
                      : input.opcode() == O::negate  ? W::negate
                      : input.opcode() == O::return_ ? W::return_
                                                     : W::push;
      if (!operand(false))
        return {};
      break;
    case O::store:
      output.opcode = W::store;
      if (!operand(false) || !operand(true) || (output.left & Word::local_tag) == 0)
        return {};
      break;
    case O::add:
    case O::subtract:
    case O::multiply:
    case O::less_than:
    case O::less_than_equals:
    case O::equals:
    case O::not_equals:
      switch (input.opcode()) {
      case O::add:
        output.opcode = W::add;
        break;
      case O::subtract:
        output.opcode = W::subtract;
        break;
      case O::multiply:
        output.opcode = W::multiply;
        break;
      case O::less_than:
        output.opcode = W::less;
        break;
      case O::less_than_equals:
        output.opcode = W::less_equal;
        break;
      case O::equals:
        output.opcode = W::equal;
        break;
      default:
        output.opcode = W::not_equal;
        break;
      }
      if (!operand(false) || !operand(true))
        return {};
      break;
    case O::branch:
      output.opcode = W::branch;
      if (!operand(false, true))
        return {};
      break;
    case O::branch_zero:
      output.opcode = W::branch_zero;
      if (!operand(false) || !operand(true, true))
        return {};
      break;
    case O::push_param_mark:
      output.opcode = W::mark;
      break;
    case O::call:
      if (std::get<Letter>(input.op1()) != self)
        return {};
      output.opcode = W::call;
      break;
    default:
      // Includes all special registers, arrays, external effects, additional
      // parameters and arithmetic whose scale/rounding needs Number semantics.
      return {};
    }
    code.push_back(output);
  }
  return Word::Program::prepare(code, static_cast<unsigned>(parameter.get()));
}
}  // namespace GD::Bc::Details
