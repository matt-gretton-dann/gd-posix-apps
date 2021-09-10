/** \file   instruction.cc
 *  \brief  bc VM instructions
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include "gd/stdlib.h"

#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <variant>
#include <vector>

#include "bc.hh"

GD::Bc::VariableMask::VariableMask() = default;

GD::Bc::VariableMask::VariableMask(std::string_view vars, std::string_view arrays)
{
  for (auto l : vars) {
    add(Variable(l));
  }
  for (auto l : arrays) {
    add(Array(l));
  }
}

void GD::Bc::VariableMask::add(Variable v)
{
  variable_mask_ |= 1 << static_cast<unsigned>(v.get());
}

void GD::Bc::VariableMask::add(Array a) { array_mask_ |= 1 << static_cast<unsigned>(a.get()); }

auto GD::Bc::VariableMask::contains(Variable v) const -> bool
{
  return (variable_mask_ & (1 << static_cast<unsigned>(v.get()))) != 0;
}

auto GD::Bc::VariableMask::contains(Array a) const -> bool
{
  return (array_mask_ & (1 << static_cast<unsigned>(a.get()))) != 0;
}

auto GD::Bc::VariableMask::operator==(VariableMask rhs) const -> bool
{
  return variable_mask_ == rhs.variable_mask_ && array_mask_ == rhs.array_mask_;
}

auto GD::Bc::operator!=(VariableMask lhs, VariableMask rhs) -> bool { return !(lhs == rhs); }

auto GD::Bc::operator<<(std::ostream& os, GD::Bc::VariableMask mask) -> std::ostream&
{
  bool need_comma = false;
  mask.for_each_variable([&need_comma, &os](Letter letter) {
    os << (need_comma ? ", " : "") << letter;
    need_comma = true;
  });
  mask.for_each_array([&need_comma, &os](Letter letter) {
    os << (need_comma ? ", " : "") << letter << "[]";
    need_comma = true;
  });

  return os;
}

GD::Bc::Instruction::Instruction(Opcode opcode) : opcode_(opcode)
{
  assert(op_count(opcode) == 0);  // NOLINT
  validate_operands();
}

GD::Bc::Instruction::Instruction(Opcode opcode, Operand const& op1) : opcode_(opcode), op1_(op1)
{
  assert(op_count(opcode) == 1);  // NOLINT
  validate_operands();
}

GD::Bc::Instruction::Instruction(Opcode opcode, Operand const& op1, Operand const& op2)
    : opcode_(opcode), op1_(op1), op2_(op2)
{
  assert(op_count(opcode) == 2);  // NOLINT
  validate_operands();
}

auto GD::Bc::Instruction::opcode() const -> GD::Bc::Instruction::Opcode { return opcode_; }

auto GD::Bc::Instruction::op1() const -> GD::Bc::Instruction::Operand const&
{
  assert(has_op1());  // NOLINT
  return *op1_;
}

void GD::Bc::Instruction::op1(Operand const& operand)
{
  assert(has_op1());  // NOLINT
  op1_ = operand;
  validate_operands();
}

auto GD::Bc::Instruction::op2() const -> GD::Bc::Instruction::Operand const&
{
  assert(has_op2());  // NOLINT
  return *op2_;
}

void GD::Bc::Instruction::op2(Operand const& operand)
{
  assert(has_op2());  // NOLINT
  op2_ = operand;
  validate_operands();
}

auto GD::Bc::Instruction::has_op1() const -> bool { return has_op1(opcode_); }
auto GD::Bc::Instruction::has_op2() const -> bool { return has_op2(opcode_); }

auto GD::Bc::Instruction::has_op1(Opcode opcode) -> bool { return op_count(opcode) >= 1; }

auto GD::Bc::Instruction::has_op2(Opcode opcode) -> bool { return op_count(opcode) >= 2; }

auto GD::Bc::operator<<(std::ostream& os, GD::Bc::Instruction::Stream s) -> std::ostream&
{
  switch (s) {
  case GD::Bc::Instruction::Stream::output:
    os << "output";
    break;
  case GD::Bc::Instruction::Stream::error:
    os << "error";
    break;
  }
  return os;
}

auto GD::Bc::operator<<(std::ostream& os, GD::Bc::Instruction::Opcode opcode) -> std::ostream&
{
  switch (opcode) {
  case GD::Bc::Instruction::Opcode::eof:
    os << "eof";
    break;
  case GD::Bc::Instruction::Opcode::print:
    os << "print";
    break;
  case GD::Bc::Instruction::Opcode::quit:
    os << "quit";
    break;
  case GD::Bc::Instruction::Opcode::string:
    os << "string";
    break;
  case GD::Bc::Instruction::Opcode::number:
    os << "number";
    break;
  case GD::Bc::Instruction::Opcode::variable:
    os << "variable";
    break;
  case GD::Bc::Instruction::Opcode::array_element:
    os << "array_element";
    break;
  case GD::Bc::Instruction::Opcode::array:
    os << "array";
    break;
  case GD::Bc::Instruction::Opcode::scale:
    os << "scale";
    break;
  case GD::Bc::Instruction::Opcode::ibase:
    os << "ibase";
    break;
  case GD::Bc::Instruction::Opcode::obase:
    os << "obase";
    break;
  case GD::Bc::Instruction::Opcode::add:
    os << "add";
    break;
  case GD::Bc::Instruction::Opcode::subtract:
    os << "subtract";
    break;
  case GD::Bc::Instruction::Opcode::negate:
    os << "negate";
    break;
  case GD::Bc::Instruction::Opcode::multiply:
    os << "multiply";
    break;
  case GD::Bc::Instruction::Opcode::divide:
    os << "divide";
    break;
  case GD::Bc::Instruction::Opcode::modulo:
    os << "modulo";
    break;
  case GD::Bc::Instruction::Opcode::power:
    os << "power";
    break;
  case GD::Bc::Instruction::Opcode::load:
    os << "load";
    break;
  case GD::Bc::Instruction::Opcode::store:
    os << "store";
    break;
  case GD::Bc::Instruction::Opcode::scale_expr:
    os << "scale_expr";
    break;
  case GD::Bc::Instruction::Opcode::sqrt:
    os << "sqrt";
    break;
  case GD::Bc::Instruction::Opcode::abs:
    os << "abs";
    break;
  case GD::Bc::Instruction::Opcode::length:
    os << "length";
    break;
  case GD::Bc::Instruction::Opcode::equals:
    os << "equals";
    break;
  case GD::Bc::Instruction::Opcode::less_than_equals:
    os << "less_than_equals";
    break;
  case GD::Bc::Instruction::Opcode::not_equals:
    os << "not_equals";
    break;
  case GD::Bc::Instruction::Opcode::less_than:
    os << "less_than";
    break;
  case GD::Bc::Instruction::Opcode::branch:
    os << "branch";
    break;
  case GD::Bc::Instruction::Opcode::branch_zero:
    os << "branch_zero";
    break;
  case GD::Bc::Instruction::Opcode::return_:
    os << "return";
    break;
  case GD::Bc::Instruction::Opcode::call:
    os << "call";
    break;
  case GD::Bc::Instruction::Opcode::push_param_mark:
    os << "push_param_mark";
    break;
  case GD::Bc::Instruction::Opcode::pop_param_mark:
    os << "pop_param_mark";
    break;
  case GD::Bc::Instruction::Opcode::push_param:
    os << "push_param";
    break;
  case GD::Bc::Instruction::Opcode::pop_param:
    os << "pop_param";
    break;
  case GD::Bc::Instruction::Opcode::pop_param_array:
    os << "pop_param_array";
    break;
  case GD::Bc::Instruction::Opcode::function_begin:
    os << "function_begin";
    break;
  case GD::Bc::Instruction::Opcode::function_end:
    os << "function_end";
    break;
  }
  return os;
}

auto GD::Bc::Instruction::op_count(Opcode opcode) -> unsigned
{
  switch (opcode) {
  case GD::Bc::Instruction::Opcode::eof:
  case GD::Bc::Instruction::Opcode::scale:
  case GD::Bc::Instruction::Opcode::ibase:
  case GD::Bc::Instruction::Opcode::obase:
  case GD::Bc::Instruction::Opcode::push_param_mark:
  case GD::Bc::Instruction::Opcode::pop_param_mark:
  case GD::Bc::Instruction::Opcode::pop_param:
  case GD::Bc::Instruction::Opcode::pop_param_array:
    return 0;
  case GD::Bc::Instruction::Opcode::quit:
  case GD::Bc::Instruction::Opcode::string:
  case GD::Bc::Instruction::Opcode::number:
  case GD::Bc::Instruction::Opcode::variable:
  case GD::Bc::Instruction::Opcode::array:
  case GD::Bc::Instruction::Opcode::negate:
  case GD::Bc::Instruction::Opcode::load:
  case GD::Bc::Instruction::Opcode::scale_expr:
  case GD::Bc::Instruction::Opcode::abs:
  case GD::Bc::Instruction::Opcode::sqrt:
  case GD::Bc::Instruction::Opcode::length:
  case GD::Bc::Instruction::Opcode::branch:
  case GD::Bc::Instruction::Opcode::return_:
  case GD::Bc::Instruction::Opcode::push_param:
    return 1;
  case GD::Bc::Instruction::Opcode::print:
  case GD::Bc::Instruction::Opcode::array_element:
  case GD::Bc::Instruction::Opcode::add:
  case GD::Bc::Instruction::Opcode::subtract:
  case GD::Bc::Instruction::Opcode::multiply:
  case GD::Bc::Instruction::Opcode::divide:
  case GD::Bc::Instruction::Opcode::modulo:
  case GD::Bc::Instruction::Opcode::power:
  case GD::Bc::Instruction::Opcode::store:
  case GD::Bc::Instruction::Opcode::equals:
  case GD::Bc::Instruction::Opcode::less_than_equals:
  case GD::Bc::Instruction::Opcode::not_equals:
  case GD::Bc::Instruction::Opcode::less_than:
  case GD::Bc::Instruction::Opcode::branch_zero:
  case GD::Bc::Instruction::Opcode::call:
  case GD::Bc::Instruction::Opcode::function_begin:
  case GD::Bc::Instruction::Opcode::function_end:
    return 2;
  }

  assert(false);
  abort();
}

void GD::Bc::Instruction::validate_operands() const
{
  switch (opcode_) {
  case GD::Bc::Instruction::Opcode::eof:
  case GD::Bc::Instruction::Opcode::scale:
  case GD::Bc::Instruction::Opcode::ibase:
  case GD::Bc::Instruction::Opcode::obase:
  case GD::Bc::Instruction::Opcode::push_param_mark:
  case GD::Bc::Instruction::Opcode::pop_param_mark:
  case GD::Bc::Instruction::Opcode::pop_param:
  case GD::Bc::Instruction::Opcode::pop_param_array:
    assert(!op1_.has_value());  // NOLINT
    assert(!op2_.has_value());  // NOLINT
    break;
  case GD::Bc::Instruction::Opcode::quit:
    assert(op1_.has_value());                         // NOLINT
    assert(!op2_.has_value());                        // NOLINT
    assert(std::holds_alternative<unsigned>(*op1_));  // NOLINT
    break;
  case GD::Bc::Instruction::Opcode::string:
  case GD::Bc::Instruction::Opcode::number:
    assert(op1_.has_value());                            // NOLINT
    assert(!op2_.has_value());                           // NOLINT
    assert(std::holds_alternative<std::string>(*op1_));  // NOLINT
    break;
  case GD::Bc::Instruction::Opcode::variable:
    assert(op1_.has_value());                         // NOLINT
    assert(!op2_.has_value());                        // NOLINT
    assert(std::holds_alternative<Variable>(*op1_));  // NOLINT
    break;
  case GD::Bc::Instruction::Opcode::array:
    assert(op1_.has_value());                      // NOLINT
    assert(!op2_.has_value());                     // NOLINT
    assert(std::holds_alternative<Array>(*op1_));  // NOLINT
    break;
  case GD::Bc::Instruction::Opcode::negate:
  case GD::Bc::Instruction::Opcode::load:
  case GD::Bc::Instruction::Opcode::scale_expr:
  case GD::Bc::Instruction::Opcode::sqrt:
  case GD::Bc::Instruction::Opcode::abs:
  case GD::Bc::Instruction::Opcode::length:
  case GD::Bc::Instruction::Opcode::branch:
  case GD::Bc::Instruction::Opcode::return_:
  case GD::Bc::Instruction::Opcode::push_param:
    assert(op1_.has_value());                       // NOLINT
    assert(!op2_.has_value());                      // NOLINT
    assert(std::holds_alternative<Offset>(*op1_));  // NOLINT
    break;
  case GD::Bc::Instruction::Opcode::print:
    assert(op1_.has_value());                       // NOLINT
    assert(op2_.has_value());                       // NOLINT
    assert(std::holds_alternative<Offset>(*op1_));  // NOLINT
    assert(std::holds_alternative<Stream>(*op2_));  // NOLINT
    break;
  case GD::Bc::Instruction::Opcode::array_element:
    assert(op1_.has_value());                       // NOLINT
    assert(op2_.has_value());                       // NOLINT
    assert(std::holds_alternative<Array>(*op1_));   // NOLINT
    assert(std::holds_alternative<Offset>(*op2_));  // NOLINT
    break;
  case GD::Bc::Instruction::Opcode::function_end:
    assert(op1_.has_value());                       // NOLINT
    assert(op2_.has_value());                       // NOLINT
    assert(std::holds_alternative<Letter>(*op1_));  // NOLINT
    assert(std::holds_alternative<Offset>(*op2_));  // NOLINT
    break;
  case GD::Bc::Instruction::Opcode::add:
  case GD::Bc::Instruction::Opcode::subtract:
  case GD::Bc::Instruction::Opcode::multiply:
  case GD::Bc::Instruction::Opcode::divide:
  case GD::Bc::Instruction::Opcode::modulo:
  case GD::Bc::Instruction::Opcode::power:
  case GD::Bc::Instruction::Opcode::store:
  case GD::Bc::Instruction::Opcode::equals:
  case GD::Bc::Instruction::Opcode::less_than_equals:
  case GD::Bc::Instruction::Opcode::not_equals:
  case GD::Bc::Instruction::Opcode::less_than:
  case GD::Bc::Instruction::Opcode::branch_zero:
    assert(op1_.has_value());                       // NOLINT
    assert(op2_.has_value());                       // NOLINT
    assert(std::holds_alternative<Offset>(*op1_));  // NOLINT
    assert(std::holds_alternative<Offset>(*op2_));  // NOLINT
    break;
  case GD::Bc::Instruction::Opcode::call:
    assert(op1_.has_value());                         // NOLINT
    assert(op2_.has_value());                         // NOLINT
    assert(std::holds_alternative<Letter>(*op1_));    // NOLINT
    assert(std::holds_alternative<Location>(*op2_));  // NOLINT
    break;
  case GD::Bc::Instruction::Opcode::function_begin:
    assert(op1_.has_value());                             // NOLINT
    assert(op2_.has_value());                             // NOLINT
    assert(std::holds_alternative<VariableMask>(*op1_));  // NOLINT
    assert(std::holds_alternative<Location>(*op2_));      // NOLINT
    break;
  }
}

auto GD::Bc::operator<<(std::ostream& os, GD::Bc::Instruction::Operand const& operand)
  -> std::ostream&
{
  std::visit([&os](auto const& o) { os << o; }, operand);
  return os;
}

auto GD::Bc::operator<<(std::ostream& os, GD::Bc::Instruction const& instruction) -> std::ostream&
{
  os << instruction.opcode();
  if (instruction.has_op1()) {
    os << " " << instruction.op1();
  }
  if (instruction.has_op2()) {
    os << ", " << instruction.op2();
  }
  return os;
}

auto GD::Bc::operator<<(std::ostream& os, GD::Bc::Instructions const& instructions) -> std::ostream&
{
  for (::size_t i = 0; i < instructions.size(); ++i) {
    os << i << '\t' << instructions[i] << '\n';
  }
  return os;
}
