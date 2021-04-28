/** \file   instruction.cc
 *  \brief  bc VM instructions
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/stdlib.h"

#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <variant>
#include <vector>

#include "bc.hh"
#include <unordered_map>

namespace {
/* Can't assume letters are contiguous code-points.  */
std::unordered_map<char, ::uint32_t> letter_mask_map = {
  {'a', 1 << 0},  {'b', 1 << 1},  {'c', 1 << 2},  {'d', 1 << 3},  {'e', 1 << 4},  {'f', 1 << 5},
  {'g', 1 << 6},  {'h', 1 << 7},  {'i', 1 << 8},  {'j', 1 << 9},  {'k', 1 << 10}, {'l', 1 << 11},
  {'m', 1 << 12}, {'n', 1 << 13}, {'o', 1 << 14}, {'p', 1 << 15}, {'q', 1 << 16}, {'r', 1 << 17},
  {'s', 1 << 18}, {'t', 1 << 19}, {'u', 1 << 20}, {'v', 1 << 21}, {'w', 1 << 22}, {'x', 1 << 23},
  {'y', 1 << 24}, {'z', 1 << 25},
};
}  // namespace

GD::Bc::VariableMask::VariableMask() : variable_mask_(0), array_mask_(0) {}

GD::Bc::VariableMask::VariableMask(std::string_view vars, std::string_view arrays)
    : variable_mask_(0), array_mask_(0)
{
  for (auto l : vars) {
    add_variable(l);
  }
  for (auto l : arrays) {
    add_array(l);
  }
}

void GD::Bc::VariableMask::add_variable(char letter)
{
  auto it = letter_mask_map.find(letter);
  assert(it != letter_mask_map.end());
  variable_mask_ |= it->second;
}

void GD::Bc::VariableMask::add_array(char letter)
{
  auto it = letter_mask_map.find(letter);
  assert(it != letter_mask_map.end());
  array_mask_ |= it->second;
}

bool GD::Bc::VariableMask::contains_variable(char letter) const
{
  auto it = letter_mask_map.find(letter);
  assert(it != letter_mask_map.end());
  return (variable_mask_ & it->second) != 0;
}

bool GD::Bc::VariableMask::contains_array(char letter) const
{
  auto it = letter_mask_map.find(letter);
  assert(it != letter_mask_map.end());
  return (array_mask_ & it->second) != 0;
}

bool GD::Bc::VariableMask::operator==(VariableMask rhs) const
{
  return variable_mask_ == rhs.variable_mask_ && array_mask_ == rhs.array_mask_;
}

bool GD::Bc::operator!=(VariableMask lhs, VariableMask rhs) { return !(lhs == rhs); }

std::ostream& GD::Bc::operator<<(std::ostream& os, GD::Bc::VariableMask mask)
{
  bool need_comma = false;
  mask.for_each_variable([&need_comma, &os](char letter) {
    os << (need_comma ? ", " : "") << letter;
    need_comma = true;
  });
  mask.for_each_array([&need_comma, &os](char letter) {
    os << (need_comma ? ", " : "") << letter << "[]";
    need_comma = true;
  });

  return os;
}

GD::Bc::Instruction::Instruction(Opcode opcode) : opcode_(opcode)
{
  assert(op_count(opcode) == 0);
  validate_operands();
}

GD::Bc::Instruction::Instruction(Opcode opcode, Operand const& op1) : opcode_(opcode), op1_(op1)
{
  assert(op_count(opcode) == 1);
  validate_operands();
}

GD::Bc::Instruction::Instruction(Opcode opcode, Operand const& op1, Operand const& op2)
    : opcode_(opcode), op1_(op1), op2_(op2)
{
  assert(op_count(opcode) == 2);
  validate_operands();
}

GD::Bc::Instruction::Opcode GD::Bc::Instruction::opcode() const { return opcode_; }

GD::Bc::Instruction::Operand const& GD::Bc::Instruction::op1() const
{
  assert(has_op1());
  return *op1_;
}

void GD::Bc::Instruction::op1(Operand const& operand)
{
  assert(has_op1());
  op1_ = operand;
  validate_operands();
}

GD::Bc::Instruction::Operand const& GD::Bc::Instruction::op2() const
{
  assert(has_op2());
  return *op2_;
}

void GD::Bc::Instruction::op2(Operand const& operand)
{
  assert(has_op2());
  op2_ = operand;
  validate_operands();
}

bool GD::Bc::Instruction::has_op1() const { return has_op1(opcode_); }
bool GD::Bc::Instruction::has_op2() const { return has_op2(opcode_); }

bool GD::Bc::Instruction::has_op1(Opcode opcode) { return op_count(opcode) >= 1; }

bool GD::Bc::Instruction::has_op2(Opcode opcode) { return op_count(opcode) >= 2; }

std::ostream& GD::Bc::operator<<(std::ostream& os, GD::Bc::Instruction::Stream s)
{
  switch (s) {
  case GD::Bc::Instruction::Stream::stdout:
    os << "stdout";
    break;
  case GD::Bc::Instruction::Stream::stderr:
    os << "stderr";
    break;
  }
  return os;
}

std::ostream& GD::Bc::operator<<(std::ostream& os, GD::Bc::Instruction::Opcode opcode)
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
  case GD::Bc::Instruction::Opcode::function_begin:
    os << "function_begin";
    break;
  case GD::Bc::Instruction::Opcode::function_end:
    os << "function_end";
    break;
  }
  return os;
}

unsigned GD::Bc::Instruction::op_count(Opcode opcode)
{
  switch (opcode) {
  case GD::Bc::Instruction::Opcode::eof:
  case GD::Bc::Instruction::Opcode::scale:
  case GD::Bc::Instruction::Opcode::ibase:
  case GD::Bc::Instruction::Opcode::obase:
  case GD::Bc::Instruction::Opcode::push_param_mark:
  case GD::Bc::Instruction::Opcode::pop_param_mark:
  case GD::Bc::Instruction::Opcode::pop_param:
    return 0;
  case GD::Bc::Instruction::Opcode::quit:
  case GD::Bc::Instruction::Opcode::string:
  case GD::Bc::Instruction::Opcode::number:
  case GD::Bc::Instruction::Opcode::variable:
  case GD::Bc::Instruction::Opcode::array:
  case GD::Bc::Instruction::Opcode::negate:
  case GD::Bc::Instruction::Opcode::load:
  case GD::Bc::Instruction::Opcode::scale_expr:
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
    assert(!op1_.has_value());
    assert(!op2_.has_value());
    break;
  case GD::Bc::Instruction::Opcode::quit:
    assert(op1_.has_value());
    assert(!op2_.has_value());
    assert(std::holds_alternative<unsigned>(*op1_));
    break;
  case GD::Bc::Instruction::Opcode::string:
  case GD::Bc::Instruction::Opcode::number:
    assert(op1_.has_value());
    assert(!op2_.has_value());
    assert(std::holds_alternative<std::string>(*op1_));
    break;
  case GD::Bc::Instruction::Opcode::variable:
  case GD::Bc::Instruction::Opcode::array:
    assert(op1_.has_value());
    assert(!op2_.has_value());
    assert(std::holds_alternative<char>(*op1_));
    break;
  case GD::Bc::Instruction::Opcode::negate:
  case GD::Bc::Instruction::Opcode::load:
  case GD::Bc::Instruction::Opcode::scale_expr:
  case GD::Bc::Instruction::Opcode::sqrt:
  case GD::Bc::Instruction::Opcode::length:
  case GD::Bc::Instruction::Opcode::branch:
  case GD::Bc::Instruction::Opcode::return_:
  case GD::Bc::Instruction::Opcode::push_param:
    assert(op1_.has_value());
    assert(!op2_.has_value());
    assert(std::holds_alternative<Offset>(*op1_));
    break;
  case GD::Bc::Instruction::Opcode::print:
    assert(op1_.has_value());
    assert(op2_.has_value());
    assert(std::holds_alternative<Offset>(*op1_));
    assert(std::holds_alternative<Stream>(*op2_));
    break;
  case GD::Bc::Instruction::Opcode::array_element:
  case GD::Bc::Instruction::Opcode::function_end:
    assert(op1_.has_value());
    assert(op2_.has_value());
    assert(std::holds_alternative<char>(*op1_));
    assert(std::holds_alternative<Offset>(*op2_));
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
    assert(op1_.has_value());
    assert(op2_.has_value());
    assert(std::holds_alternative<Offset>(*op1_));
    assert(std::holds_alternative<Offset>(*op2_));
    break;
  case GD::Bc::Instruction::Opcode::call:
    assert(op1_.has_value());
    assert(op2_.has_value());
    assert(std::holds_alternative<char>(*op1_));
    assert(std::holds_alternative<Location>(*op2_));
    break;
  case GD::Bc::Instruction::Opcode::function_begin:
    assert(op1_.has_value());
    assert(op2_.has_value());
    assert(std::holds_alternative<VariableMask>(*op1_));
    assert(std::holds_alternative<Location>(*op2_));
    break;
  }
}

std::ostream& GD::Bc::operator<<(std::ostream& os, GD::Bc::Instruction::Operand operand)
{
  std::visit([&os](auto o) { os << o; }, operand);
  return os;
}

std::ostream& GD::Bc::operator<<(std::ostream& os, GD::Bc::Instruction const& instruction)
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

std::ostream& GD::Bc::operator<<(std::ostream& os, GD::Bc::Instructions const& instructions)
{
  for (::size_t i = 0; i < instructions.size(); ++i) {
    os << i << '\t' << instructions[i] << '\n';
  }
  return os;
}
