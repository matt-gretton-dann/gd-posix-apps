/** \file   instruction.cc
 *  \brief  awk VM instructions
 *  \author Copyright 2021-2022, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/nl_types.h"

#include "gd/stdlib.h"

#include <memory>
#include <optional>
#include <ostream>
#include <regex>
#include <string>
#include <variant>
#include <vector>

#include "awk.hh"

GD::Awk::Instruction::Instruction(Opcode opcode) : opcode_(opcode)
{
  assert(op_count(opcode) == 0);
  assert(!has_result(opcode));
  validate_operands();
}

GD::Awk::Instruction::Instruction(Opcode opcode, Index reg) : opcode_(opcode), reg_(reg)
{
  assert(op_count(opcode) == 0);
  assert(has_result(opcode));
  assert(reg != illegal_index);
  validate_operands();
}

GD::Awk::Instruction::Instruction(Opcode opcode, Operand const& op1) : opcode_(opcode), op1_(op1)
{
  assert(op_count(opcode) == 1);
  assert(!has_result(opcode));
  validate_operands();
}

GD::Awk::Instruction::Instruction(Opcode opcode, Index reg, Operand const& op1)
    : opcode_(opcode), reg_(reg), op1_(op1)
{
  assert(op_count(opcode) == 1);
  assert(has_result(opcode));
  assert(reg != illegal_index);
  validate_operands();
}

GD::Awk::Instruction::Instruction(Opcode opcode, Operand const& op1, Operand const& op2)
    : opcode_(opcode), op1_(op1), op2_(op2)
{
  assert(op_count(opcode) == 2);  // NOLINT
  assert(!has_result(opcode));
  validate_operands();
}

GD::Awk::Instruction::Instruction(Opcode opcode, Index reg, Operand const& op1, Operand const& op2)
    : opcode_(opcode), reg_(reg), op1_(op1), op2_(op2)
{
  assert(op_count(opcode) == 2);  // NOLINT
  assert(has_result(opcode));
  assert(reg != illegal_index);
  validate_operands();
}

GD::Awk::Instruction::Instruction(Opcode opcode, Operand const& op1, Operand const& op2,
                                  Operand const& op3)
    : opcode_(opcode), op1_(op1), op2_(op2), op3_(op3)
{
  assert(op_count(opcode) == 3);  // NOLINT
  assert(!has_result(opcode));
  validate_operands();
}

GD::Awk::Instruction::Instruction(Opcode opcode, Index reg, Operand const& op1, Operand const& op2,
                                  Operand const& op3)
    : opcode_(opcode), reg_(reg), op1_(op1), op2_(op2), op3_(op3)
{
  assert(op_count(opcode) == 3);  // NOLINT
  assert(has_result(opcode));
  assert(reg != illegal_index);
  validate_operands();
}

auto GD::Awk::Instruction::opcode() const noexcept -> GD::Awk::Instruction::Opcode
{
  return opcode_;
}

auto GD::Awk::Instruction::has_reg() const noexcept -> bool { return reg_ != illegal_index; }

auto GD::Awk::Instruction::reg() const noexcept -> Index
{
  assert(has_reg());
  return reg_;
}

auto GD::Awk::Instruction::op1() const -> GD::Awk::Instruction::Operand const&
{
  if (!op1_.has_value()) {
    throw std::logic_error("Op 1 has no value.");
  }
  return *op1_;
}

void GD::Awk::Instruction::op1(Operand const& operand)
{
  assert(has_op1());  // NOLINT
  op1_ = operand;
  validate_operands();
}

auto GD::Awk::Instruction::op2() const -> GD::Awk::Instruction::Operand const&
{
  if (!op2_.has_value()) {
    throw std::logic_error("Op 2 has no value.");
  }
  return *op2_;
}

void GD::Awk::Instruction::op2(Operand const& operand)
{
  assert(has_op2());  // NOLINT
  op2_ = operand;
  validate_operands();
}

auto GD::Awk::Instruction::op3() const -> GD::Awk::Instruction::Operand const&
{
  if (!op3_.has_value()) {
    throw std::logic_error("Op 3 has no value.");
  }
  return *op3_;
}

void GD::Awk::Instruction::op3(Operand const& operand)
{
  assert(has_op3());  // NOLINT
  op3_ = operand;
  validate_operands();
}

auto GD::Awk::Instruction::has_op1() const noexcept -> bool { return has_op1(opcode_); }
auto GD::Awk::Instruction::has_op2() const noexcept -> bool { return has_op2(opcode_); }
auto GD::Awk::Instruction::has_op3() const noexcept -> bool { return has_op3(opcode_); }

auto GD::Awk::Instruction::has_op1(Opcode opcode) noexcept -> bool { return op_count(opcode) >= 1; }
auto GD::Awk::Instruction::has_op2(Opcode opcode) noexcept -> bool { return op_count(opcode) >= 2; }
auto GD::Awk::Instruction::has_op3(Opcode opcode) noexcept -> bool { return op_count(opcode) >= 3; }

auto GD::Awk::Instruction::has_result(Opcode opcode) noexcept -> bool
{
  switch (opcode) {
  case GD::Awk::Instruction::Opcode::open_param_pack:
  case GD::Awk::Instruction::Opcode::load_literal:
  case GD::Awk::Instruction::Opcode::load_lvalue:
  case GD::Awk::Instruction::Opcode::field:
  case GD::Awk::Instruction::Opcode::variable:
  case GD::Awk::Instruction::Opcode::to_number:
  case GD::Awk::Instruction::Opcode::to_bool:
  case GD::Awk::Instruction::Opcode::negate:
  case GD::Awk::Instruction::Opcode::logical_not:
  case GD::Awk::Instruction::Opcode::add:
  case GD::Awk::Instruction::Opcode::sub:
  case GD::Awk::Instruction::Opcode::power:
  case GD::Awk::Instruction::Opcode::multiply:
  case GD::Awk::Instruction::Opcode::divide:
  case GD::Awk::Instruction::Opcode::modulo:
  case GD::Awk::Instruction::Opcode::concat:
  case GD::Awk::Instruction::Opcode::is_equal:
  case GD::Awk::Instruction::Opcode::is_greater_than_equal:
  case GD::Awk::Instruction::Opcode::is_greater_than:
  case GD::Awk::Instruction::Opcode::is_less_than_equal:
  case GD::Awk::Instruction::Opcode::is_less_than:
  case GD::Awk::Instruction::Opcode::is_not_equal:
  case GD::Awk::Instruction::Opcode::re_match:
  case GD::Awk::Instruction::Opcode::logical_and:
  case GD::Awk::Instruction::Opcode::logical_or:
  case GD::Awk::Instruction::Opcode::open:
  case GD::Awk::Instruction::Opcode::popen:
  case GD::Awk::Instruction::Opcode::copy:
  case GD::Awk::Instruction::Opcode::length:
  case GD::Awk::Instruction::Opcode::array:
  case GD::Awk::Instruction::Opcode::array_element:
  case GD::Awk::Instruction::Opcode::atan2:
  case GD::Awk::Instruction::Opcode::cos:
  case GD::Awk::Instruction::Opcode::sin:
  case GD::Awk::Instruction::Opcode::exp:
  case GD::Awk::Instruction::Opcode::log:
  case GD::Awk::Instruction::Opcode::sqrt:
  case GD::Awk::Instruction::Opcode::int_:
  case GD::Awk::Instruction::Opcode::rand:
  case GD::Awk::Instruction::Opcode::srand:
  case GD::Awk::Instruction::Opcode::current_time:
  case GD::Awk::Instruction::Opcode::subst:
  case GD::Awk::Instruction::Opcode::gsubst:
    return true;
  case GD::Awk::Instruction::Opcode::close_param_pack:
  case GD::Awk::Instruction::Opcode::store_lvalue:
  case GD::Awk::Instruction::Opcode::print:
  case GD::Awk::Instruction::Opcode::printf:
  case GD::Awk::Instruction::Opcode::push_param:
  case GD::Awk::Instruction::Opcode::branch_if_false:
  case GD::Awk::Instruction::Opcode::reserve_regs:
  case GD::Awk::Instruction::Opcode::branch:
    return false;
  }
}

auto GD::Awk::operator<<(std::ostream& os, GD::Awk::Instruction::Opcode opcode) -> std::ostream&
{
  switch (opcode) {
  case GD::Awk::Instruction::Opcode::load_literal:
    os << "load_literal";
    break;
  case GD::Awk::Instruction::Opcode::load_lvalue:
    os << "load_lvalue";
    break;
  case GD::Awk::Instruction::Opcode::store_lvalue:
    os << "store_lvalue";
    break;
  case GD::Awk::Instruction::Opcode::field:
    os << "field";
    break;
  case GD::Awk::Instruction::Opcode::variable:
    os << "variable";
    break;
  case GD::Awk::Instruction::Opcode::print:
    os << "print";
    break;
  case GD::Awk::Instruction::Opcode::printf:
    os << "printf";
    break;
  case GD::Awk::Instruction::Opcode::open_param_pack:
    os << "open_param_pack";
    break;
  case GD::Awk::Instruction::Opcode::push_param:
    os << "push_param";
    break;
  case GD::Awk::Instruction::Opcode::close_param_pack:
    os << "close_param_pack";
    break;
  case GD::Awk::Instruction::Opcode::add:
    os << "add";
    break;
  case GD::Awk::Instruction::Opcode::sub:
    os << "sub";
    break;
  case GD::Awk::Instruction::Opcode::power:
    os << "power";
    break;
  case GD::Awk::Instruction::Opcode::multiply:
    os << "multiply";
    break;
  case GD::Awk::Instruction::Opcode::divide:
    os << "divide";
    break;
  case GD::Awk::Instruction::Opcode::modulo:
    os << "modulo";
    break;
  case GD::Awk::Instruction::Opcode::concat:
    os << "concat";
    break;
  case GD::Awk::Instruction::Opcode::to_bool:
    os << "to_bool";
    break;
  case GD::Awk::Instruction::Opcode::to_number:
    os << "to_number";
    break;
  case GD::Awk::Instruction::Opcode::negate:
    os << "negate";
    break;
  case GD::Awk::Instruction::Opcode::logical_not:
    os << "logical_not";
    break;
  case GD::Awk::Instruction::Opcode::is_equal:
    os << "is_equal";
    break;
  case GD::Awk::Instruction::Opcode::is_not_equal:
    os << "is_not_equal";
    break;
  case GD::Awk::Instruction::Opcode::is_less_than:
    os << "is_less_than";
    break;
  case GD::Awk::Instruction::Opcode::is_less_than_equal:
    os << "is_less_than_equal";
    break;
  case GD::Awk::Instruction::Opcode::is_greater_than:
    os << "is_greater_than";
    break;
  case GD::Awk::Instruction::Opcode::is_greater_than_equal:
    os << "is_greater_than_equal";
    break;
  case GD::Awk::Instruction::Opcode::branch_if_false:
    os << "branch_if_false";
    break;
  case GD::Awk::Instruction::Opcode::re_match:
    os << "re_match";
    break;
  case GD::Awk::Instruction::Opcode::logical_and:
    os << "logical_and";
    break;
  case GD::Awk::Instruction::Opcode::logical_or:
    os << "logical_or";
    break;
  case GD::Awk::Instruction::Opcode::popen:
    os << "popen";
    break;
  case GD::Awk::Instruction::Opcode::open:
    os << "open";
    break;
  case GD::Awk::Instruction::Opcode::reserve_regs:
    os << "reserve_regs";
    break;
  case GD::Awk::Instruction::Opcode::branch:
    os << "branch";
    break;
  case GD::Awk::Instruction::Opcode::copy:
    os << "copy";
    break;
  case GD::Awk::Instruction::Opcode::length:
    os << "length";
    break;
  case GD::Awk::Instruction::Opcode::array:
    os << "array";
    break;
  case GD::Awk::Instruction::Opcode::array_element:
    os << "array_element";
    break;
  case Instruction::Opcode::atan2:
    os << "atan2";
    break;
  case Instruction::Opcode::cos:
    os << "cos";
    break;
  case Instruction::Opcode::sin:
    os << "sin";
    break;
  case Instruction::Opcode::exp:
    os << "exp";
    break;
  case Instruction::Opcode::log:
    os << "log";
    break;
  case Instruction::Opcode::sqrt:
    os << "sqrt";
    break;
  case Instruction::Opcode::int_:
    os << "int_";
    break;
  case Instruction::Opcode::rand:
    os << "rand";
    break;
  case Instruction::Opcode::srand:
    os << "srand";
    break;
  case Instruction::Opcode::current_time:
    os << "current_time";
    break;
  case Instruction::Opcode::subst:
    os << "subst";
    break;
  case Instruction::Opcode::gsubst:
    os << "gsubst";
    break;
  }
  return os;
}

auto GD::Awk::Instruction::op_count(Opcode opcode) noexcept -> unsigned
{
  switch (opcode) {
  case GD::Awk::Instruction::Opcode::open_param_pack:
  case GD::Awk::Instruction::Opcode::rand:
  case GD::Awk::Instruction::Opcode::current_time:
    return 0;
  case GD::Awk::Instruction::Opcode::load_literal:
  case GD::Awk::Instruction::Opcode::load_lvalue:
  case GD::Awk::Instruction::Opcode::field:
  case GD::Awk::Instruction::Opcode::variable:
  case GD::Awk::Instruction::Opcode::close_param_pack:
  case GD::Awk::Instruction::Opcode::to_number:
  case GD::Awk::Instruction::Opcode::to_bool:
  case GD::Awk::Instruction::Opcode::negate:
  case GD::Awk::Instruction::Opcode::logical_not:
  case GD::Awk::Instruction::Opcode::reserve_regs:
  case GD::Awk::Instruction::Opcode::popen:
  case GD::Awk::Instruction::Opcode::branch:
  case GD::Awk::Instruction::Opcode::copy:
  case GD::Awk::Instruction::Opcode::length:
  case GD::Awk::Instruction::Opcode::array:
  case GD::Awk::Instruction::Opcode::cos:
  case GD::Awk::Instruction::Opcode::sin:
  case GD::Awk::Instruction::Opcode::exp:
  case GD::Awk::Instruction::Opcode::log:
  case GD::Awk::Instruction::Opcode::sqrt:
  case GD::Awk::Instruction::Opcode::int_:
  case GD::Awk::Instruction::Opcode::srand:
    return 1;
  case GD::Awk::Instruction::Opcode::store_lvalue:
  case GD::Awk::Instruction::Opcode::print:
  case GD::Awk::Instruction::Opcode::printf:
  case GD::Awk::Instruction::Opcode::push_param:
  case GD::Awk::Instruction::Opcode::add:
  case GD::Awk::Instruction::Opcode::sub:
  case GD::Awk::Instruction::Opcode::power:
  case GD::Awk::Instruction::Opcode::multiply:
  case GD::Awk::Instruction::Opcode::divide:
  case GD::Awk::Instruction::Opcode::modulo:
  case GD::Awk::Instruction::Opcode::concat:
  case GD::Awk::Instruction::Opcode::is_equal:
  case GD::Awk::Instruction::Opcode::is_greater_than_equal:
  case GD::Awk::Instruction::Opcode::is_greater_than:
  case GD::Awk::Instruction::Opcode::is_less_than_equal:
  case GD::Awk::Instruction::Opcode::is_less_than:
  case GD::Awk::Instruction::Opcode::is_not_equal:
  case GD::Awk::Instruction::Opcode::branch_if_false:
  case GD::Awk::Instruction::Opcode::re_match:
  case GD::Awk::Instruction::Opcode::logical_and:
  case GD::Awk::Instruction::Opcode::logical_or:
  case GD::Awk::Instruction::Opcode::open:
  case GD::Awk::Instruction::Opcode::array_element:
  case GD::Awk::Instruction::Opcode::atan2:
    return 2;
  case GD::Awk::Instruction::Opcode::subst:
  case GD::Awk::Instruction::Opcode::gsubst:
    return 3;
  }
}

void GD::Awk::Instruction::validate_operands() const
{
#ifndef NDEBUG
  assert(has_reg() == has_result(opcode_));
  assert(op1_.has_value() == (op_count(opcode_) >= 1));
  assert(op2_.has_value() == (op_count(opcode_) >= 2));
  assert(op3_.has_value() == (op_count(opcode_) >= 3));
  switch (opcode_) {
  case GD::Awk::Instruction::Opcode::open_param_pack:
  case GD::Awk::Instruction::Opcode::current_time:
  case GD::Awk::Instruction::Opcode::rand:
    break;
  case GD::Awk::Instruction::Opcode::load_literal:
    // NOLINTNEXTLINE
    assert(std::holds_alternative<Integer>(*op1_) || std::holds_alternative<Floating>(*op1_) ||
           std::holds_alternative<FileDescriptor>(*op1_) ||  // NOLINT
           std::holds_alternative<std::string>(*op1_) ||     // NOLINT
           std::holds_alternative<std::regex>(*op1_));       // NOLINT
    break;
  case GD::Awk::Instruction::Opcode::store_lvalue:
  case GD::Awk::Instruction::Opcode::add:
  case GD::Awk::Instruction::Opcode::sub:
  case GD::Awk::Instruction::Opcode::power:
  case GD::Awk::Instruction::Opcode::multiply:
  case GD::Awk::Instruction::Opcode::divide:
  case GD::Awk::Instruction::Opcode::modulo:
  case GD::Awk::Instruction::Opcode::concat:
  case GD::Awk::Instruction::Opcode::print:
  case GD::Awk::Instruction::Opcode::printf:
  case GD::Awk::Instruction::Opcode::push_param:
  case GD::Awk::Instruction::Opcode::is_less_than:
  case GD::Awk::Instruction::Opcode::is_not_equal:
  case GD::Awk::Instruction::Opcode::is_less_than_equal:
  case GD::Awk::Instruction::Opcode::is_greater_than:
  case GD::Awk::Instruction::Opcode::is_greater_than_equal:
  case GD::Awk::Instruction::Opcode::is_equal:
  case GD::Awk::Instruction::Opcode::branch_if_false:
  case GD::Awk::Instruction::Opcode::re_match:
  case GD::Awk::Instruction::Opcode::logical_and:
  case GD::Awk::Instruction::Opcode::logical_or:
  case GD::Awk::Instruction::Opcode::atan2:
    assert(std::holds_alternative<Index>(*op1_));  // NOLINT
    assert(std::holds_alternative<Index>(*op2_));  // NOLINT
    break;
  case GD::Awk::Instruction::Opcode::open:
    assert(std::holds_alternative<Index>(*op1_));    // NOLINT
    assert(std::holds_alternative<Integer>(*op2_));  // NOLINT
    break;
  case GD::Awk::Instruction::Opcode::variable:
    assert(std::holds_alternative<VariableName>(*op1_));  // NOLINT
    break;
  case GD::Awk::Instruction::Opcode::array:
    assert(std::holds_alternative<ArrayName>(*op1_));  // NOLINT
    break;
  case GD::Awk::Instruction::Opcode::array_element:
    assert(std::holds_alternative<ArrayName>(*op1_));  // NOLINT
    assert(std::holds_alternative<Index>(*op2_));      // NOLINT
    break;
  case GD::Awk::Instruction::Opcode::close_param_pack:
  case GD::Awk::Instruction::Opcode::field:
  case GD::Awk::Instruction::Opcode::load_lvalue:
  case GD::Awk::Instruction::Opcode::to_bool:
  case GD::Awk::Instruction::Opcode::to_number:
  case GD::Awk::Instruction::Opcode::negate:
  case GD::Awk::Instruction::Opcode::logical_not:
  case GD::Awk::Instruction::Opcode::reserve_regs:
  case GD::Awk::Instruction::Opcode::popen:
  case GD::Awk::Instruction::Opcode::copy:
  case GD::Awk::Instruction::Opcode::branch:
  case GD::Awk::Instruction::Opcode::length:
  case GD::Awk::Instruction::Opcode::cos:
  case GD::Awk::Instruction::Opcode::sin:
  case GD::Awk::Instruction::Opcode::exp:
  case GD::Awk::Instruction::Opcode::log:
  case GD::Awk::Instruction::Opcode::sqrt:
  case GD::Awk::Instruction::Opcode::int_:
  case GD::Awk::Instruction::Opcode::srand:
    assert(std::holds_alternative<Index>(*op1_));  // NOLINT
    break;
  case GD::Awk::Instruction::Opcode::subst:
  case GD::Awk::Instruction::Opcode::gsubst:
    assert(std::holds_alternative<Index>(*op1_));  // NOLINT
    assert(std::holds_alternative<Index>(*op2_));  // NOLINT
    assert(std::holds_alternative<Index>(*op3_));  // NOLINT
    break;
  }
#endif
}

auto GD::Awk::operator<<(std::ostream& os, Instruction::Operand const& operand) -> std::ostream&
{
  std::visit(GD::Overloaded{
               [&os](auto const& o) { os << o; },
               [&os]<typename T, typename U>(GD::TypeWrapper<T, U> const& t) { os << t.get(); },
               [&os](std::regex const&) { os << "regex"; },
             },
             operand);
  return os;
}

auto GD::Awk::operator<<(std::ostream& os, Instruction const& instruction) -> std::ostream&
{
  os << instruction.opcode();
  if (instruction.has_op1()) {
    os << " " << instruction.op1();
  }
  if (instruction.has_op2()) {
    os << ", " << instruction.op2();
  }
  if (instruction.has_op3()) {
    os << ", " << instruction.op3();
  }
  if (instruction.has_reg()) {
    os << " -> " << instruction.reg();
  }
  return os;
}

auto GD::Awk::operator<<(std::ostream& os, Instructions const& instructions) -> std::ostream&
{
  for (std::size_t i{0}; i != instructions.size(); ++i) {
    os << i << '\t' << instructions[i] << '\n';
  }
  return os;
}
