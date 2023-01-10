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
  case Opcode::open_param_pack:
  case Opcode::load_literal:
  case Opcode::load_lvalue:
  case Opcode::field:
  case Opcode::variable:
  case Opcode::to_number:
  case Opcode::to_bool:
  case Opcode::negate:
  case Opcode::logical_not:
  case Opcode::add:
  case Opcode::sub:
  case Opcode::power:
  case Opcode::multiply:
  case Opcode::divide:
  case Opcode::modulo:
  case Opcode::concat:
  case Opcode::is_equal:
  case Opcode::is_greater_than_equal:
  case Opcode::is_greater_than:
  case Opcode::is_less_than_equal:
  case Opcode::is_less_than:
  case Opcode::is_not_equal:
  case Opcode::re_match:
  case Opcode::logical_and:
  case Opcode::logical_or:
  case Opcode::open:
  case Opcode::popen:
  case Opcode::copy:
  case Opcode::length:
  case Opcode::array:
  case Opcode::array_element:
  case Opcode::atan2:
  case Opcode::cos:
  case Opcode::sin:
  case Opcode::exp:
  case Opcode::log:
  case Opcode::sqrt:
  case Opcode::int_:
  case Opcode::rand:
  case Opcode::srand:
  case Opcode::current_time:
  case Opcode::subst:
  case Opcode::gsubst:
  case Opcode::index:
  case Opcode::match:
    return true;
  case Opcode::close_param_pack:
  case Opcode::store_lvalue:
  case Opcode::print:
  case Opcode::printf:
  case Opcode::push_param:
  case Opcode::branch_if_false:
  case Opcode::reserve_regs:
  case Opcode::branch:
    return false;
  }
}

auto GD::Awk::operator<<(std::ostream& os, GD::Awk::Instruction::Opcode opcode) -> std::ostream&
{
  switch (opcode) {
  case Instruction::Opcode::load_literal:
    os << "load_literal";
    break;
  case Instruction::Opcode::load_lvalue:
    os << "load_lvalue";
    break;
  case Instruction::Opcode::store_lvalue:
    os << "store_lvalue";
    break;
  case Instruction::Opcode::field:
    os << "field";
    break;
  case Instruction::Opcode::variable:
    os << "variable";
    break;
  case Instruction::Opcode::print:
    os << "print";
    break;
  case Instruction::Opcode::printf:
    os << "printf";
    break;
  case Instruction::Opcode::open_param_pack:
    os << "open_param_pack";
    break;
  case Instruction::Opcode::push_param:
    os << "push_param";
    break;
  case Instruction::Opcode::close_param_pack:
    os << "close_param_pack";
    break;
  case Instruction::Opcode::add:
    os << "add";
    break;
  case Instruction::Opcode::sub:
    os << "sub";
    break;
  case Instruction::Opcode::power:
    os << "power";
    break;
  case Instruction::Opcode::multiply:
    os << "multiply";
    break;
  case Instruction::Opcode::divide:
    os << "divide";
    break;
  case Instruction::Opcode::modulo:
    os << "modulo";
    break;
  case Instruction::Opcode::concat:
    os << "concat";
    break;
  case Instruction::Opcode::to_bool:
    os << "to_bool";
    break;
  case Instruction::Opcode::to_number:
    os << "to_number";
    break;
  case Instruction::Opcode::negate:
    os << "negate";
    break;
  case Instruction::Opcode::logical_not:
    os << "logical_not";
    break;
  case Instruction::Opcode::is_equal:
    os << "is_equal";
    break;
  case Instruction::Opcode::is_not_equal:
    os << "is_not_equal";
    break;
  case Instruction::Opcode::is_less_than:
    os << "is_less_than";
    break;
  case Instruction::Opcode::is_less_than_equal:
    os << "is_less_than_equal";
    break;
  case Instruction::Opcode::is_greater_than:
    os << "is_greater_than";
    break;
  case Instruction::Opcode::is_greater_than_equal:
    os << "is_greater_than_equal";
    break;
  case Instruction::Opcode::branch_if_false:
    os << "branch_if_false";
    break;
  case Instruction::Opcode::re_match:
    os << "re_match";
    break;
  case Instruction::Opcode::logical_and:
    os << "logical_and";
    break;
  case Instruction::Opcode::logical_or:
    os << "logical_or";
    break;
  case Instruction::Opcode::popen:
    os << "popen";
    break;
  case Instruction::Opcode::open:
    os << "open";
    break;
  case Instruction::Opcode::reserve_regs:
    os << "reserve_regs";
    break;
  case Instruction::Opcode::branch:
    os << "branch";
    break;
  case Instruction::Opcode::copy:
    os << "copy";
    break;
  case Instruction::Opcode::length:
    os << "length";
    break;
  case Instruction::Opcode::array:
    os << "array";
    break;
  case Instruction::Opcode::array_element:
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
  case Instruction::Opcode::index:
    os << "index";
    break;
  case Instruction::Opcode::match:
    os << "match";
    break;
  }
  return os;
}

auto GD::Awk::Instruction::op_count(Opcode opcode) noexcept -> unsigned
{
  switch (opcode) {
  case Opcode::open_param_pack:
  case Opcode::rand:
  case Opcode::current_time:
    return 0;
  case Opcode::load_literal:
  case Opcode::load_lvalue:
  case Opcode::field:
  case Opcode::variable:
  case Opcode::close_param_pack:
  case Opcode::to_number:
  case Opcode::to_bool:
  case Opcode::negate:
  case Opcode::logical_not:
  case Opcode::reserve_regs:
  case Opcode::popen:
  case Opcode::branch:
  case Opcode::copy:
  case Opcode::length:
  case Opcode::array:
  case Opcode::cos:
  case Opcode::sin:
  case Opcode::exp:
  case Opcode::log:
  case Opcode::sqrt:
  case Opcode::int_:
  case Opcode::srand:
    return 1;
  case Opcode::store_lvalue:
  case Opcode::print:
  case Opcode::printf:
  case Opcode::push_param:
  case Opcode::add:
  case Opcode::sub:
  case Opcode::power:
  case Opcode::multiply:
  case Opcode::divide:
  case Opcode::modulo:
  case Opcode::concat:
  case Opcode::is_equal:
  case Opcode::is_greater_than_equal:
  case Opcode::is_greater_than:
  case Opcode::is_less_than_equal:
  case Opcode::is_less_than:
  case Opcode::is_not_equal:
  case Opcode::branch_if_false:
  case Opcode::re_match:
  case Opcode::logical_and:
  case Opcode::logical_or:
  case Opcode::atan2:
  case Opcode::open:
  case Opcode::array_element:
  case Opcode::index:
  case Opcode::match:
    return 2;
  case Opcode::subst:
  case Opcode::gsubst:
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
  case Opcode::open_param_pack:
  case Opcode::current_time:
  case Opcode::rand:
    break;
  case Opcode::load_literal:
    // NOLINTNEXTLINE
    assert(std::holds_alternative<Integer>(*op1_) || std::holds_alternative<Floating>(*op1_) ||
           std::holds_alternative<FileDescriptor>(*op1_) ||  // NOLINT
           std::holds_alternative<std::string>(*op1_) ||     // NOLINT
           std::holds_alternative<std::regex>(*op1_));       // NOLINT
    break;
  case Opcode::store_lvalue:
  case Opcode::add:
  case Opcode::sub:
  case Opcode::power:
  case Opcode::multiply:
  case Opcode::divide:
  case Opcode::modulo:
  case Opcode::concat:
  case Opcode::print:
  case Opcode::printf:
  case Opcode::push_param:
  case Opcode::is_less_than:
  case Opcode::is_not_equal:
  case Opcode::is_less_than_equal:
  case Opcode::is_greater_than:
  case Opcode::is_greater_than_equal:
  case Opcode::is_equal:
  case Opcode::branch_if_false:
  case Opcode::re_match:
  case Opcode::logical_and:
  case Opcode::logical_or:
  case Opcode::atan2:
  case Opcode::index:
  case Opcode::match:
    assert(std::holds_alternative<Index>(*op1_));  // NOLINT
    assert(std::holds_alternative<Index>(*op2_));  // NOLINT
    break;
  case Opcode::open:
    assert(std::holds_alternative<Index>(*op1_));    // NOLINT
    assert(std::holds_alternative<Integer>(*op2_));  // NOLINT
    break;
  case Opcode::variable:
    assert(std::holds_alternative<VariableName>(*op1_));  // NOLINT
    break;
  case Opcode::array:
    assert(std::holds_alternative<ArrayName>(*op1_));  // NOLINT
    break;
  case Opcode::array_element:
    assert(std::holds_alternative<ArrayName>(*op1_));  // NOLINT
    assert(std::holds_alternative<Index>(*op2_));      // NOLINT
    break;
  case Opcode::close_param_pack:
  case Opcode::field:
  case Opcode::load_lvalue:
  case Opcode::to_bool:
  case Opcode::to_number:
  case Opcode::negate:
  case Opcode::logical_not:
  case Opcode::reserve_regs:
  case Opcode::popen:
  case Opcode::copy:
  case Opcode::branch:
  case Opcode::length:
  case Opcode::cos:
  case Opcode::sin:
  case Opcode::exp:
  case Opcode::log:
  case Opcode::sqrt:
  case Opcode::int_:
  case Opcode::srand:
    assert(std::holds_alternative<Index>(*op1_));  // NOLINT
    break;
  case Opcode::subst:
  case Opcode::gsubst:
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
