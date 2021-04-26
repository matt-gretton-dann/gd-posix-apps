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

GD::Bc::Instruction::Instruction(Opcode opcode) : opcode_(opcode)
{
  assert(!has_op1(opcode));
  assert(!has_op2(opcode));
}

GD::Bc::Instruction::Instruction(Opcode opcode, Operand const& op1) : opcode_(opcode), op1_(op1)
{
  assert(has_op1(opcode));
  assert(!has_op2(opcode));
}

GD::Bc::Instruction::Instruction(Opcode opcode, Operand const& op1, Operand const& op2)
    : opcode_(opcode), op1_(op1), op2_(op2)
{
  assert(has_op1(opcode));
  assert(has_op2(opcode));
}

GD::Bc::Instruction::Opcode GD::Bc::Instruction::opcode() const { return opcode_; }

GD::Bc::Instruction::Operand const& GD::Bc::Instruction::op1() const
{
  assert(has_op1());
  assert(op1_.has_value());
  return *op1_;
}

GD::Bc::Instruction::Operand const& GD::Bc::Instruction::op2() const
{
  assert(has_op2());
  assert(op2_.has_value());
  return *op2_;
}

bool GD::Bc::Instruction::has_op1() const { return has_op1(opcode_); }
bool GD::Bc::Instruction::has_op2() const { return has_op2(opcode_); }

bool GD::Bc::Instruction::has_op1(Opcode opcode)
{
  return opcode == Opcode::print || opcode == Opcode::quit || opcode == Opcode::string;
}

bool GD::Bc::Instruction::has_op2(Opcode opcode) { return opcode == Opcode::print; }

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
  }
  return os;
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
