/** \file   vm.cc
 *  \brief  Implementation of GD::BC::VM and related classes
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "bc-messages.hh"

#include <array>
#include <assert.h>
#include <ostream>

#include "bc.hh"
#include "number.hh"

GD::Bc::VM::VM(std::ostream& stdout, std::ostream& stderr) : stdout_(stdout), stderr_(stderr) {}

void GD::Bc::VM::execute(Instructions& instructions)
{
  for (Instruction::Index i = 0; i < instructions.size(); ++i) {
    switch (instructions[i].opcode()) {
    case Instruction::Opcode::string:
      execute_string(instructions, i);
      break;
    case Instruction::Opcode::number:
      execute_number(instructions, i);
      break;
    case Instruction::Opcode::print:
      execute_print(instructions, i);
      break;
    case Instruction::Opcode::quit:
      execute_quit(instructions, i);
      break;
    case Instruction::Opcode::eof:
      break;
    default:
      assert(false);
      break;
    }
  }
}

void GD::Bc::VM::execute_string(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::string);
  instructions[i].result(std::string_view(std::get<std::string>(instructions[i].op1())));
}

void GD::Bc::VM::execute_number(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::number);
  instructions[i].result(Number(std::get<std::string>(instructions[i].op1()), ibase_));
}

void GD::Bc::VM::execute_print(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::print);
  Index expr = std::get<Offset>(instructions[i].op1()) + i;
  assert(expr < instructions.size());
  std::ostream& os =
    (std::get<Instruction::Stream>(instructions[i].op2()) == Instruction::Stream::stdout) ? stdout_
                                                                                          : stderr_;

  std::visit(Overloaded{
               [&os](std::string_view sv) { os << sv << '\n'; },
               [&os](Number n) {
                 n.debug(os);
                 os << '\n';
               },
               [&os](Variable v) { os << v << '\n'; },
               [&os](Array a) { os << a << '\n'; },
               [&os](ArrayElement const& ae) {
                 os << ae.first << '[';
                 ae.second.debug(os);
                 os << ']' << '\n';
               },
               [&os](Ibase) { os << "ibase\n"; },
               [&os](Obase) { os << "obase\n"; },
               [&os](Scale) { os << "scale\n"; },
             },
             instructions[expr].result());
}

void GD::Bc::VM::execute_quit(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::quit);
  ::exit(std::get<unsigned>(instructions[i].op1()));
}
