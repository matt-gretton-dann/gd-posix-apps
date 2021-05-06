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

bool GD::Bc::VM::execute(Instructions& instructions)
{
  for (Instruction::Index i = 0; i < instructions.size(); ++i) {
    switch (instructions[i].opcode()) {
    case Instruction::Opcode::string:
      execute_string(instructions, i);
      break;
    case Instruction::Opcode::number:
      execute_number(instructions, i);
      break;
    case Instruction::Opcode::variable:
      execute_variable(instructions, i);
      break;
    case Instruction::Opcode::array:
      execute_array(instructions, i);
      break;
    case Instruction::Opcode::array_element:
      execute_array_element(instructions, i);
      break;
    case Instruction::Opcode::ibase:
      execute_ibase(instructions, i);
      break;
    case Instruction::Opcode::obase:
      execute_obase(instructions, i);
      break;
    case Instruction::Opcode::scale:
      execute_scale(instructions, i);
      break;
    case Instruction::Opcode::print:
      execute_print(instructions, i);
      break;
    case Instruction::Opcode::quit:
      execute_quit(instructions, i);
      break;
    case Instruction::Opcode::load:
      execute_load(instructions, i);
      break;
    case Instruction::Opcode::store:
      execute_store(instructions, i);
      break;
    case Instruction::Opcode::negate:
      execute_negate(instructions, i);
      break;
    case Instruction::Opcode::add:
      execute_add(instructions, i);
      break;
    case Instruction::Opcode::subtract:
      execute_sub(instructions, i);
      break;
    case Instruction::Opcode::eof:
      assert(i == instructions.size() - 1);
      return false;
      break;
    default:
      assert(false);
      break;
    }
  }

  return true;
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

void GD::Bc::VM::execute_variable(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::variable);
  instructions[i].result(std::get<Variable>(instructions[i].op1()));
}

void GD::Bc::VM::execute_array(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::array);
  instructions[i].result(std::get<Array>(instructions[i].op1()));
}

void GD::Bc::VM::execute_array_element(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::array_element);
  Index elt_expr = std::get<Offset>(instructions[i].op2()) + i;
  assert(elt_expr < instructions.size());
  Number elt = std::get<Number>(instructions[elt_expr].result());
  instructions[i].result(ArrayElement(std::get<Array>(instructions[i].op1()), elt.to_unsigned()));
}

void GD::Bc::VM::execute_scale(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::scale);
  instructions[i].result(Scale());
}

void GD::Bc::VM::execute_ibase(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::ibase);
  instructions[i].result(Ibase());
}

void GD::Bc::VM::execute_obase(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::obase);
  instructions[i].result(Obase());
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
               [&os, this](Number n) {
                 n.output(os, obase_);
                 os << '\n';
               },
               [&os](Variable v) { os << v << '\n'; },
               [&os](Array a) { os << a << '\n'; },
               [&os](ArrayElement const& ae) { os << ae.first << '[' << ae.second << "]\n"; },
               [&os](ArrayValues const&) { os << "<ARRAY VALUES>\n"; },
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

void GD::Bc::VM::execute_load(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::load);
  Index from_idx = std::get<Offset>(instructions[i].op1()) + i;
  auto& expr = instructions[i];

  std::visit(
    Overloaded{
      [](std::string_view) { assert(false); },
      [](Number) { assert(false); },
      [](ArrayValues const&) { assert(false); },
      [&expr, this](Variable v) { expr.result(variables_[static_cast<unsigned>(v.get())]); },
      [&expr, this](Array a) { expr.result(arrays_[static_cast<unsigned>(a.get())]); },
      [&expr, this](ArrayElement const& ae) { expr.result(get(ae)); },
      [&expr, this](Ibase) { expr.result(Number(ibase_)); },
      [&expr, this](Obase) { expr.result(Number(obase_)); },
      [&expr, this](Scale) { expr.result(Number(scale_)); },
    },
    instructions[from_idx].result());
}

void GD::Bc::VM::execute_store(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::store);
  Index to = std::get<Offset>(instructions[i].op1()) + i;
  assert(to < instructions.size());
  Index expr_idx = std::get<Offset>(instructions[i].op2()) + i;
  assert(expr_idx < instructions.size());

  auto& expr = instructions[expr_idx];

  std::visit(Overloaded{
               [](std::string_view) { assert(false); },
               [](Number) { assert(false); },
               [](ArrayValues const&) { assert(false); },
               [expr, this](Variable v) {
                 variables_[static_cast<unsigned>(v.get())] = std::get<Number>(expr.result());
               },
               [expr, this](Array a) {
                 arrays_[static_cast<unsigned>(a.get())] = std::get<ArrayValues>(expr.result());
               },
               [expr, this](ArrayElement const& ae) { set(ae, std::get<Number>(expr.result())); },
               [expr, this](Ibase) { set_ibase(std::get<Number>(expr.result())); },
               [expr, this](Obase) { set_obase(std::get<Number>(expr.result())); },
               [expr, this](Scale) { set_scale(std::get<Number>(expr.result())); },
             },
             instructions[to].result());
}

void GD::Bc::VM::execute_negate(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::negate);
  Number n = get_op1_expr(instructions, i);
  n.negate();
  instructions[i].result(n);
}

void GD::Bc::VM::execute_add(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::add);
  Number lhs = get_op1_expr(instructions, i);
  Number rhs = get_op2_expr(instructions, i);
  lhs.add(rhs);
  instructions[i].result(lhs);
}

void GD::Bc::VM::execute_sub(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::subtract);
  Number lhs = get_op1_expr(instructions, i);
  Number rhs = get_op2_expr(instructions, i);
  lhs.sub(rhs);
  instructions[i].result(lhs);
}

void GD::Bc::VM::set_ibase(Number num)
{
  NumType n = num.to_unsigned();
  if (n < 2 || n > 16) {
    Details::error(Msg::base_out_of_range, "ibase", "<NUMBER>");
  }
  ibase_ = n;
}

void GD::Bc::VM::set_obase(Number num) { obase_ = num.to_unsigned(); }

void GD::Bc::VM::set_scale(Number num) { scale_ = num.to_unsigned(); }

void GD::Bc::VM::set(ArrayElement const& ae, Number num)
{
  ArrayValues a = arrays_[static_cast<unsigned>(ae.first.get())];
  if (!a) {
    arrays_[static_cast<unsigned>(ae.first.get())] = a =
      std::make_shared<ArrayValues::element_type>();
  }

  a->insert_or_assign(ae.second, num);
}

GD::Bc::Number GD::Bc::VM::get(ArrayElement const& ae) const
{
  ArrayValues a = arrays_[static_cast<unsigned>(ae.first.get())];
  if (!a) {
    return Number();
  }

  auto it = a->find(ae.second);
  if (it == a->end()) {
    return Number();
  }

  return it->second;
}

GD::Bc::Number GD::Bc::VM::get_op1_expr(Instructions& instructions, Index i)
{
  Index expr_idx = std::get<Offset>(instructions[i].op1()) + i;
  assert(expr_idx < instructions.size());
  return std::get<Number>(instructions[expr_idx].result());
}

GD::Bc::Number GD::Bc::VM::get_op2_expr(Instructions& instructions, Index i)
{
  Index expr_idx = std::get<Offset>(instructions[i].op2()) + i;
  assert(expr_idx < instructions.size());
  return std::get<Number>(instructions[expr_idx].result());
}
