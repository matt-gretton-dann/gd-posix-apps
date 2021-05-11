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

std::pair<GD::Bc::Number, bool> GD::Bc::VM::execute(Instructions& instructions)
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
    case Instruction::Opcode::less_than:
      execute_less_than(instructions, i);
      break;
    case Instruction::Opcode::less_than_equals:
      execute_less_than_equals(instructions, i);
      break;
    case Instruction::Opcode::equals:
      execute_equals(instructions, i);
      break;
    case Instruction::Opcode::not_equals:
      execute_not_equals(instructions, i);
      break;
    case Instruction::Opcode::branch:
      i = execute_branch(instructions, i) - 1;
      break;
    case Instruction::Opcode::branch_zero:
      i = execute_branch_zero(instructions, i) - 1;
      break;
    case Instruction::Opcode::function_begin:
      i = execute_function_begin(instructions, i) - 1;
      break;
    case Instruction::Opcode::push_param_mark:
      execute_push_param_mark(instructions, i);
      break;
    case Instruction::Opcode::push_param:
      execute_push_param(instructions, i);
      break;
    case Instruction::Opcode::pop_param_mark:
      execute_pop_param_mark(instructions, i);
      break;
    case Instruction::Opcode::pop_param:
      execute_pop_param(instructions, i);
      break;
    case Instruction::Opcode::pop_param_array:
      execute_pop_param_array(instructions, i);
      break;
    case Instruction::Opcode::call:
      execute_call(instructions, i);
      break;
    case Instruction::Opcode::return_:
      return std::make_pair(execute_return(instructions, i), true);
    case Instruction::Opcode::eof:
      assert(i == instructions.size() - 1);
      return std::make_pair(Number(0), false);
      break;
    case Instruction::Opcode::function_end: /* Should never been seen here.  */
    default:
      assert(false);
      abort();
      break;
    }
  }

  return std::make_pair(Number(0), true);
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

void GD::Bc::VM::execute_less_than(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::less_than);
  Number lhs = get_op1_expr(instructions, i);
  Number rhs = get_op2_expr(instructions, i);
  instructions[i].result(Number(lhs < rhs ? 1 : 0));
}

void GD::Bc::VM::execute_less_than_equals(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::less_than_equals);
  Number lhs = get_op1_expr(instructions, i);
  Number rhs = get_op2_expr(instructions, i);
  instructions[i].result(Number(lhs <= rhs ? 1 : 0));
}

void GD::Bc::VM::execute_equals(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::equals);
  Number lhs = get_op1_expr(instructions, i);
  Number rhs = get_op2_expr(instructions, i);
  instructions[i].result(Number(lhs == rhs ? 1 : 0));
}

void GD::Bc::VM::execute_not_equals(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::not_equals);
  Number lhs = get_op1_expr(instructions, i);
  Number rhs = get_op2_expr(instructions, i);
  instructions[i].result(Number(lhs != rhs ? 1 : 0));
}

GD::Bc::VM::Index GD::Bc::VM::execute_branch(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::branch);
  Index dest_idx = std::get<Offset>(instructions[i].op1()) + i;
  return dest_idx;
}

GD::Bc::VM::Index GD::Bc::VM::execute_branch_zero(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::branch_zero);
  Number c = get_op1_expr(instructions, i);
  Index dest_idx = std::get<Offset>(instructions[i].op2()) + i;
  return c.is_zero() ? dest_idx : i + 1;
}

GD::Bc::VM::Index GD::Bc::VM::execute_function_begin(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::function_begin);
  VariableMask mask = std::get<VariableMask>(instructions[i].op1());
  Location loc = std::get<Location>(instructions[i].op2());
  auto it = instructions.begin() + i + 1;
  auto ite = it;
  while (ite != instructions.end() && ite->opcode() != Instruction::Opcode::function_end) {
    ++ite;
  }
  if (ite == instructions.end()) {
    Details::error(Msg::no_end_to_function_definition, loc.file_name(), loc.line(), loc.column());
  }
  Letter func = std::get<Letter>(ite->op1());
  Offset dist = std::get<Offset>(ite->op2());
  assert(-dist == ite - it + 1);

  functions_[static_cast<unsigned int>(func)] =
    std::make_optional(std::make_tuple(Instructions(it, ite), mask, loc));
  ++ite;
  return ite - instructions.begin();
}

void GD::Bc::VM::execute_push_param_mark(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::push_param_mark);
  param_stack_.emplace_back(Params{});
}

void GD::Bc::VM::execute_push_param(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::push_param);
  assert(!param_stack_.empty());
  Index expr_idx = std::get<Offset>(instructions[i].op1()) + i;
  assert(expr_idx < instructions.size());
  auto expr = instructions[expr_idx].result();

  std::visit(Overloaded{
               [this](Number n) { param_stack_.back().push_back(n); },
               [this](ArrayValues const& av) { param_stack_.back().push_back(av); },
               []([[maybe_unused]] auto a) { assert(false); },
             },
             expr);
}

void GD::Bc::VM::execute_pop_param_mark(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::pop_param_mark);
  assert(!param_stack_.empty());
  assert(param_stack_.back().empty());
  param_stack_.pop_back();
}

void GD::Bc::VM::execute_pop_param(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::pop_param);
  assert(!param_stack_.empty());
  assert(!param_stack_.back().empty());
  instructions[i].result(std::get<Number>(param_stack_.back().front()));
  param_stack_.back().pop_front();
}

void GD::Bc::VM::execute_pop_param_array(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::pop_param_array);
  assert(!param_stack_.empty());
  assert(!param_stack_.back().empty());
  instructions[i].result(std::get<ArrayValues>(param_stack_.back().front()));
  param_stack_.back().pop_front();
}

void GD::Bc::VM::execute_call(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::call);
  Letter func = std::get<Letter>(instructions[i].op1());
  Location loc = std::get<Location>(instructions[i].op2());

  if (!functions_[static_cast<unsigned>(func)].has_value()) {
    Details::error(Msg::function_not_defined, func, loc.file_name(), loc.line(), loc.column());
  }

  FunctionDefinition const& def = functions_[static_cast<unsigned>(func)].value();
  auto func_instructions = std::get<0>(def);
  auto locals = std::get<1>(def);

  /* Save the locals.  */
  Params p;
  locals.for_each_variable(
    [&p, this](Letter l) { p.push_back(variables_[static_cast<unsigned>(l)]); });
  locals.for_each_array([&p, this](Letter l) { p.push_back(arrays_[static_cast<unsigned>(l)]); });
  local_stack_.push_back(p);

  auto [result, cont] = execute(func_instructions);

  /* Restore the locals. */
  locals.for_each_variable([&p, this](Letter l) {
    variables_[static_cast<unsigned>(l)] = std::get<Number>(p.front());
    p.pop_front();
  });
  locals.for_each_array([&p, this](Letter l) {
    arrays_[static_cast<unsigned>(l)] = std::get<ArrayValues>(p.front());
    p.pop_front();
  });
  local_stack_.pop_back();

  /* Clear results so that we don't have hanging references to data we no longer need.
   */
  std::for_each(func_instructions.begin(), func_instructions.end(),
                [](Instruction& i) { i.clear_result(); });
  instructions[i].result(result);
}

GD::Bc::Number GD::Bc::VM::execute_return(Instructions& instructions, Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::return_);
  return get_op1_expr(instructions, i);
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
