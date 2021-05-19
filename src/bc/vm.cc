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
      execute_unary(instructions, i, [](Instruction::Operand const& o) {
        return std::string_view(std::get<std::string>(o));
      });
      break;
    case Instruction::Opcode::number:
      execute_unary(instructions, i, [this](Instruction::Operand const& o) {
        return Number(std::get<std::string>(o), ibase_);
      });
      break;
    case Instruction::Opcode::variable:
      execute_unary(instructions, i,
                    [this](Instruction::Operand const& o) { return std::get<Variable>(o); });
      break;
    case Instruction::Opcode::array:
      execute_unary(instructions, i,
                    [this](Instruction::Operand const& o) { return std::get<Array>(o); });
      break;
    case Instruction::Opcode::array_element:
      execute_binary(
        instructions, i,
        [this, &instructions, i](Instruction::Operand const& o1, Instruction::Operand const& o2) {
          Number elt = get_op_expr(instructions, i, o2);
          return ArrayElement(std::get<Array>(o1), elt.to_unsigned());
        });
      break;
    case Instruction::Opcode::ibase:
      execute_nonary(instructions, i, []() { return Ibase(); });
      break;
    case Instruction::Opcode::obase:
      execute_nonary(instructions, i, []() { return Obase(); });
      break;
    case Instruction::Opcode::scale:
      execute_nonary(instructions, i, []() { return Scale(); });
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
      execute_unary_op(instructions, i, [](Number& lhs) { lhs.negate(); });
      break;
    case Instruction::Opcode::add:
      execute_bin_op(instructions, i, [](Number& lhs, Number& rhs) { lhs.add(rhs); });
      break;
    case Instruction::Opcode::subtract:
      execute_bin_op(instructions, i, [](Number& lhs, Number& rhs) { lhs.sub(rhs); });
      break;
    case Instruction::Opcode::power:
      execute_bin_op(instructions, i, [this](Number& lhs, Number& rhs) { lhs.power(rhs, scale_); });
      break;
    case Instruction::Opcode::multiply:
      execute_bin_op(instructions, i,
                     [this](Number& lhs, Number& rhs) { lhs.multiply(rhs, scale_); });
      break;
    case Instruction::Opcode::divide:
      execute_bin_op(instructions, i,
                     [this](Number& lhs, Number& rhs) { lhs.divide(rhs, scale_); });
      break;
    case Instruction::Opcode::modulo:
      execute_bin_op(instructions, i,
                     [this](Number& lhs, Number& rhs) { lhs.modulo(rhs, scale_); });
      break;
    case Instruction::Opcode::sqrt:
      execute_unary_op(instructions, i, [this](Number& lhs) { lhs.sqrt(scale_); });
      break;
    case Instruction::Opcode::scale_expr:
      execute_unary_op(instructions, i, [](Number& lhs) { lhs = Number(lhs.scale()); });
      break;
    case Instruction::Opcode::length:
      execute_unary_op(instructions, i, [](Number& lhs) { lhs = Number(lhs.length()); });
      break;
    case Instruction::Opcode::less_than:
      execute_bin_op(instructions, i,
                     [](Number& lhs, Number& rhs) { lhs = Number(lhs < rhs ? 1 : 0); });
      break;
    case Instruction::Opcode::less_than_equals:
      execute_bin_op(instructions, i,
                     [](Number& lhs, Number& rhs) { lhs = Number(lhs <= rhs ? 1 : 0); });
      break;
    case Instruction::Opcode::equals:
      execute_bin_op(instructions, i,
                     [](Number& lhs, Number& rhs) { lhs = Number(lhs == rhs ? 1 : 0); });
      break;
    case Instruction::Opcode::not_equals:
      execute_bin_op(instructions, i,
                     [](Number& lhs, Number& rhs) { lhs = Number(lhs != rhs ? 1 : 0); });
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
      execute_nonary(instructions, i, [this]() { return do_pop_param(); });
      break;
    case Instruction::Opcode::pop_param_array:
      execute_nonary(instructions, i, [this]() { return do_pop_param_array(); });
      break;
    case Instruction::Opcode::call:
      execute_binary(instructions, i,
                     [this](Instruction::Operand const& o1, Instruction::Operand const& o2) {
                       return do_call(std::get<Letter>(o1), std::get<Location>(o2));
                     });
      break;
    case Instruction::Opcode::return_:
      return std::make_pair(execute_return(instructions, i), true);
    case Instruction::Opcode::eof:
      assert(i == instructions.size() - 1);
      return std::make_pair(Number(0), false);
      break;
    case Instruction::Opcode::function_end: /* Should never been seen here.  */
      abort();
      break;
    }
  }

  return std::make_pair(Number(0), true);
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
  [[maybe_unused]] Offset dist = std::get<Offset>(ite->op2());
  assert(-dist == ite - it + 1);

  functions_[static_cast<unsigned int>(func)] =
    std::make_optional(std::make_tuple(Instructions(it, ite), mask, loc));
  ++ite;
  return ite - instructions.begin();
}

void GD::Bc::VM::execute_push_param_mark([[maybe_unused]] Instructions& instructions,
                                         [[maybe_unused]] Index i)
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

void GD::Bc::VM::execute_pop_param_mark([[maybe_unused]] Instructions& instructions,
                                        [[maybe_unused]] Index i)
{
  assert(instructions.at(i).opcode() == Instruction::Opcode::pop_param_mark);
  assert(!param_stack_.empty());
  assert(param_stack_.back().empty());
  param_stack_.pop_back();
}

GD::Bc::Number GD::Bc::VM::do_pop_param()
{
  assert(!param_stack_.empty());
  assert(!param_stack_.back().empty());
  auto result(std::get<Number>(param_stack_.back().front()));
  param_stack_.back().pop_front();
  return result;
}

GD::Bc::ArrayValues GD::Bc::VM::do_pop_param_array()
{
  assert(!param_stack_.empty());
  assert(!param_stack_.back().empty());
  auto result(std::get<ArrayValues>(param_stack_.back().front()));
  param_stack_.back().pop_front();
  return result;
}

GD::Bc::Number GD::Bc::VM::do_call(Letter func, Location const& loc)
{
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
  return result;
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

GD::Bc::Number GD::Bc::VM::get_op_expr(Instructions const& instructions, Index i,
                                       Instruction::Operand const& op)
{
  auto offset = std::get<Offset>(op);
  assert(offset <= i);
  assert(offset < instructions.size() - i);
  Index expr_idx = i + offset;
  return std::get<Number>(instructions[expr_idx].result());
}

GD::Bc::Number GD::Bc::VM::get_op1_expr(Instructions& instructions, Index i)
{
  return get_op_expr(instructions, i, instructions[i].op1());
}

GD::Bc::Number GD::Bc::VM::get_op2_expr(Instructions& instructions, Index i)
{
  return get_op_expr(instructions, i, instructions[i].op2());
}
