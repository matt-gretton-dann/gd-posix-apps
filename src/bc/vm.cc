/** \file   vm.cc
 *  \brief  Implementation of GD::BC::VM and related classes
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/bits/defines.h"
#include "gd/signal.h"

#include "bc-messages.hh"

#include <array>
#include <assert.h>
#include <ostream>

#include "bc.hh"
#include "number.hh"

#define STRINGIFY2(a) #a
#define STRINGIFY(a) STRINGIFY2(a)

__DISABLE_NARROWING_WARNING

/* Assert an error.
 * To work around some awkward compilers ther first ... argument is a message ID. */
#define assert_error(TEST, ...)                                                                    \
  if (!(TEST)) {                                                                                   \
    error(__func__, __FILE__, __LINE__, STRINGIFY(TEST), __VA_ARGS__);                             \
  }

namespace GD::Bc::Details {
/** \brief Hold virtual machine state.
 */
struct VMState
{
  /** \brief  Constructor.  */
  VMState(std::ostream& out, std::ostream& err);

  /** \brief  Get the appropriate output stream for \a stream.  */
  std::ostream& stream(Instruction::Stream stream) const;

  /** \brief  Set the input base.  */
  void ibase(Number num);

  /** Get the input base.  */
  Number::NumType ibase() const;

  /** \brief  Set the output base.  */
  void obase(Number num);

  /** Get the output base.  */
  Number::NumType obase() const;

  /** \brief  Set the scale.  */
  void scale(Number num);

  /** Get the scale base.  */
  Number::NumType scale() const;

  /** \brief  Get the number stored in the array element ae.  */
  Number array_element(ArrayElement const& ae) const;

  /** \brief  Store a number in an array eleemnt.  */
  void array_element(ArrayElement const& ae, Number num);

  Number const& variable(Variable v) const;
  void variable(Variable v, Number const& num);

  ArrayValues array(Array a) const;
  void array(Array a, ArrayValues av);

  void function(Letter func, Instructions::const_iterator begin, Instructions::const_iterator end,
                VariableMask mask, Location const& loc);

  /** \brief       Do a call
   *  \param  func Function to call.
   *  \param  loc  Location of call.
   *  \return      Result of call.
   */
  Number call(Letter func, Location const& loc);

  /** Push a new parameter pack onto the parameter stack.  */
  void push_param_pack();

  /** Push a scalar parameter into the current parameter pack.  */
  void push_param(Number const& num);

  /** Push a vector parameter into the current parameter pack.  */
  void push_param(ArrayValues av);

  /** Pop the current parameter pack.  */
  void pop_param_pack();

  /** \brief  Pop a scalar param.
   *  \return Value of parameter
   */
  Number pop_param();

  /** \brief  Pop a scalar param.
   *  \return Value of parameter
   */
  ArrayValues pop_param_array();

  /** \brief  Validate an instruction vectore.  */
  void validate(Instructions const& instrs) const;

private:
  template<typename... Ts>
  [[noreturn]] void error(char const* func, char const* file, unsigned line, char const* test,
                          Msg msg, Ts... args) const
  {
    error_ << Messages::get().format(Set::bc, Msg::internal_error, func, file, line, test) << '\n'
           << Messages::get().format(Set::bc, msg, args...) << '\n';
    ::exit(1);
  }

  using Param = std::variant<ArrayValues, Number>;  ///< Parameter to a function
  using Params = std::list<Param>;                  ///< List of parameters
  using ParamStack = std::list<Params>;             ///< Stack of current function parameters
  using Index = Instruction::Index;                 ///< Index into the instructions
  using Offset = Instruction::Offset;               ///< Offset from current instruction
  using NumType = Number::NumType;                  ///< Number type
  using FunctionDefinition =
    std::tuple<Instructions, VariableMask, Location>;  ///< Function definition.

  std::ostream& output_;                            ///< Stream for "normal output."
  std::ostream& error_;                             ///< Stream for error output.
  std::array<ArrayValues, Letter::count_> arrays_;  ///< Array of arrays, indexed by Letter.
  std::array<std::optional<FunctionDefinition>, Letter::count_>
    functions_;  ///< Array of (optional) functions, indexed by letter.
  std::array<Number, Letter::count_> variables_;  ///< Array of scalar variables, indexed by Letter.
  ParamStack param_stack_;                        ///< Stack of parameters
  ParamStack local_stack_;                        ///< Stack of locals.
  NumType ibase_ = 10;                            ///< Input base, range [2, 16]
  NumType obase_ = 10;                            ///< Output base, range: [2, base_)
  NumType scale_ = 0;                             ///< Scale, range: [0, base_)
};

/** \brief  An executable instruction pack.
 *
 * This holds a set of instructions to be executed along with the results and a current program
 * counter.
 */
struct InstructionPack
{
public:
  /** Valid result types for an instruction:
   *
   * Number: A number
   * string_view: a string, legal as it should only point to the string in op1().
   * Variable: Variable name
   * Array: Array name
   * ArrayElement: Array element.
   */
  using Index = Instruction::Index;
  using Result = std::variant<Number, ArrayValues, std::string_view, Variable, Array, ArrayElement,
                              Ibase, Obase, Scale>;

  /** \brief        Constructor
   *  \param vm     VM state to modify
   *  \param instrs Instructions to hold.
   */
  InstructionPack(VMState* vm, Instructions const& instrs);

  /** \brief  Execute the instruction pack
   *  \return Pair of returned value, and whether we should execute more instruction packs.
   */
  std::pair<Number, bool> execute();

private:
  friend std::ostream& operator<<(std::ostream& os, InstructionPack const& instrs);

  template<typename... Ts>
  [[noreturn]] void error(char const* func, char const* file, unsigned line, char const* test,
                          Msg msg, Ts... args) const
  {
    vm_->stream(Instruction::Stream::error)
      << Messages::get().format(Set::bc, Msg::internal_error, func, file, line, test) << '\n'
      << Messages::get().format(Set::bc, Msg::instruction_header) << '\n'
      << *this << Messages::get().format(Set::bc, msg, args...) << '\n';
    ::exit(1);
  }

  /** Execute print instruction.  */
  void execute_print();

  /** Execute quit instruction.  */
  void execute_quit();

  /** Execute load instruction.  */
  void execute_load();

  /** Execute store instruction.  */
  void execute_store();

  /** Execute branch instruction.  Returns index of next instruction to execute.  */
  Index execute_branch();

  /** Execute branch_zero instruction.  Returns index of next instruction to execute.  */
  Index execute_branch_zero();

  /** Execute function_begin instruction.  Returns index of next instruction to execute.  */
  Index execute_function_begin();

  /** Execute the return instruction. Returns value to return.  */
  Number execute_return();

  /** Execute push_param_mark instruction.  */
  void execute_push_param_mark();

  /** Execute push_param instruction.  */
  void execute_push_param();

  /** Execute pop_param_mark instruction.  */
  void execute_pop_param_mark();

  /** \brief        Execute an instruction with no operands, but which sets a result.
   *  \param f      Function to call to do the execution
   *
   * \a f should take no parameters and return a value castable to Result.
   */
  template<typename Fn>
  void execute_nonary(Fn f)
  {
    results_[pc_] = f();
  }

  /** \brief        Execute an instruction with one operand, and which sets a result.
   *  \param f      Function to call to do the execution
   *
   * \a f should take one Operand parameter and return a value castable to Result.
   *
   * If both the operand is an Offset to an expression result and the result is a Number then
   * use `execute_unary_op()` instead.
   */
  template<typename Fn>
  void execute_unary(Fn f)
  {
    results_[pc_] = f(instrs_[pc_].op1());
  }

  /** \brief              Execute an instruction with two operands, and which sets a result.
   *  \param f            Function to call to do the execution
   *
   * \a f should have a signature like `Result f(Operand const& op1, Operand const& op2)`
   *
   * If both of the operands is an Offset to an expression result and the result is a Number then
   * use `execute_binary_op()` instead.
   */
  template<typename Fn>
  void execute_binary(Fn f)
  {
    results_[pc_] = f(instrs_[pc_].op1(), instrs_[pc_].op2());
  }

  /** \brief   Execute an instruction with one expression operand, and has a Number result.
   *  \param f Function to call to do the execution
   *
   * \a f should have a signature like `void f(Number& op)`.  It should update op with the result.
   */
  template<typename Fn>
  void execute_unary_op(Fn f)
  {
    Number lhs = get_op1_expr();
    f(lhs);
    results_[pc_] = std::move(lhs);
  }

  /** \brief   Execute an instruction with two expression operands, and has a Number result.
   *  \param f Function to call to do the execution
   *
   * \a f should have a signature like `void f(Number& op1, Number const& op2)`.  It should update
   * op1 with the result.
   */
  template<typename Fn>
  void execute_binary_op(Fn f)
  {
    Number lhs = get_op1_expr();
    Number const& rhs = get_op2_expr();
    f(lhs, rhs);
    results_[pc_] = std::move(lhs);
  }

  /** \brief  Conver the offset in the index  */
  Index get_offset_index(Instruction::Operand const& op) const;

  /** \brief  Get the value pointed to by op relative to the current PC.  */
  Number const& get_op_expr(Instruction::Operand const& op) const;

  /** \brief Get the value pointed to by op1 of the current instruction.  */
  Number const& get_op1_expr() const;

  /** \brief Get the value pointed to by op2 of the current instruction.  */
  Number const& get_op2_expr() const;

  /** Validate the result in the current pc. */
  void validate_result(Instructions::size_type i) const;

  VMState* vm_;                                 ///< Virtual Machine state
  Instructions const& instrs_;                  ///< Instructions to execute
  Instruction::Index pc_;                       ///< Current program counter
  std::vector<std::optional<Result>> results_;  ///< Results of instructions.
};

std::ostream& operator<<(std::ostream& os, InstructionPack const& instrs);
std::ostream& operator<<(std::ostream& os, InstructionPack::Result const& result);

/** Interrupt handler globals */
sig_atomic_t have_been_interrupted = false;

/** Handle receiving SIGINT.  */
__EXTERN_C void handle_sigint(int) { have_been_interrupted = true; }

/** Install the signal handler for SIGINT.  */
void install_interrupt_handler()
{
  have_been_interrupted = false;
  struct sigaction sa;
  sa.sa_handler = handle_sigint;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_RESTART | SA_RESETHAND; /* Restart functions if interrupted by handler.  */
  /* We don't care if this fails as users will still be able to ^C out but just with a worse
   * experience.  */
  sigaction(SIGINT, &sa, nullptr);
}

/* Reset the SIGINT signal handler.  */
void reset_interrupt_handler()
{
  struct sigaction sa;
  sa.sa_handler = SIG_DFL;
  sigemptyset(&sa.sa_mask);
  sigaction(SIGINT, &sa, nullptr);
  // We assume this has worked for usefulness sake.
}

}  // namespace GD::Bc::Details

GD::Bc::Details::VMState::VMState(std::ostream& out, std::ostream& err) : output_(out), error_(err)
{
}

std::ostream& GD::Bc::Details::VMState::stream(Instruction::Stream stream) const
{
  return stream == Instruction::Stream::output ? output_ : error_;
}

void GD::Bc::Details::VMState::push_param(Number const& n)
{
  assert_error(!param_stack_.empty(), Msg::parameter_stack_empty);
  param_stack_.back().push_back(n);
}

void GD::Bc::Details::VMState::push_param(ArrayValues av)
{
  assert_error(!param_stack_.empty(), Msg::parameter_stack_empty);
  param_stack_.back().push_back(av);
}

void GD::Bc::Details::VMState::push_param_pack() { param_stack_.push_back(Params{}); }

void GD::Bc::Details::VMState::pop_param_pack()
{
  assert_error(!param_stack_.empty(), Msg::parameter_stack_empty);
  assert_error(param_stack_.back().empty(), Msg::parameter_pack_not_empty);
  param_stack_.pop_back();
}

GD::Bc::Number GD::Bc::Details::VMState::pop_param()
{
  assert_error(!param_stack_.empty(), Msg::parameter_stack_empty);
  assert_error(!param_stack_.back().empty(), Msg::parameter_pack_empty);
  auto result(std::get<Number>(param_stack_.back().front()));
  param_stack_.back().pop_front();
  return result;
}

GD::Bc::ArrayValues GD::Bc::Details::VMState::pop_param_array()
{
  assert_error(!param_stack_.empty(), Msg::parameter_stack_empty);
  assert_error(!param_stack_.back().empty(), Msg::parameter_pack_empty);
  auto result(std::get<ArrayValues>(param_stack_.back().front()));
  param_stack_.back().pop_front();
  return result;
}

void GD::Bc::Details::VMState::function(Letter func, Instructions::const_iterator begin,
                                        Instructions::const_iterator end, VariableMask mask,
                                        Location const& loc)
{
  Instructions instrs(begin, end);
  validate(instrs);
  functions_[static_cast<unsigned>(func)] =
    std::make_optional(std::make_tuple(std::move(instrs), mask, loc));
}

GD::Bc::Number GD::Bc::Details::VMState::call(Letter func, Location const& loc)
{
  if (!functions_[static_cast<unsigned>(func)].has_value()) {
    Details::error(Msg::function_not_defined, func, loc.file_name(), loc.line(), loc.column());
  }

  FunctionDefinition const& def = functions_[static_cast<unsigned>(func)].value();
  auto func_instructions = std::get<0>(def);
  auto locals = std::get<1>(def);

  /* Save the locals.  */
  Params p;
  locals.for_each_variable([&p, this](Letter l) {
    p.push_back(variables_[static_cast<unsigned>(l)]);
    variables_[static_cast<unsigned>(l)] = Number(0);
  });
  locals.for_each_array([&p, this](Letter l) {
    p.push_back(arrays_[static_cast<unsigned>(l)]);
    arrays_[static_cast<unsigned>(l)].reset();
  });
  local_stack_.push_back(p);

  InstructionPack fn(this, func_instructions);
  auto [result, cont] = fn.execute();

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

  return result;
}

void GD::Bc::Details::VMState::ibase(Number num)
{
  NumType n = num.to_unsigned();
  if (n < 2 || n > 16) {
    Details::error(Msg::ibase_out_of_range, n);
  }
  ibase_ = n;
}

GD::Bc::Number::NumType GD::Bc::Details::VMState::ibase() const { return ibase_; }

void GD::Bc::Details::VMState::obase(Number num)
{
  NumType n = num.to_unsigned();
  if (n < 2) {
    Details::error(Msg::obase_out_of_range, n, Number::base_);
  }
  obase_ = num.to_unsigned();
}

GD::Bc::Number::NumType GD::Bc::Details::VMState::obase() const { return obase_; }

void GD::Bc::Details::VMState::scale(Number num) { scale_ = num.to_unsigned(); }

GD::Bc::Number::NumType GD::Bc::Details::VMState::scale() const { return scale_; }

void GD::Bc::Details::VMState::array_element(ArrayElement const& ae, Number num)
{
  ArrayValues a = arrays_[static_cast<unsigned>(ae.first.get())];
  if (!a) {
    arrays_[static_cast<unsigned>(ae.first.get())] = a =
      std::make_shared<ArrayValues::element_type>();
  }

  a->insert_or_assign(ae.second, num);
}

GD::Bc::Number GD::Bc::Details::VMState::array_element(ArrayElement const& ae) const
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

GD::Bc::Number const& GD::Bc::Details::VMState::variable(Variable v) const
{
  return variables_[static_cast<unsigned>(v.get())];
}

void GD::Bc::Details::VMState::variable(Variable v, Number const& num)
{
  variables_[static_cast<unsigned>(v.get())] = num;
}

GD::Bc::ArrayValues GD::Bc::Details::VMState::array(Array a) const
{
  return arrays_[static_cast<unsigned>(a.get())];
}

void GD::Bc::Details::VMState::array(Array a, ArrayValues av)
{
  arrays_[static_cast<unsigned>(a.get())] = av;
}

void GD::Bc::Details::VMState::validate(Instructions const& instrs) const
{
  for (Instructions::size_type i = 0; i < instrs.size(); ++i) {
    if (instrs[i].has_op1()) {
      auto const& op = instrs[i].op1();
      if (std::holds_alternative<Instruction::Offset>(op)) {
        Instruction::Offset offset = std::get<Instruction::Offset>(op);
        assert_error(offset >= 0 || static_cast<Instruction::Index>(-offset) <= i,
                     Msg::op1_offset_underflow, i, offset);
        assert_error(offset < 0 || static_cast<Instruction::Index>(offset) <= instrs.size() - i,
                     Msg::op1_offset_overflow, i, offset);
      }
    }
    if (instrs[i].has_op2()) {
      auto const& op = instrs[i].op1();
      if (std::holds_alternative<Instruction::Offset>(op)) {
        Instruction::Offset offset = std::get<Instruction::Offset>(op);
        assert_error(offset >= 0 || static_cast<Instruction::Index>(-offset) <= i,
                     Msg::op2_offset_underflow, i, offset);
        assert_error(offset < 0 || static_cast<Instruction::Index>(offset) <= instrs.size() - i,
                     Msg::op2_offset_overflow, i, offset);
      }
    }
  }
}

GD::Bc::Details::InstructionPack::InstructionPack(VMState* vm, Instructions const& instrs)
    : vm_(vm), instrs_(instrs), pc_(0), results_(instrs.size())
{
  assert(vm_ != nullptr);
}

std::pair<GD::Bc::Number, bool> GD::Bc::Details::InstructionPack::execute()
{
  for (; pc_ < instrs_.size(); ++pc_) {
    if (Details::have_been_interrupted) {
      return std::make_pair(Number(0), true);
    }
    auto i = pc_;
    switch (instrs_[pc_].opcode()) {
    case Instruction::Opcode::string:
      execute_unary(
        [](Instruction::Operand const& o) { return std::string_view(std::get<std::string>(o)); });
      break;
    case Instruction::Opcode::number:
      execute_unary([this](Instruction::Operand const& o) {
        return Number(std::get<std::string>(o), vm_->ibase());
      });
      break;
    case Instruction::Opcode::variable:
      execute_unary([](Instruction::Operand const& o) { return std::get<Variable>(o); });
      break;
    case Instruction::Opcode::array:
      execute_unary([](Instruction::Operand const& o) { return std::get<Array>(o); });
      break;
    case Instruction::Opcode::array_element:
      execute_binary([this](Instruction::Operand const& o1, Instruction::Operand const& o2) {
        return ArrayElement(std::get<Array>(o1), get_op_expr(o2).to_unsigned());
      });
      break;
    case Instruction::Opcode::ibase:
      execute_nonary([]() { return Ibase(); });
      break;
    case Instruction::Opcode::obase:
      execute_nonary([]() { return Obase(); });
      break;
    case Instruction::Opcode::scale:
      execute_nonary([]() { return Scale(); });
      break;
    case Instruction::Opcode::print:
      execute_print();
      break;
    case Instruction::Opcode::quit:
      execute_quit();
      break;
    case Instruction::Opcode::load:
      execute_load();
      break;
    case Instruction::Opcode::store:
      execute_store();
      break;
    case Instruction::Opcode::negate:
      execute_unary_op([](Number& lhs) { lhs.negate(); });
      break;
    case Instruction::Opcode::add:
      execute_binary_op([](Number& lhs, Number const& rhs) { lhs.add(rhs); });
      break;
    case Instruction::Opcode::subtract:
      execute_binary_op([](Number& lhs, Number const& rhs) { lhs.sub(rhs); });
      break;
    case Instruction::Opcode::power:
      execute_binary_op([this](Number& lhs, Number const& rhs) { lhs.power(rhs, vm_->scale()); });
      break;
    case Instruction::Opcode::multiply:
      execute_binary_op(
        [this](Number& lhs, Number const& rhs) { lhs.multiply(rhs, vm_->scale()); });
      break;
    case Instruction::Opcode::divide:
      execute_binary_op([this](Number& lhs, Number const& rhs) { lhs.divide(rhs, vm_->scale()); });
      break;
    case Instruction::Opcode::modulo:
      execute_binary_op([this](Number& lhs, Number const& rhs) { lhs.modulo(rhs, vm_->scale()); });
      break;
    case Instruction::Opcode::sqrt:
      execute_unary_op([this](Number& lhs) { lhs.sqrt(vm_->scale()); });
      break;
    case Instruction::Opcode::scale_expr:
      execute_unary_op([](Number& lhs) { lhs = Number(lhs.scale()); });
      break;
    case Instruction::Opcode::length:
      execute_unary_op([](Number& lhs) { lhs = Number(lhs.length()); });
      break;
    case Instruction::Opcode::less_than:
      execute_binary_op([](Number& lhs, Number const& rhs) { lhs = Number(lhs < rhs ? 1 : 0); });
      break;
    case Instruction::Opcode::less_than_equals:
      execute_binary_op([](Number& lhs, Number const& rhs) { lhs = Number(lhs <= rhs ? 1 : 0); });
      break;
    case Instruction::Opcode::equals:
      execute_binary_op([](Number& lhs, Number const& rhs) { lhs = Number(lhs == rhs ? 1 : 0); });
      break;
    case Instruction::Opcode::not_equals:
      execute_binary_op([](Number& lhs, Number const& rhs) { lhs = Number(lhs != rhs ? 1 : 0); });
      break;
    case Instruction::Opcode::branch:
      pc_ = execute_branch() - 1;
      break;
    case Instruction::Opcode::branch_zero:
      pc_ = execute_branch_zero() - 1;
      break;
    case Instruction::Opcode::function_begin:
      pc_ = execute_function_begin() - 1;
      break;
    case Instruction::Opcode::push_param_mark:
      execute_push_param_mark();
      break;
    case Instruction::Opcode::push_param:
      execute_push_param();
      break;
    case Instruction::Opcode::pop_param_mark:
      execute_pop_param_mark();
      break;
    case Instruction::Opcode::pop_param:
      execute_nonary([this]() { return vm_->pop_param(); });
      break;
    case Instruction::Opcode::pop_param_array:
      execute_nonary([this]() { return vm_->pop_param_array(); });
      break;
    case Instruction::Opcode::call:
      execute_binary([this](Instruction::Operand const& o1, Instruction::Operand const& o2) {
        return vm_->call(std::get<Letter>(o1), std::get<Location>(o2));
      });
      break;
    case Instruction::Opcode::return_:
      return std::make_pair(get_op1_expr(), true);
    case Instruction::Opcode::eof:
      assert(pc_ == instrs_.size() - 1);
      return std::make_pair(Number(0), false);
      break;
    case Instruction::Opcode::function_end: /* Should never been seen here.  */
      abort();
      break;
    }

    /* Validate the results.  */
    validate_result(i);
  }

  /* Fall off the end assume 'return (0);' */
  return std::make_pair(Number(0), true);
}

void GD::Bc::Details::InstructionPack::execute_print()
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::print);
  Index result_idx = get_offset_index(instrs_[pc_].op1());
  std::ostream& os = vm_->stream(std::get<Instruction::Stream>(instrs_[pc_].op2()));
  auto& result = results_[result_idx];
  assert_error(result.has_value(), Msg::empty_result, result_idx);
  std::visit(Overloaded{
               [&os](std::string_view sv) { os << sv; },
               [&os, this](Number n) {
                 n.output(os, vm_->obase());
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
             *result);
}

void GD::Bc::Details::InstructionPack::execute_quit()
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::quit);
  ::exit(std::get<unsigned>(instrs_[pc_].op1()));
}

void GD::Bc::Details::InstructionPack::execute_load()
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::load);
  Index loc_idx = get_offset_index(instrs_[pc_].op1());
  auto& result = results_[pc_];
  auto const& loc = results_[loc_idx];
  assert_error(loc.has_value(), Msg::empty_result, loc_idx);

  std::visit(Overloaded{
               [this](std::string_view) { assert_error(false, Msg::cannot_load, "string_view"); },
               [this](Number) { assert_error(false, Msg::cannot_load, "Number"); },
               [this](ArrayValues const&) { assert_error(false, Msg::cannot_load, "ArrayValues"); },
               [&result, this](Variable v) { result = vm_->variable(v); },
               [&result, this](Array a) { result = vm_->array(a); },
               [&result, this](ArrayElement const& ae) { result = vm_->array_element(ae); },
               [&result, this](Ibase) { result = Number(vm_->ibase()); },
               [&result, this](Obase) { result = Number(vm_->obase()); },
               [&result, this](Scale) { result = Number(vm_->scale()); },
             },
             *loc);
}

void GD::Bc::Details::InstructionPack::execute_store()
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::store);
  Index loc_idx = get_offset_index(instrs_[pc_].op1());
  auto& loc = results_[loc_idx];
  assert_error(loc.has_value(), Msg::empty_result, loc_idx);
  Index expr_idx = get_offset_index(instrs_[pc_].op2());
  auto& result = results_[expr_idx];
  assert_error(result.has_value(), Msg::empty_result, expr_idx);
  auto& expr = *result;

  std::visit(
    Overloaded{
      [this](std::string_view) { assert_error(false, Msg::cannot_store, "string_view"); },
      [this](Number) { assert_error(false, Msg::cannot_store, "Number"); },
      [this](ArrayValues const&) { assert_error(false, Msg::cannot_store, "ArrayValues"); },
      [&expr, this](Variable v) { vm_->variable(v, std::get<Number>(expr)); },
      [&expr, this](Array a) { vm_->array(a, std::get<ArrayValues>(expr)); },
      [&expr, this](ArrayElement const& ae) { vm_->array_element(ae, std::get<Number>(expr)); },
      [&expr, this](Ibase) { vm_->ibase(std::get<Number>(expr)); },
      [&expr, this](Obase) { vm_->obase(std::get<Number>(expr)); },
      [&expr, this](Scale) { vm_->scale(std::get<Number>(expr)); },
    },
    *loc);
}

GD::Bc::Instruction::Index GD::Bc::Details::InstructionPack::execute_branch()
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::branch);
  return get_offset_index(instrs_[pc_].op1());
}

GD::Bc::Instruction::Index GD::Bc::Details::InstructionPack::execute_branch_zero()
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::branch_zero);
  Number c = get_op1_expr();
  Index dest_idx = get_offset_index(instrs_[pc_].op2());
  return c.is_zero() ? dest_idx : pc_ + 1;
}

GD::Bc::Instruction::Index GD::Bc::Details::InstructionPack::execute_function_begin()
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::function_begin);
  VariableMask mask = std::get<VariableMask>(instrs_[pc_].op1());
  Location loc = std::get<Location>(instrs_[pc_].op2());
  [[maybe_unused]] Index start = pc_++;
  while (pc_ != instrs_.size() && instrs_[pc_].opcode() != Instruction::Opcode::function_end) {
    ++pc_;
  }
  assert_error(pc_ != instrs_.size(), Msg::no_end_to_function_definition, loc.file_name(),
               loc.line(), loc.column());

  Letter func = std::get<Letter>(instrs_[pc_].op1());
  Index given_start = get_offset_index(instrs_[pc_].op2());
  assert_error(start == given_start, Msg::bad_function_definition);

  vm_->function(func, instrs_.begin() + start + 1, instrs_.begin() + pc_, mask, loc);
  return pc_ + 1;
}

void GD::Bc::Details::InstructionPack::execute_push_param_mark()
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::push_param_mark);
  vm_->push_param_pack();
}

void GD::Bc::Details::InstructionPack::execute_push_param()
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::push_param);
  Index expr_idx = get_offset_index(instrs_[pc_].op1());
  auto& expr = results_[expr_idx];
  assert_error(expr.has_value(), Msg::empty_result, expr_idx);

  std::visit(Overloaded{
               [this](Number const& n) { vm_->push_param(n); },
               [this](ArrayValues const& av) { vm_->push_param(av); },
               []([[maybe_unused]] auto a) { assert(false); },
             },
             *expr);
}

void GD::Bc::Details::InstructionPack::execute_pop_param_mark()
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::pop_param_mark);
  vm_->pop_param_pack();
}

GD::Bc::Instruction::Index
GD::Bc::Details::InstructionPack::get_offset_index(Instruction::Operand const& op) const
{
  auto offset = std::get<Instruction::Offset>(op);
  /* We know the following is safe as we checked in validate().  */
  Index expr_idx = pc_ + offset;
  return expr_idx;
}

GD::Bc::Number const&
GD::Bc::Details::InstructionPack::get_op_expr(Instruction::Operand const& op) const
{
  auto const& result = results_[get_offset_index(op)];
  assert_error(result.has_value(), Msg::empty_result, get_offset_index(op));
  return std::get<Number>(*result);
}

GD::Bc::Number const& GD::Bc::Details::InstructionPack::get_op1_expr() const
{
  return get_op_expr(instrs_[pc_].op1());
}

GD::Bc::Number const& GD::Bc::Details::InstructionPack::get_op2_expr() const
{
  return get_op_expr(instrs_[pc_].op2());
}

std::ostream& GD::Bc::Details::operator<<(std::ostream& os,
                                          GD::Bc::Details::InstructionPack::Result const& result)
{
  std::visit(Overloaded{
               [&os](std::string_view s) { os << "String(" << s << ')'; },
               [&os](Number n) { n.debug(os); },
               [&os](Variable v) { os << v; },
               [&os](Array a) { os << a << "[]"; },
               [&os](ArrayElement const& ae) { os << ae.first << '[' << ae.second << ']'; },
               [&os](ArrayValues const&) { os << "<ARRAY VALUES>"; },
               [&os](Ibase) { os << "ibase"; },
               [&os](Obase) { os << "obase"; },
               [&os](Scale) { os << "scale"; },
             },
             result);
  return os;
}

std::ostream& GD::Bc::Details::operator<<(std::ostream& os,
                                          GD::Bc::Details::InstructionPack const& instrs)
{
  for (::size_t i = 0; i < instrs.instrs_.size(); ++i) {
    os << i;
    if (i == instrs.pc_) {
      os << "**";
    }
    os << '\t' << instrs.instrs_[i];
    if (instrs.results_[i].has_value()) {
      os << " = " << *(instrs.results_[i]);
    }
    os << '\n';
  }
  return os;
}

void GD::Bc::Details::InstructionPack::validate_result(Index i) const
{
  [[maybe_unused]] auto const& result = results_[i];
  switch (instrs_[i].opcode()) {
  case GD::Bc::Instruction::Opcode::eof:
  case GD::Bc::Instruction::Opcode::push_param_mark:
  case GD::Bc::Instruction::Opcode::pop_param_mark:
  case GD::Bc::Instruction::Opcode::quit:
  case GD::Bc::Instruction::Opcode::branch:
  case GD::Bc::Instruction::Opcode::push_param:
  case GD::Bc::Instruction::Opcode::print:
  case GD::Bc::Instruction::Opcode::function_end:
  case GD::Bc::Instruction::Opcode::store:
  case GD::Bc::Instruction::Opcode::branch_zero:
  case GD::Bc::Instruction::Opcode::function_begin:
    assert_error(!result.has_value(), Msg::non_empty_result, i);
    break;
  case GD::Bc::Instruction::Opcode::scale:
    assert_error(result.has_value(), Msg::empty_result, i);
    assert_error(std::holds_alternative<Scale>(*result), Msg::wrong_result_type, i, "Scale");
    break;
  case GD::Bc::Instruction::Opcode::ibase:
    assert_error(result.has_value(), Msg::empty_result, i);
    assert_error(std::holds_alternative<Ibase>(*result), Msg::wrong_result_type, i, "Ibase");
    break;
  case GD::Bc::Instruction::Opcode::obase:
    assert_error(result.has_value(), Msg::empty_result, i);
    assert_error(std::holds_alternative<Obase>(*result), Msg::wrong_result_type, i, "Obase");
    break;
  case GD::Bc::Instruction::Opcode::string:
    assert_error(result.has_value(), Msg::empty_result, i);
    assert_error(std::holds_alternative<std::string_view>(*result), Msg::wrong_result_type, i,
                 "string_view");

    break;
  case GD::Bc::Instruction::Opcode::number:
  case GD::Bc::Instruction::Opcode::negate:
  case GD::Bc::Instruction::Opcode::load:
  case GD::Bc::Instruction::Opcode::scale_expr:
  case GD::Bc::Instruction::Opcode::sqrt:
  case GD::Bc::Instruction::Opcode::length:
  case GD::Bc::Instruction::Opcode::return_:
  case GD::Bc::Instruction::Opcode::add:
  case GD::Bc::Instruction::Opcode::subtract:
  case GD::Bc::Instruction::Opcode::multiply:
  case GD::Bc::Instruction::Opcode::divide:
  case GD::Bc::Instruction::Opcode::modulo:
  case GD::Bc::Instruction::Opcode::power:
  case GD::Bc::Instruction::Opcode::equals:
  case GD::Bc::Instruction::Opcode::less_than_equals:
  case GD::Bc::Instruction::Opcode::not_equals:
  case GD::Bc::Instruction::Opcode::less_than:
  case GD::Bc::Instruction::Opcode::call:
  case GD::Bc::Instruction::Opcode::pop_param:
    assert_error(result.has_value(), Msg::empty_result, i);
    assert_error(std::holds_alternative<Number>(*result), Msg::wrong_result_type, i, "Number");
    break;
  case GD::Bc::Instruction::Opcode::variable:
    assert_error(result.has_value(), Msg::empty_result, i);
    assert_error(std::holds_alternative<Variable>(*result), Msg::wrong_result_type, i, "Variable");
    break;
  case GD::Bc::Instruction::Opcode::array:
  case GD::Bc::Instruction::Opcode::pop_param_array:
    assert_error(result.has_value(), Msg::empty_result, i);
    assert_error(std::holds_alternative<Array>(*result), Msg::wrong_result_type, i, "Array");
    break;
  case GD::Bc::Instruction::Opcode::array_element:
    assert_error(result.has_value(), Msg::empty_result, i);
    assert_error(std::holds_alternative<ArrayElement>(*result), Msg::wrong_result_type, i,
                 "ArrayElement");
    break;
  }
}

GD::Bc::VM::VM(std::ostream& out, std::ostream& err) : state_(new Details::VMState(out, err)) {}

bool GD::Bc::VM::execute(Instructions& instructions)
{
  state_->validate(instructions);
  Details::InstructionPack instrs(state_, instructions);
  /* We want a way for people to interrupt execution - so install a SIGINT handler
   * but only whilst executing instructions.
   */
  Details::install_interrupt_handler();
  auto result = instrs.execute();
  Details::reset_interrupt_handler();
  if (Details::have_been_interrupted) {
    state_->stream(Instruction::Stream::error) << Messages::get().format(Msg::interrupted) << '\n';
    Details::have_been_interrupted = false;
  }
  return result.second;
}
