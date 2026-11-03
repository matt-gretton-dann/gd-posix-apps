/** \file   vm.cc
 *  \brief  Implementation of GD::BC::VM and related classes
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/bits/defines.h"
#include "gd/signal.h"

#include "bc-messages.hh"

#include <array>
#include <atomic>
#include <cassert>
#include <cstdlib>
#include <deque>
#include <ostream>
#include <type_traits>
#include <utility>

#include "bc.hh"
#include "execution-plan.hh"
#include "number.hh"
#include "verification.hh"

// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define STRINGIFY2(a) #a
// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define STRINGIFY(a) STRINGIFY2(a)

__DISABLE_NARROWING_WARNING

/* Assert an error. Keep the repository-relative source name stable across
 * independent checkout roots, retaining the actual function and line number.
 * To work around some awkward compilers the first ... argument is a message ID. */
// NOLINTNEXTLINE(cppcoreguidelines-macro-usage)
#define assert_error(TEST, ...)                                                                    \
  if (!(TEST)) {                                                                                   \
    error(__func__, "src/bc/vm.cc", __LINE__, STRINGIFY(TEST), __VA_ARGS__);                             \
  }

namespace GD::Bc::Details {
using ExecutionResult = std::variant<std::monostate, Number, ArrayValues, std::string_view,
                                     Variable, Array, ArrayElement, Ibase, Obase, Scale>;

/** A result slot has one discriminant, including its empty state.
 * The previous optional<variant<...>> needed a separate engagement flag and
 * padding for every instruction, even those that produce no value. Keep the
 * existing checked access protocol while representing emptiness in the variant.
 * Clearing an activation still destroys every owning value before reuse.
 */
class ResultSlot
{
public:
  auto has_value() const noexcept -> bool { return value_.index() != 0; }
  auto operator*() -> ExecutionResult& { return value_; }
  auto operator*() const -> ExecutionResult const& { return value_; }

  template<typename Value>
    requires std::is_assignable_v<ExecutionResult&, Value&&>
  auto operator=(Value&& value) -> ResultSlot&
  {
    value_ = std::forward<Value>(value);
    return *this;
  }

private:
  ExecutionResult value_;
};
using ResultBuffer =
  std::vector<ResultSlot, Profile::Allocator<ResultSlot, Profile::AllocationKind::results>>;

/** A borrowed instruction stream whose entry verification has completed.
 * Construction is confined to the verifier and persistent verified owners.
 * Instruction structure and metadata stay unchanged and outlive execution.
 * Ordinary decoded literal caches can still populate at runtime.
 */
class VerifiedInstructions
{
public:
  auto instructions() const -> Instructions const& { return *instructions_; }
  auto consumption() const -> ConsumptionPlan const& { return *consumption_; }
  auto result_slots() const -> ResultSlotPlan const& { return *result_slots_; }

private:
  friend struct VMState;
  friend class FunctionCode;
  friend class PreparedInstructions;
  VerifiedInstructions(Instructions const& instructions, ConsumptionPlan const& consumption,
                       ResultSlotPlan const& result_slots)
      : instructions_(&instructions), consumption_(&consumption), result_slots_(&result_slots)
  {
  }
  Instructions const* instructions_;
  ConsumptionPlan const* consumption_;
  ResultSlotPlan const* result_slots_;
};

/** Own the outer stream's prepared metadata while borrowing its instructions.
 * Views are formed on demand, so copying/moving this owner never leaves a view
 * pointing into a previous owner's metadata. No prepared view escapes execute().
 */
class PreparedInstructions
{
public:
  auto view() const -> VerifiedInstructions
  {
    return VerifiedInstructions(*instructions_, consumption_, result_slots_);
  }

private:
  friend struct VMState;
  PreparedInstructions(Instructions const& instructions, ConsumptionPlan consumption,
                       ResultSlotPlan result_slots)
      : instructions_(&instructions), consumption_(std::move(consumption)),
        result_slots_(std::move(result_slots))
  {
  }
  Instructions const* instructions_;
  ConsumptionPlan consumption_;
  ResultSlotPlan result_slots_;
};

/** Persistent function code can be constructed only after body verification.
 * Copying/replacing this owner preserves the proof; no mutable code view escapes.
 */
class FunctionCode
{
public:
  FunctionCode(FunctionCode const&) = default;
  FunctionCode(FunctionCode&&) noexcept = default;
  auto operator=(FunctionCode const&) -> FunctionCode& = default;
  auto operator=(FunctionCode&&) noexcept -> FunctionCode& = default;
  auto view() const -> VerifiedInstructions
  {
    return VerifiedInstructions(instructions_, consumption_, result_slots_);
  }

private:
  friend struct VMState;
  FunctionCode(Instructions instructions, ConsumptionPlan consumption, ResultSlotPlan result_slots)
      : instructions_(std::move(instructions)), consumption_(std::move(consumption)),
        result_slots_(std::move(result_slots))
  {
  }
  Instructions instructions_;
  ConsumptionPlan consumption_;
  ResultSlotPlan result_slots_;
};

/** \brief Hold virtual machine state.
 */
struct VMState
{
  /** \brief  Constructor.  */
  VMState(std::ostream& out, std::ostream& err, bool save_specials, VM::ExecutionChecks checks);

  /** \brief  Get the appropriate output stream for \a stream.  */
  [[nodiscard]] auto stream(Instruction::Stream stream) const -> std::ostream&;

  /** \brief  Set the input base.  */
  void ibase(Number const& num);

  /** Get the input base.  */
  [[nodiscard]] auto ibase() const -> Number::NumType;

  /** \brief  Set the output base.  */
  void obase(Number const& num);

  /** Get the output base.  */
  [[nodiscard]] auto obase() const -> Number::NumType;

  /** \brief  Set the scale.  */
  void scale(Number const& num);

  /** Get the scale base.  */
  [[nodiscard]] auto scale() const -> Number::NumType;

#if GD_BC_PREPARED_DIVISORS
  /** Try bounded reuse for heap divisors; scalar dispatch keeps its ordinary path. */
#  if defined(_MSC_VER)
  __declspec(noinline)
#  elif __has_cpp_attribute(gnu::noinline)
  [[gnu::noinline]]
#  endif
  auto try_divide_with_reuse(Number& lhs, Number const& rhs, bool remainder) -> bool;
#endif

  /** \brief  Get the number stored in the array element ae.  */
  // Keep container-specific work outside scalar dispatch on GCC. Native Linux
  // measurements exposed broad regressions after the storage layout changed.
  [[nodiscard]]
#if defined(__GNUC__) && !defined(__clang__)
  __attribute__((noinline))
#endif
  auto array_element(ArrayElement const& ae) const -> Number;

  /** \brief  Store a number in an array eleemnt.  */
#if defined(__GNUC__) && !defined(__clang__)
  __attribute__((noinline))
#endif
  void array_element(ArrayElement const& ae, Number const& num);

  [[nodiscard]] auto variable(Variable v) const -> Number const&;
  void variable(Variable v, Number const& num);

  [[nodiscard]] auto array(Array a) const -> ArrayValues;
  void array(Array a, ArrayValues av);
  [[nodiscard]] auto array_length(Array a) const -> Number::NumType;

  void function(Letter func, Instructions::const_iterator const& begin,
                Instructions::const_iterator const& end, VariableMask mask, Location const& loc);

  /** \brief       Do a call
   *  \param  func Function to call.
   *  \param  loc  Location of call.
   *  \return      Result of call.
   */
  auto call(Letter func, Location const& loc) -> Number;

  /** Push a new parameter pack onto the parameter stack.  */
  void push_param_pack();

  /** Push a scalar parameter into the current parameter pack.  */
  void push_param(Number const& num);

  /** Push a vector parameter into the current parameter pack.  */
  void push_param(ArrayValues av);

  /** Pop the current parameter pack.  */
  void pop_param_pack();

  /** \brief  Consume a scalar parameter.
   *  \return Value of parameter
   */
  auto pop_param() -> Number;

  /** \brief  Consume an array parameter.
   *  \return Value of parameter
   */
  auto pop_param_array() -> ArrayValues;

  // Each active pack owns one depth slot. deque growth preserves references
  // held by callers; clearing a slot drops values while retaining capacity.
  auto acquire_results(std::size_t count) -> ResultBuffer&;
  void release_results();

  /** \brief  Validate instruction bounds, result types and control-flow availability.  */
  void validate(Instructions const& instrs, bool function_body = false) const;
  auto verify(Instructions const& instructions) const -> PreparedInstructions
  {
    validate(instructions);
    return PreparedInstructions(
      instructions, full_checks() ? ConsumptionPlan{} : plan_consumption(instructions, false),
      full_checks() ? ResultSlotPlan{} : plan_result_slots(instructions, sizeof(ResultSlot)));
  }
  auto full_checks() const noexcept -> bool { return checks_ == VM::ExecutionChecks::full; }

private:
  template<typename... Ts>
  [[noreturn]] void error(char const* func, char const* file, unsigned line, char const* test,
                          Msg msg, Ts... args) const
  {
    error_ << Messages::get().format(Set::bc, Msg::internal_error, func, file, line, test) << '\n'
           << Messages::get().format(Set::bc, msg, args...) << '\n';
    std::exit(1);  // NOLINT(concurrency-mt-unsafe)
  }

  using Param = std::variant<ArrayValues, Number>;  ///< Parameter to a function
  using Params = std::vector<Param, Profile::Allocator<Param, Profile::AllocationKind::frames>>;
  struct ParamMark
  {
    std::size_t begin;
    std::size_t next;
  };
  using ParamMarks =
    std::vector<ParamMark, Profile::Allocator<ParamMark, Profile::AllocationKind::frames>>;
  using Index = Instruction::Index;    ///< Index into the instructions
  using Offset = Instruction::Offset;  ///< Offset from current instruction
  using NumType = Number::NumType;     ///< Number type
  using FunctionDefinition =
    std::tuple<FunctionCode, VariableMask, Location>;  ///< Function definition.

  static constexpr NumType default_base_ = 10;
  std::ostream& output_;                            ///< Stream for "normal output."
  std::ostream& error_;                             ///< Stream for error output.
  std::array<ArrayValues, Letter::count_> arrays_;  ///< Array of arrays, indexed by Letter.
  std::array<std::optional<FunctionDefinition>, Letter::count_>
    functions_;  ///< Array of (optional) functions, indexed by letter.
  std::array<Number, Letter::count_> variables_;  ///< Array of scalar variables, indexed by Letter.
  Params parameters_;                             ///< Contiguous values for active argument packs.
  ParamMarks param_marks_;                        ///< Pack offsets; indices survive vector growth.
  Params saved_locals_;                           ///< Contiguous saved locals for active calls.
  std::deque<ResultBuffer, Profile::Allocator<ResultBuffer, Profile::AllocationKind::frames>>
    result_buffers_;  ///< Reusable buffers, indexed by simultaneous activation depth.
  std::size_t result_depth_ = 0;
  NumType ibase_ = default_base_;  ///< Input base, range [2, 16]
  NumType obase_ = default_base_;  ///< Output base, range: [2, base_)
  NumType scale_ = 0;              ///< Scale, range: [0, base_)
  bool save_specials_;             ///< Save specials on function entry?
  VM::ExecutionChecks checks_;     ///< Full diagnostic oracle or verified dispatch.
#if GD_BC_PREPARED_DIVISORS
  struct DivisorCacheDelete
  {
    void operator()(Number::DivisorCache* cache) const noexcept
    {
      delete cache;
#  if GD_BC_PROFILE
      Profile::deallocated(Profile::AllocationKind::divisors, sizeof(Number::DivisorCache));
#  endif
    }
  };
  std::unique_ptr<Number::DivisorCache, DivisorCacheDelete> divisor_cache_;
#endif
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
  using Result = ExecutionResult;

  /** \brief        Constructor
   *  \param vm     VM state to modify
   *  \param instrs Instructions to hold.
   */
  InstructionPack(VMState* vm, VerifiedInstructions instrs);
  ~InstructionPack() { vm_->release_results(); }
  InstructionPack(InstructionPack const&) = delete;
  auto operator=(InstructionPack const&) -> InstructionPack& = delete;

  /** \brief  Execute the instruction pack
   *  \return Pair of returned value, and whether we should execute more instruction packs.
   */
  auto execute() -> std::pair<Number, bool>;

private:
  friend auto operator<<(std::ostream& os, InstructionPack const& instrs) -> std::ostream&;

  template<typename... Ts>
  [[noreturn]] void error(char const* func, char const* file, unsigned line, char const* test,
                          Msg msg, Ts... args) const
  {
    vm_->stream(Instruction::Stream::error)
      << Messages::get().format(Set::bc, Msg::internal_error, func, file, line, test) << '\n'
      << Messages::get().format(Set::bc, Msg::instruction_header) << '\n'
      << *this << Messages::get().format(Set::bc, msg, args...) << '\n';
    std::exit(1);  // NOLINT(concurrency-mt-unsafe)
  }

  /** Execute print instruction.  */
  void execute_print();

  /** Execute quit instruction.  */
  void execute_quit();

  /** Execute length instruction.  */
  void execute_length();

  /** Execute load instruction.  */
  void execute_load();

  /** Execute store instruction.  */
  void execute_store();

  /** Execute branch instruction.  Returns index of next instruction to execute.  */
  auto execute_branch() -> Index;

  /** Execute branch_zero instruction.  Returns index of next instruction to execute.  */
  auto execute_branch_zero() -> Index;

  /** Execute function_begin instruction.  Returns index of next instruction to execute.  */
  auto execute_function_begin() -> Index;

  /** Execute the return instruction. Returns value to return.  */
  auto execute_return() -> Number;

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
    result_at(pc_) = f();
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
    result_at(pc_) = f(instrs_[pc_].op1());
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
    result_at(pc_) = f(instrs_[pc_].op1(), instrs_[pc_].op2());
  }

  /** \brief   Execute an instruction with one expression operand, and has a Number result.
   *  \param f Function to call to do the execution
   *
   * \a f should have a signature like `void f(Number& op)`.  It should update op with the result.
   */
  template<typename Fn>
  void execute_unary_op(Fn f)
  {
    Number lhs = take_op1_expr();
    f(lhs);
    result_at(pc_) = std::move(lhs);
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
    Number lhs = take_op1_expr();
    Number const& rhs = get_op2_expr();
    f(lhs, rhs);
    result_at(pc_) = std::move(lhs);
  }

  /** \brief  Conver the offset in the index  */
  [[nodiscard]] auto get_offset_index(Instruction::Operand const& op) const -> Index;

  /** \brief  Get the value pointed to by op relative to the current PC.  */
  [[nodiscard]] auto get_op_expr(Instruction::Operand const& op) const -> Number const&;

  /** Copy the first expression, or transfer it after a proven last use. */
  auto take_op1_expr() -> Number;

  /** \brief Get the value pointed to by op1 of the current instruction.  */
  [[nodiscard]] auto get_op1_expr() const -> Number const&;

  /** \brief Get the value pointed to by op2 of the current instruction.  */
  [[nodiscard]] auto get_op2_expr() const -> Number const&;

  /** Validate the result in the current pc. */
  void validate_result(Instructions::size_type i) const;
  template<bool FullChecks>
  auto execute_impl() -> std::pair<Number, bool>;

  // The verified producer map preserves instruction-relative operands. Full
  // checks and unprofitable maps use the original instruction-indexed buffer.
  auto result_at(Index instruction) -> ResultSlot&
  {
    return results_[result_indices_ ? result_indices_[instruction] : instruction];
  }
  auto result_at(Index instruction) const -> ResultSlot const&
  {
    return results_[result_indices_ ? result_indices_[instruction] : instruction];
  }

  VMState* vm_;                         ///< Virtual Machine state
  Instructions const& instrs_;          ///< Instructions to execute
  Instruction::Index pc_{0};            ///< Current program counter
  ResultBuffer& results_;               ///< Borrowed stable buffer for this activation.
  ConsumptionPlan const& consumption_;  ///< Immutable metadata owned beside the code.
  // Immutable plan data is owned beside the verified code and cannot relocate
  // during this activation. Null preserves the full-width identity fallback.
  ResultSlotPlan::CompactIndex const* result_indices_;
};

auto operator<<(std::ostream& os, InstructionPack const& instrs) -> std::ostream&;
auto operator<<(std::ostream& os, InstructionPack::Result const& result) -> std::ostream&;

/** Always-lock-free state shared with POSIX handlers or the Windows console thread. */
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
std::atomic_flag have_been_interrupted = ATOMIC_FLAG_INIT;

/** Handle receiving SIGINT.  */
__EXTERN_C void handle_sigint(int /*signal*/)
{
  (void)have_been_interrupted.test_and_set(std::memory_order_relaxed);
}

/** Install the VM handler only for execution, preserving all caller signal state.
 * A failed installation leaves the existing handler responsible for interruption.
 */
class InterruptHandler
{
public:
  InterruptHandler()
  {
    have_been_interrupted.clear(std::memory_order_relaxed);
    struct sigaction action{};
    action.sa_handler = handle_sigint;
    sigemptyset(&action.sa_mask);
    action.sa_flags = SA_RESTART | SA_RESETHAND;
    installed_ = sigaction(SIGINT, &action, &previous_) == 0;
  }

  ~InterruptHandler()
  {
    if (installed_) {
      (void)sigaction(SIGINT, &previous_, nullptr);
    }
  }

  InterruptHandler(InterruptHandler const&) = delete;
  auto operator=(InterruptHandler const&) -> InterruptHandler& = delete;

private:
  struct sigaction previous_{};
  bool installed_{false};
};

}  // namespace GD::Bc::Details

// NOLINTNEXTLINE
GD::Bc::Details::VMState::VMState(std::ostream& out, std::ostream& err, bool save_specials,
                                  VM::ExecutionChecks checks)
    : output_(out), error_(err), save_specials_(save_specials), checks_(checks)
{
}

#if GD_BC_PREPARED_DIVISORS
auto GD::Bc::Details::VMState::try_divide_with_reuse(Number& lhs, Number const& rhs, bool remainder)
  -> bool
{
  // Query before any by-value copy can make a unique temporary appear shared.
  // Discard an older snapshot so an unrelated request cannot pin its owner.
  if (!rhs.has_shared_heap_storage()) {
    if (divisor_cache_) {
      divisor_cache_->clear();
    }
    return false;
  }
  if (!divisor_cache_) {
    auto cache = std::make_unique<Number::DivisorCache>();
#  if GD_BC_PROFILE
    Profile::allocated(Profile::AllocationKind::divisors, sizeof(Number::DivisorCache));
#  endif
    divisor_cache_.reset(cache.release());
  }
  return remainder ? lhs.try_modulo_reusing(rhs, scale(), *divisor_cache_)
                   : lhs.try_divide_reusing(rhs, scale(), *divisor_cache_);
}
#endif

auto GD::Bc::Details::VMState::stream(Instruction::Stream stream) const -> std::ostream&
{
  return stream == Instruction::Stream::output ? output_ : error_;
}

void GD::Bc::Details::VMState::push_param(Number const& n)
{
  assert_error(!param_marks_.empty(), Msg::parameter_stack_empty);  // NOLINT
  parameters_.emplace_back(n);
}

void GD::Bc::Details::VMState::push_param(ArrayValues av)
{
  assert_error(!param_marks_.empty(), Msg::parameter_stack_empty);  // NOLINT
  parameters_.emplace_back(std::move(av));
}

void GD::Bc::Details::VMState::push_param_pack()
{
  param_marks_.push_back({parameters_.size(), parameters_.size()});
}

void GD::Bc::Details::VMState::pop_param_pack()
{
  assert_error(!param_marks_.empty(), Msg::parameter_stack_empty);  // NOLINT
  auto const mark = param_marks_.back();
  assert_error(mark.next == parameters_.size(), Msg::parameter_pack_not_empty);  // NOLINT
  parameters_.resize(mark.begin);
  param_marks_.pop_back();
}

auto GD::Bc::Details::VMState::pop_param() -> GD::Bc::Number
{
  assert_error(!param_marks_.empty(), Msg::parameter_stack_empty);  // NOLINT
  auto& mark = param_marks_.back();
  assert_error(mark.next < parameters_.size(), Msg::parameter_pack_empty);  // NOLINT
  auto& value = parameters_[mark.next];
  if (!std::holds_alternative<Number>(value)) {
    Details::error(Msg::parameter_type_mismatch, "number");
  }
  ++mark.next;
  return std::move(std::get<Number>(value));
}

auto GD::Bc::Details::VMState::pop_param_array() -> GD::Bc::ArrayValues
{
  assert_error(!param_marks_.empty(), Msg::parameter_stack_empty);  // NOLINT
  auto& mark = param_marks_.back();
  assert_error(mark.next < parameters_.size(), Msg::parameter_pack_empty);  // NOLINT
  auto& value = parameters_[mark.next];
  if (!std::holds_alternative<ArrayValues>(value)) {
    Details::error(Msg::parameter_type_mismatch, "array");
  }
  ++mark.next;
  return std::move(std::get<ArrayValues>(value));
}

void GD::Bc::Details::VMState::function(Letter func, Instructions::const_iterator const& begin,
                                        Instructions::const_iterator const& end, VariableMask mask,
                                        Location const& loc)
{
  Instructions instrs(begin, end);
  validate(instrs, true);
  auto consumption = full_checks() ? ConsumptionPlan{} : plan_consumption(instrs, true);
  auto result_slots =
    full_checks() ? ResultSlotPlan{} : plan_result_slots(instrs, sizeof(ResultSlot));
  functions_.at(static_cast<unsigned>(func)) = std::make_optional(std::make_tuple(
    FunctionCode(std::move(instrs), std::move(consumption), std::move(result_slots)), mask, loc));
}

auto GD::Bc::Details::VMState::call(Letter func, Location const& loc) -> GD::Bc::Number
{
  GD_BC_PROFILE_EVENT(Profile::Event::function_calls, 1);
  auto const& func_ptr{functions_.at(static_cast<unsigned>(func))};
  if (!func_ptr.has_value()) {
    Details::error(Msg::function_not_defined, func, loc.file_name(), loc.line(), loc.column());
  }

  FunctionDefinition const& def = func_ptr.value();
  auto const& func_instructions = std::get<0>(def);
  auto locals = std::get<1>(def);

  /* Save the locals.  */
  // Retain offsets rather than references across execute(): recursive calls
  // may grow the same buffer. Capacity remains available after restoration.
  auto const saved_begin = saved_locals_.size();
  locals.for_each_variable([this](Letter l) {
    saved_locals_.emplace_back(std::move(variables_.at(static_cast<unsigned>(l))));
    variables_.at(static_cast<unsigned>(l)) = Number(0);
  });
  locals.for_each_array([this](Letter l) {
    saved_locals_.emplace_back(std::move(arrays_.at(static_cast<unsigned>(l))));
    arrays_.at(static_cast<unsigned>(l)).reset();
  });

  /* Save specials.  */
  auto saved_scale = scale_;
  auto saved_ibase = ibase_;
  auto saved_obase = obase_;

  InstructionPack fn(this, func_instructions.view());
  auto [result, cont] = fn.execute();

  /* Restore the locals. */
  auto saved = saved_begin;
  locals.for_each_variable([&saved, this](Letter l) {
    variables_.at(static_cast<unsigned>(l)) = std::move(std::get<Number>(saved_locals_[saved++]));
  });
  locals.for_each_array([&saved, this](Letter l) {
    arrays_.at(static_cast<unsigned>(l)) = std::move(std::get<ArrayValues>(saved_locals_[saved++]));
  });
  saved_locals_.resize(saved_begin);

  /* Restore specials.  */
  if (save_specials_) {
    scale_ = saved_scale;
    ibase_ = saved_ibase;
    obase_ = saved_obase;
  }

  return result;
}

void GD::Bc::Details::VMState::ibase(Number const& num)
{
  NumType const n = num.to_unsigned();
  if (n < 2 || n > 16) {  // NOLINT - 16 is not magic
    Details::error(Msg::ibase_out_of_range, n);
  }
  ibase_ = n;
}

auto GD::Bc::Details::VMState::ibase() const -> GD::Bc::Number::NumType { return ibase_; }

void GD::Bc::Details::VMState::obase(Number const& num)
{
  NumType const n = num.to_unsigned();
  if (n < 2) {
    Details::error(Msg::obase_out_of_range, n, Number::base_);
  }
  obase_ = num.to_unsigned();
}

auto GD::Bc::Details::VMState::obase() const -> GD::Bc::Number::NumType { return obase_; }

void GD::Bc::Details::VMState::scale(Number const& num) { scale_ = num.to_unsigned(); }

auto GD::Bc::Details::VMState::scale() const -> GD::Bc::Number::NumType { return scale_; }

void GD::Bc::Details::VMState::array_element(ArrayElement const& ae, Number const& num)
{
  auto& a = arrays_.at(static_cast<unsigned>(ae.first.get()));
  if (!a) {
    a = GD_BC_PROFILE_SHARED(Profile::AllocationKind::arrays, ArrayValues::element_type)();
  }
  else if (a.use_count() > 1) {
    // Array arguments and saved locals are values; isolate a write from their
    // shared snapshot before changing an element.
    GD_BC_PROFILE_EVENT(Profile::Event::array_clones, 1);
    GD_BC_PROFILE_EVENT(Profile::Event::array_entries_copied, a->size());
    a = GD_BC_PROFILE_SHARED(Profile::AllocationKind::arrays, ArrayValues::element_type)(*a);
  }

  a->insert_or_assign(ae.second, num);
}

auto GD::Bc::Details::VMState::array_element(ArrayElement const& ae) const -> GD::Bc::Number
{
  ArrayValues const& a = arrays_.at(static_cast<unsigned>(ae.first.get()));
  if (!a) {
    return {};
  }

  auto const* value = a->lookup(ae.second);
  return value ? *value : Number();
}

auto GD::Bc::Details::VMState::variable(Variable v) const -> GD::Bc::Number const&
{
  return variables_.at(static_cast<unsigned>(v.get()));
}

void GD::Bc::Details::VMState::variable(Variable v, Number const& num)
{
  variables_.at(static_cast<unsigned>(v.get())) = num;
}

auto GD::Bc::Details::VMState::array(Array a) const -> GD::Bc::ArrayValues
{
  return arrays_.at(static_cast<unsigned>(a.get()));
}

void GD::Bc::Details::VMState::array(Array a, ArrayValues av)
{
  arrays_.at(static_cast<unsigned>(a.get())) = std::move(av);
}

auto GD::Bc::Details::VMState::array_length(Array a) const -> GD::Bc::Number::NumType
{
  ArrayValues const& arr = arrays_.at(static_cast<unsigned>(a.get()));
  // Untouched arrays have no allocated map and contain no elements.
  if (!arr || arr->empty()) {
    return 0;
  }

  return arr->length();
}

void GD::Bc::Details::VMState::validate(Instructions const& instrs, bool function_body) const
{
  for (Instructions::size_type i = 0; i < instrs.size(); ++i) {
    auto check_offset = [&](Instruction::Operand const& operand, bool allow_end, Msg underflow,
                            Msg overflow) {
      auto const* offset = std::get_if<Instruction::Offset>(&operand);
      if (offset == nullptr) {
        return;
      }
      // -(minimum signed offset) is not representable. For a negative offset,
      // comparing its magnitude minus one avoids that overflow entirely.
      assert_error(*offset >= 0 || static_cast<Instruction::Index>(-(*offset + 1)) < i, underflow,
                   i, *offset);  // NOLINT
      auto const remaining = instrs.size() - i;
      // Branches may fall through at end; a data reference needs an actual result.
      assert_error(*offset < 0 || static_cast<Instruction::Index>(*offset) < remaining ||
                     (allow_end && static_cast<Instruction::Index>(*offset) == remaining),
                   overflow, i, *offset);  // NOLINT
    };
    if (instrs[i].has_op1()) {
      check_offset(instrs[i].op1(), instrs[i].opcode() == Instruction::Opcode::branch,
                   Msg::op1_offset_underflow, Msg::op1_offset_overflow);
    }
    if (instrs[i].has_op2()) {
      check_offset(instrs[i].op2(), instrs[i].opcode() == Instruction::Opcode::branch_zero,
                   Msg::op2_offset_underflow, Msg::op2_offset_overflow);
    }
  }

  // Classify result contracts independently of instruction layout. In particular,
  // a forward reference may be valid when control flow executes its producer
  // first. This checks types, not dominance or parameter-stack balance.
  using O = Instruction::Opcode;
  auto target = [](Instruction::Index i, Instruction::Operand const& operand) {
    // The complete range pass above makes signed-min and one-past data offsets
    // impossible here. Unsigned addition also handles valid negative offsets.
    return i + static_cast<Instruction::Index>(std::get<Instruction::Offset>(operand));
  };
  std::vector<ResultKind> kinds(instrs.size(), ResultKind::none);
  for (Instruction::Index i = 0; i < instrs.size(); ++i) {
    kinds[i] = classify_result(instrs, i);
  }
  for (Instruction::Index i = 0; i < instrs.size(); ++i) {
    auto const& instruction = instrs[i];
    auto check = [this, i](bool valid, unsigned operand) {
      assert_error(valid, Msg::incompatible_operand_result, operand, i);  // NOLINT
    };
    auto first = [&]() { return kinds[target(i, instruction.op1())]; };
    auto second = [&]() { return kinds[target(i, instruction.op2())]; };
    switch (instruction.opcode()) {
    case O::add:
    case O::subtract:
    case O::multiply:
    case O::divide:
    case O::modulo:
    case O::power:
    case O::equals:
    case O::less_than_equals:
    case O::not_equals:
    case O::less_than:
      check(second() == ResultKind::number, 2);
      [[fallthrough]];
    case O::negate:
    case O::scale_expr:
    case O::sqrt:
    case O::abs:
    case O::branch_zero:
    case O::return_:
      check(first() == ResultKind::number, 1);
      break;
    case O::array_element:
      check(second() == ResultKind::number, 2);
      break;
    case O::load:
      check(first() == ResultKind::scalar_location || first() == ResultKind::array_location, 1);
      break;
    case O::store:
      check(first() == ResultKind::scalar_location || first() == ResultKind::array_location, 1);
      check(second() == (first() == ResultKind::array_location ? ResultKind::array_values
                                                               : ResultKind::number),
            2);
      break;
    case O::length:
      check(first() == ResultKind::number || first() == ResultKind::array_location, 1);
      break;
    case O::push_param:
      check(first() == ResultKind::number || first() == ResultKind::array_values, 1);
      break;
    case O::print:
      // Printing all result alternatives is a supported diagnostic operation.
      check(first() != ResultKind::none, 1);
      break;
    case O::eof:
    case O::quit:
    case O::string:
    case O::number:
    case O::variable:
    case O::array:
    case O::scale:
    case O::ibase:
    case O::obase:
    case O::branch:
    case O::call:
    case O::push_param_mark:
    case O::pop_param_mark:
    case O::pop_param:
    case O::pop_param_array:
    case O::function_begin:
    case O::function_end:
      break;
    }
  }
  auto const failure = verify_control_flow(instrs, function_body);
  if (failure) {
    assert_error(false, Msg::invalid_control_flow, failure->instruction,
                 failure->reason);  // NOLINT
  }
}

auto GD::Bc::Details::VMState::acquire_results(std::size_t count) -> ResultBuffer&
{
  if (result_depth_ == result_buffers_.size()) {
    result_buffers_.emplace_back();
  }
  auto& result = result_buffers_[result_depth_];
  assert(result.empty());  // NOLINT
  result.resize(count);
  ++result_depth_;
  return result;
}

void GD::Bc::Details::VMState::release_results()
{
  assert(result_depth_ != 0);  // NOLINT
  result_buffers_[--result_depth_].clear();
}

GD::Bc::Details::InstructionPack::InstructionPack(VMState* vm, VerifiedInstructions instrs)
    : vm_(vm), instrs_(instrs.instructions()),
      results_(vm->acquire_results(instrs.result_slots().storage_size(instrs_.size()))),
      consumption_(instrs.consumption()),
      result_indices_(instrs.result_slots().indices.empty() ? nullptr
                                                         : instrs.result_slots().indices.data())
{
  assert(vm_ != nullptr);  // NOLINT
}

auto GD::Bc::Details::InstructionPack::execute() -> std::pair<GD::Bc::Number, bool>
{
  return vm_->full_checks() ? execute_impl<true>() : execute_impl<false>();
}

template<bool FullChecks>
auto GD::Bc::Details::InstructionPack::execute_impl() -> std::pair<GD::Bc::Number, bool>
{
  for (; pc_ < instrs_.size(); ++pc_) {
    if (Details::have_been_interrupted.test(std::memory_order_relaxed)) {
      return std::make_pair(Number(0), true);
    }
    GD_BC_PROFILE_EVENT(Profile::Event::instructions_executed, 1);
    [[maybe_unused]] auto const i = pc_;
    switch (instrs_[pc_].opcode()) {
    case Instruction::Opcode::string:
      execute_unary([](Instruction::Operand const& o) {
        return static_cast<std::string_view>(std::get<std::string>(o));
      });
      break;
    case Instruction::Opcode::number:
      execute_unary(
        [this](Instruction::Operand const&) { return instrs_[pc_].decoded_number(vm_->ibase()); });
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
#if GD_BC_PREPARED_DIVISORS
      execute_binary_op([this](Number& lhs, Number const& rhs) {
        if (!rhs.has_heap_storage() || !vm_->try_divide_with_reuse(lhs, rhs, false)) {
          lhs.divide(rhs, vm_->scale());
        }
      });
#else
      execute_binary_op([this](Number& lhs, Number const& rhs) { lhs.divide(rhs, vm_->scale()); });
#endif
      break;
    case Instruction::Opcode::modulo:
#if GD_BC_PREPARED_DIVISORS
      execute_binary_op([this](Number& lhs, Number const& rhs) {
        if (!rhs.has_heap_storage() || !vm_->try_divide_with_reuse(lhs, rhs, true)) {
          lhs.modulo(rhs, vm_->scale());
        }
      });
#else
      execute_binary_op([this](Number& lhs, Number const& rhs) { lhs.modulo(rhs, vm_->scale()); });
#endif
      break;
    case Instruction::Opcode::sqrt:
      execute_unary_op([this](Number& lhs) { lhs.sqrt(vm_->scale()); });
      break;
    case Instruction::Opcode::abs:
      execute_unary_op([](Number& lhs) { lhs.abs(); });
      break;
    case Instruction::Opcode::scale_expr:
      execute_unary_op([](Number& lhs) { lhs = Number(lhs.scale()); });
      break;
    case Instruction::Opcode::length:
      execute_length();
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
    case Instruction::Opcode::return_: {
      // Returning ends this activation. No producer slot is read afterwards,
      // so transfer the owned result without an extra shared-owner copy.
      auto const index = get_offset_index(instrs_[pc_].op1());
      auto& value = result_at(index);
      assert_error(value.has_value(), Msg::empty_result, index);  // NOLINT
      return std::make_pair(std::move(std::get<Number>(*value)), true);
    }
    case Instruction::Opcode::eof:
      assert(pc_ == instrs_.size() - 1);  // NOLINT
      return std::make_pair(Number(0), false);
      break;
    case Instruction::Opcode::function_end: /* Should never been seen here.  */
      abort();
      break;
    }

    // Entry verification proves the result kind and availability at each
    // consumer. Retain the original second switch in the diagnostic oracle.
    if constexpr (FullChecks) {
      validate_result(i);
    }
  }

  /* Fall off the end assume 'return (0);' */
  return std::make_pair(Number(0), true);
}

void GD::Bc::Details::InstructionPack::execute_print()
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::print);  // NOLINT
  Index const result_idx = get_offset_index(instrs_[pc_].op1());
  std::ostream& os = vm_->stream(std::get<Instruction::Stream>(instrs_[pc_].op2()));
  auto& result = result_at(result_idx);
  assert_error(result.has_value(), Msg::empty_result, result_idx);  // NOLINT
  std::visit(Overloaded{
               // Empty slots are rejected before visiting in both execution modes.
               [](std::monostate) { std::abort(); },
               [&os](std::string_view sv) { os << sv; },
               [&os, this](Number const& n) {
                 n.output(os, vm_->obase(), 0);
                 os << '\n';
               },
               [&os](Variable v) { os << v << '\n'; },
               [&os](Array a) { os << a << '\n'; },
               [&os](ArrayElement const& ae) { os << ae.first << '[' << ae.second << "]\n"; },
               [&os](ArrayValues const& /*unused*/) { os << "<ARRAY VALUES>\n"; },
               [&os](Ibase /*unused*/) { os << "ibase\n"; },
               [&os](Obase /*unused*/) { os << "obase\n"; },
               [&os](Scale /*unused*/) { os << "scale\n"; },
             },
             *result);
}

void GD::Bc::Details::InstructionPack::execute_quit()
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::quit);  // NOLINT
  std::exit(                                                   // NOLINT(concurrency-mt-unsafe)
    static_cast<int>(std::get<unsigned>(instrs_.at(pc_).op1())));
}

void GD::Bc::Details::InstructionPack::execute_length()
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::length);  // NOLINT
  Index const loc_idx = get_offset_index(instrs_[pc_].op1());
  auto& result = result_at(pc_);
  auto const& loc = result_at(loc_idx);
  assert_error(loc.has_value(), Msg::empty_result, loc_idx);  // NOLINT

  std::visit(Overloaded{
               // Empty slots are rejected before visiting in both execution modes.
               [](std::monostate) { std::abort(); },
               [this](std::string_view /*s*/) {
                 assert_error(false, Msg::cannot_get_length, "string_view");  // NOLINT
               },
               [&result](Number const& n) { result = Number(n.length()); },
               [this](ArrayValues const& /*av*/) {
                 assert_error(false, Msg::cannot_get_length, "ArrayValues");  // NOLINT
               },
               [this](Variable /*v*/) {
                 // NOLINTNEXTLINE
                 assert_error(false, Msg::cannot_get_length, "Variable");
               },
               [&result, this](Array a) { result = Number(vm_->array_length(a)); },
               [this](ArrayElement const& /*ae*/) {
                 // NOLINTNEXTLINE
                 assert_error(false, Msg::cannot_get_length, "ArrayElement");
               },
               [this](Ibase) { assert_error(false, Msg::cannot_get_length, "ibase"); },  // NOLINT
               [this](Obase) { assert_error(false, Msg::cannot_get_length, "obase"); },  // NOLINT
               [this](Scale) { assert_error(false, Msg::cannot_get_length, "scale"); },  // NOLINT
             },
             *loc);
}

void GD::Bc::Details::InstructionPack::execute_load()
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::load);  // NOLINT
  Index const loc_idx = get_offset_index(instrs_.at(pc_).op1());
  auto& result = result_at(pc_);
  auto const& loc = result_at(loc_idx);
  assert_error(loc.has_value(), Msg::empty_result, loc_idx);  // NOLINT

  std::visit(Overloaded{
               // Empty slots are rejected before visiting in both execution modes.
               [](std::monostate) { std::abort(); },
               [this](std::string_view /*sv*/) {
                 assert_error(false, Msg::cannot_load, "string_view");  // NOLINT
               },
               [this](Number) { assert_error(false, Msg::cannot_load, "Number"); },  // NOLINT
               [this](ArrayValues const& /*av*/) {
                 assert_error(false, Msg::cannot_load, "ArrayValues");  // NOLINT
               },
               [&result, this](Variable v) { result = vm_->variable(v); },
               [&result, this](Array a) { result = vm_->array(a); },
               [&result, this](ArrayElement const& ae) { result = vm_->array_element(ae); },
               [&result, this](Ibase /*ib*/) { result = Number(vm_->ibase()); },
               [&result, this](Obase /*ob*/) { result = Number(vm_->obase()); },
               [&result, this](Scale /*s*/) { result = Number(vm_->scale()); },
             },
             *loc);
}

void GD::Bc::Details::InstructionPack::execute_store()
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::store);  // NOLINT
  Index const loc_idx = get_offset_index(instrs_[pc_].op1());
  auto& loc = result_at(loc_idx);
  assert_error(loc.has_value(), Msg::empty_result, loc_idx);  // NOLINT
  Index const expr_idx = get_offset_index(instrs_[pc_].op2());
  auto& result = result_at(expr_idx);
  assert_error(result.has_value(), Msg::empty_result, expr_idx);  // NOLINT
  auto& expr = *result;

  std::visit(
    Overloaded{
      // Empty slots are rejected before visiting in both execution modes.
      [](std::monostate) { std::abort(); },
      [this](std::string_view /*sv*/) {
        assert_error(false, Msg::cannot_store, "string_view");  // NOLINT
      },
      [this](Number) { assert_error(false, Msg::cannot_store, "Number"); },  // NOLINT
      [this](ArrayValues const& /*av*/) {
        assert_error(false, Msg::cannot_store, "ArrayValues");  // NOLINT
      },
      [&expr, this](Variable v) { vm_->variable(v, std::get<Number>(expr)); },
      [&expr, this](Array a) { vm_->array(a, std::get<ArrayValues>(expr)); },
      [&expr, this](ArrayElement const& ae) { vm_->array_element(ae, std::get<Number>(expr)); },
      [&expr, this](Ibase /*unused*/) { vm_->ibase(std::get<Number>(expr)); },
      [&expr, this](Obase /*unused*/) { vm_->obase(std::get<Number>(expr)); },
      [&expr, this](Scale /*unused*/) { vm_->scale(std::get<Number>(expr)); },
    },
    *loc);
}

auto GD::Bc::Details::InstructionPack::execute_branch() -> GD::Bc::Instruction::Index
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::branch);  // NOLINT
  return get_offset_index(instrs_[pc_].op1());
}

auto GD::Bc::Details::InstructionPack::execute_branch_zero() -> GD::Bc::Instruction::Index
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::branch_zero);  // NOLINT
  Number const c = get_op1_expr();
  Index const dest_idx = get_offset_index(instrs_[pc_].op2());
  return c.is_zero() ? dest_idx : pc_ + 1;
}

auto GD::Bc::Details::InstructionPack::execute_function_begin() -> GD::Bc::Instruction::Index
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::function_begin);  // NOLINT
  VariableMask const mask = std::get<VariableMask>(instrs_[pc_].op1());
  auto const& loc = std::get<Location>(instrs_[pc_].op2());
  Index const start = pc_++;
  while (pc_ != instrs_.size() && instrs_[pc_].opcode() != Instruction::Opcode::function_end) {
    ++pc_;
  }
  // NOLINTNEXTLINE
  assert_error(pc_ != instrs_.size(), Msg::no_end_to_function_definition, loc.file_name(),
               loc.line(), loc.column());

  Letter const func = std::get<Letter>(instrs_[pc_].op1());
  Index const given_start = get_offset_index(instrs_[pc_].op2());
  assert_error(start == given_start, Msg::bad_function_definition);  // NOLINT

  vm_->function(func, instrs_.begin() + static_cast<Instruction::Offset>(start) + 1,
                instrs_.begin() + static_cast<Instruction::Offset>(pc_), mask, loc);
  return pc_ + 1;
}

void GD::Bc::Details::InstructionPack::execute_push_param_mark()
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::push_param_mark);  // NOLINT
  vm_->push_param_pack();
}

void GD::Bc::Details::InstructionPack::execute_push_param()
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::push_param);  // NOLINT
  Index const expr_idx = get_offset_index(instrs_[pc_].op1());
  auto& expr = result_at(expr_idx);
  assert_error(expr.has_value(), Msg::empty_result, expr_idx);  // NOLINT

  std::visit(Overloaded{
               [this](Number const& n) { vm_->push_param(n); },
               [this](ArrayValues const& av) { vm_->push_param(av); },
               []([[maybe_unused]] auto a) { abort(); },
             },
             *expr);
}

void GD::Bc::Details::InstructionPack::execute_pop_param_mark()
{
  assert(instrs_[pc_].opcode() == Instruction::Opcode::pop_param_mark);  // NOLINT
  vm_->pop_param_pack();
}

auto GD::Bc::Details::InstructionPack::get_offset_index(Instruction::Operand const& op) const
  -> GD::Bc::Instruction::Index
{
  auto const offset = std::get<Instruction::Offset>(op);
  /* We know the following is safe as we checked in validate().  */
  Index const expr_idx = pc_ + offset;
  return expr_idx;
}

auto GD::Bc::Details::InstructionPack::get_op_expr(Instruction::Operand const& op) const
  -> GD::Bc::Number const&
{
  auto const& result = result_at(get_offset_index(op));
  assert_error(result.has_value(), Msg::empty_result, get_offset_index(op));  // NOLINT
  return std::get<Number>(*result);
}

auto GD::Bc::Details::InstructionPack::take_op1_expr() -> GD::Bc::Number
{
  auto const index = get_offset_index(instrs_[pc_].op1());
  auto& result = result_at(index);
  assert_error(result.has_value(), Msg::empty_result, index);  // NOLINT
  auto& number = std::get<Number>(*result);
  if (number.has_heap_storage() && !consumption_.empty() && consumption_[pc_] != 0) {
    // One static use within a straight-line block proves this slot cannot be
    // read again before its producer executes. Other COW owners stay intact.
    // Inline copies retain no heap owner, so they keep the ordinary copy path.
    GD_BC_PROFILE_EVENT(Profile::Event::temporary_consumptions, 1);
    return std::move(number);
  }
  return number;
}

auto GD::Bc::Details::InstructionPack::get_op1_expr() const -> GD::Bc::Number const&
{
  return get_op_expr(instrs_[pc_].op1());
}

auto GD::Bc::Details::InstructionPack::get_op2_expr() const -> GD::Bc::Number const&
{
  return get_op_expr(instrs_[pc_].op2());
}

auto GD::Bc::Details::operator<<(std::ostream& os,
                                 GD::Bc::Details::InstructionPack::Result const& result)
  -> std::ostream&
{
  std::visit(Overloaded{
               [&os](std::monostate) { os << "Empty"; },
               [&os](std::string_view s) { os << "String(" << s << ')'; },
               [&os](Number const& n) { n.debug(os); },
               [&os](Variable v) { os << v; },
               [&os](Array a) { os << a << "[]"; },
               [&os](ArrayElement const& ae) { os << ae.first << '[' << ae.second << ']'; },
               [&os](ArrayValues const& /*unused*/) { os << "<ARRAY VALUES>"; },
               [&os](Ibase /*unused*/) { os << "ibase"; },
               [&os](Obase /*unused*/) { os << "obase"; },
               [&os](Scale /*unused*/) { os << "scale"; },
             },
             result);
  return os;
}

auto GD::Bc::Details::operator<<(std::ostream& os, GD::Bc::Details::InstructionPack const& instrs)
  -> std::ostream&
{
  for (::size_t i = 0; i < instrs.instrs_.size(); ++i) {
    os << i;
    if (i == instrs.pc_) {
      os << "**";
    }
    os << '\t' << instrs.instrs_[i];
    auto const& instr{instrs.result_at(i)};
    if (instr.has_value()) {
      os << " = " << *instr;
    }
    os << '\n';
  }
  return os;
}

void GD::Bc::Details::InstructionPack::validate_result(Index i) const
{
  GD_BC_PROFILE_EVENT(Profile::Event::result_validation_calls, 1);
  [[maybe_unused]] auto const& result = result_at(i);
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
    assert_error(!result.has_value(), Msg::non_empty_result, i);  // NOLINT
    break;
  case GD::Bc::Instruction::Opcode::scale:
    assert_error(result.has_value(), Msg::empty_result, i);  // NOLINT
    // NOLINTNEXTLINE
    assert_error(std::holds_alternative<Scale>(*result), Msg::wrong_result_type, i, "Scale");
    break;
  case GD::Bc::Instruction::Opcode::ibase:
    assert_error(result.has_value(), Msg::empty_result, i);  // NOLINT
    // NOLINTNEXTLINE
    assert_error(std::holds_alternative<Ibase>(*result), Msg::wrong_result_type, i, "Ibase");
    break;
  case GD::Bc::Instruction::Opcode::obase:
    assert_error(result.has_value(), Msg::empty_result, i);  // NOLINT
    // NOLINTNEXTLINE
    assert_error(std::holds_alternative<Obase>(*result), Msg::wrong_result_type, i, "Obase");
    break;
  case GD::Bc::Instruction::Opcode::string:
    assert_error(result.has_value(), Msg::empty_result, i);  // NOLINT
    // NOLINTNEXTLINE
    assert_error(std::holds_alternative<std::string_view>(*result), Msg::wrong_result_type, i,
                 "string_view");

    break;
  case GD::Bc::Instruction::Opcode::number:
  case GD::Bc::Instruction::Opcode::negate:
  case GD::Bc::Instruction::Opcode::scale_expr:
  case GD::Bc::Instruction::Opcode::sqrt:
  case GD::Bc::Instruction::Opcode::abs:
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
    assert_error(result.has_value(), Msg::empty_result, i);  // NOLINT
    // NOLINTNEXTLINE
    assert_error(std::holds_alternative<Number>(*result), Msg::wrong_result_type, i, "Number");
    break;
  case GD::Bc::Instruction::Opcode::variable:
    assert_error(result.has_value(), Msg::empty_result, i);  // NOLINT
    // NOLINTNEXTLINE
    assert_error(std::holds_alternative<Variable>(*result), Msg::wrong_result_type, i, "Variable");
    break;
  case GD::Bc::Instruction::Opcode::load:
    assert_error(result.has_value(), Msg::empty_result, i);  // NOLINT
    // Scalar locations load Number; whole-array locations load ArrayValues.
    assert_error(std::holds_alternative<Number>(*result) ||
                   std::holds_alternative<ArrayValues>(*result),
                 Msg::wrong_result_type, i, "Number or ArrayValues");  // NOLINT
    break;
  case GD::Bc::Instruction::Opcode::pop_param_array:
    assert_error(result.has_value(), Msg::empty_result, i);  // NOLINT
    assert_error(std::holds_alternative<ArrayValues>(*result), Msg::wrong_result_type, i,
                 "ArrayValues");  // NOLINT
    break;
  case GD::Bc::Instruction::Opcode::array:
    assert_error(result.has_value(), Msg::empty_result, i);  // NOLINT
    // NOLINTNEXTLINE
    assert_error(std::holds_alternative<Array>(*result), Msg::wrong_result_type, i, "Array");
    break;
  case GD::Bc::Instruction::Opcode::array_element:
    assert_error(result.has_value(), Msg::empty_result, i);  // NOLINT
    // NOLINTNEXTLINE
    assert_error(std::holds_alternative<ArrayElement>(*result), Msg::wrong_result_type, i,
                 "ArrayElement");
    break;
  }
}

GD::Bc::VM::VM(std::ostream& out, std::ostream& err, bool save_specials, ExecutionChecks checks)
    : state_(std::make_unique<Details::VMState>(out, err, save_specials, checks))
{
}

// VMState is complete here: destruction and move assignment release all
// variables, array snapshots, saved frames and registered function bodies.
GD::Bc::VM::~VM() = default;
GD::Bc::VM::VM(VM&&) noexcept = default;
auto GD::Bc::VM::operator=(VM&&) noexcept -> VM& = default;

auto GD::Bc::VM::execute(Instructions& instructions) -> bool
{
  auto const verified = state_->verify(instructions);
  Details::InstructionPack instrs(state_.get(), verified.view());
  // Restore the caller's handler before reporting interruption, including when
  // execution throws (for example, through an output stream's exception mask).
  auto const result = [&instrs]() {
    Details::InterruptHandler handler;
    return instrs.execute();
  }();
  if (Details::have_been_interrupted.test(std::memory_order_relaxed)) {
    state_->stream(Instruction::Stream::error) << Messages::get().format(Msg::interrupted) << '\n';
    Details::have_been_interrupted.clear(std::memory_order_relaxed);
  }
  return result.second;
}
