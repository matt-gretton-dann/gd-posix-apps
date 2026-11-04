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

#ifdef GD_BC_NATIVE_SCALAR
#  include "native-scalar.hh"
#endif

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

struct GenericExecutionLayout
{
  ResultSlotPlan slots;
  ExecutableCode code;
};

#if GD_BC_TYPED_RESULTS && GD_BC_COMPACT_EXECUTION
using OtherExecutionResult = std::variant<std::monostate, ArrayValues, std::string_view, Variable,
                                          Array, ArrayElement, Ibase, Obase, Scale>;
using NumberBuffer =
  std::vector<Number, Profile::Allocator<Number, Profile::AllocationKind::results>>;
using OtherBuffer =
  std::vector<OtherExecutionResult,
              Profile::Allocator<OtherExecutionResult, Profile::AllocationKind::results>>;

struct TypedExecutionLayout
{
  TypedResultPlan slots;
  TypedExecutableCode code;
};

using ExecutionLayout = std::variant<GenericExecutionLayout, TypedExecutionLayout>;

struct TypedResultBuffers
{
  NumberBuffer numbers;
  OtherBuffer others;
  void clear()
  {
    numbers.clear();
    others.clear();
  }
};

// Retain capacities only for the current format. A format change releases the
// old buffers before allocating its replacement. Deque growth and deeper
// activations cannot move or resize either lane of an active depth slot.
using ActivationResults = std::variant<ResultBuffer, TypedResultBuffers>;

struct TypedExecutionView
{
  TypedExecutionLayout const* layout;
  TypedResultBuffers* buffers;
};

auto prepare_typed_layout(Instructions const& code, ResultSlotPlan const& generic, bool persistent,
                          bool enabled) -> TypedExecutionLayout
{
  if (enabled) {
    // Charge actual payload and owner/frame/view objects against the existing
    // generic representation. Runtime capacities are measured separately.
    TypedResultCosts const costs{sizeof(Number),
                                 sizeof(OtherExecutionResult),
                                 sizeof(ResultSlot),
                                 sizeof(GenericExecutionLayout) + sizeof(ResultBuffer),
                                 sizeof(ExecutionLayout) + sizeof(ActivationResults) +
                                   sizeof(TypedExecutionView),
                                 sizeof(ExecutableInstruction),
                                 sizeof(TypedExecutableInstruction)};
    auto slots = plan_typed_results(code, generic, costs, persistent);
    if (!slots.empty()) {
      auto executable = lower_typed_executable(code, slots, persistent);
      if (!executable.empty())
        return {std::move(slots), std::move(executable)};
    }
  }
  return {};
}

#else
// Compile out typed ownership and activation state when the feature is off.
using ExecutionLayout = GenericExecutionLayout;
using ActivationResults = ResultBuffer;
#endif

// Lower only verified execution. The build switch and empty lowering preserve
// the portable wide-operand interpreter, including its full index domain.
auto prepare_executable(Instructions const& code, ResultSlotPlan const& slots, bool persistent)
  -> ExecutableCode
{
#if GD_BC_COMPACT_EXECUTION
  return lower_executable(code, slots, persistent);
#else
  (void)code;
  (void)slots;
  (void)persistent;
  return {};
#endif
}

auto prepare_layout(Instructions const& code, ResultSlotPlan generic, bool persistent, bool compact)
  -> ExecutionLayout
{
#if GD_BC_TYPED_RESULTS && GD_BC_COMPACT_EXECUTION
  auto typed = prepare_typed_layout(code, generic, persistent, compact);
  if (!typed.code.empty())
    return ExecutionLayout{std::move(typed)};
#endif
  auto executable = compact ? prepare_executable(code, generic, persistent) : ExecutableCode{};
  return GenericExecutionLayout{std::move(generic), std::move(executable)};
}

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
  auto layout() const -> ExecutionLayout const& { return *layout_; }
  auto generic_layout() const -> GenericExecutionLayout const*
  {
#if GD_BC_TYPED_RESULTS && GD_BC_COMPACT_EXECUTION
    return std::get_if<GenericExecutionLayout>(layout_);
#else
    return layout_;
#endif
  }
#if GD_BC_TYPED_RESULTS && GD_BC_COMPACT_EXECUTION
  auto typed_layout() const -> TypedExecutionLayout const*
  {
    return std::get_if<TypedExecutionLayout>(layout_);
  }
#endif

private:
  friend struct VMState;
  friend class FunctionCode;
  friend class PreparedInstructions;
  VerifiedInstructions(Instructions const& instructions, ConsumptionPlan const& consumption,
                       ExecutionLayout const& layout)
      : instructions_(&instructions), consumption_(&consumption), layout_(&layout)
  {
  }
  Instructions const* instructions_;
  ConsumptionPlan const* consumption_;
  ExecutionLayout const* layout_;
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
    return VerifiedInstructions(*instructions_, consumption_, layout_);
  }

private:
  friend struct VMState;
  PreparedInstructions(Instructions const& instructions, ConsumptionPlan consumption,
                       ResultSlotPlan result_slots, bool compact)
      : instructions_(&instructions), consumption_(std::move(consumption)),
        layout_(prepare_layout(instructions, std::move(result_slots), false, compact))
  {
  }
  Instructions const* instructions_;
  ConsumptionPlan consumption_;
  ExecutionLayout layout_;
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
  auto view(bool full_checks, [[maybe_unused]] VariableMask locals) -> VerifiedInstructions
  {
    if (!full_checks && !prepared_) {
      // Publish both plans together so a failed allocation leaves the
      // verified definition ready for another preparation attempt.
      auto consumption = plan_consumption(instructions_, true);
      auto slots = plan_result_slots(instructions_, sizeof(ResultSlot), consumption);
      auto layout = prepare_layout(instructions_, std::move(slots), true, true);
#if !GD_BC_PROFILE
      auto parameter = prove_scalar_parameter();
      if (parameter && !locals.contains(*parameter)) {
        parameter.reset();
      }
      prepared_.emplace(std::move(consumption), std::move(layout), parameter);
#else
      prepared_.emplace(std::move(consumption), std::move(layout));
#endif
    }
    auto const& plans = prepared_ ? *prepared_ : empty_plans_;
    return VerifiedInstructions(instructions_, plans.consumption, plans.layout);
  }

private:
  friend struct VMState;
  explicit FunctionCode(Instructions instructions) : instructions_(std::move(instructions))
  {
  }

#if !GD_BC_PROFILE
  // Prove the parser's one-scalar prologue and that the rest of the
  // function cannot read its result slots or branch back into it.
  auto prove_scalar_parameter() const -> std::optional<Variable>
  {
    using O = Instruction::Opcode;
    using D = Instruction::Offset;
    constexpr auto body = scalar_prologue_size_;
    if (instructions_.size() <= body || instructions_[0].opcode() != O::pop_param ||
        instructions_[1].opcode() != O::variable || instructions_[2].opcode() != O::store ||
        instructions_[3].opcode() != O::pop_param_mark ||
        std::get<D>(instructions_[2].op1()) != -1 ||
        std::get<D>(instructions_[2].op2()) != -2) {
      return std::nullopt;
    }
    for (Instruction::Index i = body; i < instructions_.size(); ++i) {
      auto uses_prologue = [i](Instruction::Operand const& operand) {
        auto const* offset = std::get_if<D>(&operand);
        return offset != nullptr && i + static_cast<Instruction::Index>(*offset) < body;
      };
      auto const& instruction = instructions_[i];
      if ((instruction.has_op1() && uses_prologue(instruction.op1())) ||
          (instruction.has_op2() && uses_prologue(instruction.op2()))) {
        return std::nullopt;
      }
    }
    return std::get<Variable>(instructions_[1].op1());
  }
  static constexpr Instruction::Index scalar_prologue_size_ = 4;
#endif

  struct Plans
  {
    ConsumptionPlan consumption;
    ExecutionLayout layout;
#if !GD_BC_PROFILE
    std::optional<Variable> scalar_parameter;
#endif
  };
  inline static Plans const empty_plans_{};
  Instructions instructions_;
  std::optional<Plans> prepared_;
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

#if defined(GD_BC_PREPARSED_LIBRARY)
  void defer_library_function(Letter name, std::string_view image,
                              Instructions (*decode)(std::string_view));
#endif

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

  // A terminated top-level pack can leave an argument list unfinished.
  void discard_parameters() noexcept
  {
    parameters_.clear();
    param_marks_.clear();
  }

  /** \brief  Consume a scalar parameter.
   *  \return Value of parameter
   */
  auto pop_param() -> Number;

  /** \brief  Consume an array parameter.
   *  \return Value of parameter
   */
  auto pop_param_array() -> ArrayValues;

  // Each active pack owns one depth slot. deque growth preserves references
  // held by callers; clearing drops values and retains the current format's
  // capacities. Changing format discards the previous format's capacities.
  auto acquire_results(std::size_t count, ExecutionLayout const& layout) -> ResultBuffer*;
#if GD_BC_TYPED_RESULTS && GD_BC_COMPACT_EXECUTION
  auto active_results() -> ActivationResults& { return result_buffers_[result_depth_ - 1]; }
#endif
  void release_results();

  /** \brief  Validate instruction bounds, result types and control-flow availability.  */
  void validate(Instructions const& instrs, bool function_body = false) const;
  auto verify(Instructions const& instructions) const -> PreparedInstructions
  {
    validate(instructions);
    return PreparedInstructions(
      instructions, full_checks() ? ConsumptionPlan{} : plan_consumption(instructions, false),
      full_checks() ? ResultSlotPlan{} : plan_result_slots(instructions, sizeof(ResultSlot)),
      !full_checks());
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
#if defined(GD_BC_PREPARSED_LIBRARY)
  struct DeferredDefinition
  {
    std::string_view image;
    Instructions (*decode)(std::string_view) = nullptr;
  };
  std::array<DeferredDefinition, Letter::count_> deferred_functions_{};
#endif
  std::array<Number, Letter::count_> variables_;  ///< Array of scalar variables, indexed by Letter.
  Params parameters_;                             ///< Contiguous values for active argument packs.
  ParamMarks param_marks_;                        ///< Pack offsets; indices survive vector growth.
  Params saved_locals_;                           ///< Contiguous saved locals for active calls.
  std::deque<ActivationResults,
             Profile::Allocator<ActivationResults, Profile::AllocationKind::frames>>
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
  friend struct VMState;
  // Only the function entry proof can authorize a nonzero starting index.
  auto execute_from(Index start) -> std::pair<Number, bool>
  {
    pc_ = start;
    return execute();
  }


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
  auto execute_compact() -> std::pair<Number, bool>;
#if GD_BC_TYPED_RESULTS && GD_BC_COMPACT_EXECUTION
  auto execute_typed() -> std::pair<Number, bool>;
#endif

  // Shared value semantics: wide execution decodes references before calling
  // these; compact execution supplies its already-lowered register operands.
  void print_value(ResultSlot const& result, Instruction::Stream stream, Index result_idx);
  void length_value(ResultSlot& result, ResultSlot const& loc, Index loc_idx);
  void load_value(ResultSlot& result, ResultSlot const& loc, Index loc_idx);
  void store_value(ResultSlot const& loc, ResultSlot const& result, Index loc_idx, Index expr_idx);
  void push_value(ResultSlot const& expr, Index expr_idx);

  // The verified producer map preserves instruction-relative operands. Full
  // checks and unprofitable maps use the original instruction-indexed buffer.
  auto result_at(Index instruction) -> ResultSlot&
  {
    return (*results_)[result_indices_ ? result_indices_[instruction] : instruction];
  }
  auto result_at(Index instruction) const -> ResultSlot const&
  {
    return (*results_)[result_indices_ ? result_indices_[instruction] : instruction];
  }

  VMState* vm_;                         ///< Virtual Machine state
  Instructions const& instrs_;          ///< Instructions to execute
  Instruction::Index pc_{0};            ///< Current program counter
  ExecutableCode const* executable_;    ///< Generic lowered code, null during typed execution.
  ResultBuffer* results_;               ///< Generic depth buffer, null during typed execution.
  ConsumptionPlan const& consumption_;  ///< Immutable metadata owned beside the code.
  // Immutable plan data is owned beside the verified code and cannot relocate
  // during this activation. Null preserves the full-width identity fallback.
  ResultSlotPlan::CompactIndex const* result_indices_;
#if GD_BC_TYPED_RESULTS && GD_BC_COMPACT_EXECUTION
  TypedExecutionView typed_;
#endif
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

void set_cli_interrupt_active(bool active) noexcept;

/** Install the VM handler only for execution, preserving all caller signal state.
 * A failed installation leaves the existing handler responsible for interruption.
 */
class InterruptHandler
{
public:
  explicit InterruptHandler(bool cli)
  {
    have_been_interrupted.clear(std::memory_order_relaxed);
#if !defined(_WIN32)
    if (cli) {
      cli_ = true;
      set_cli_interrupt_active(true);
      return;
    }
#else
    (void)cli;
#endif
    struct sigaction action{};
    action.sa_handler = handle_sigint;
    sigemptyset(&action.sa_mask);
    action.sa_flags = SA_RESTART | SA_RESETHAND;
    installed_ = sigaction(SIGINT, &action, &previous_) == 0;
  }

  ~InterruptHandler()
  {
#if !defined(_WIN32)
    if (cli_) {
      set_cli_interrupt_active(false);
      return;
    }
#endif
    if (installed_) {
      (void)sigaction(SIGINT, &previous_, nullptr);
    }
  }

  InterruptHandler(InterruptHandler const&) = delete;
  auto operator=(InterruptHandler const&) -> InterruptHandler& = delete;

private:
  struct sigaction previous_{};
  bool installed_{false};
#if !defined(_WIN32)
  bool cli_{false};
#endif
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
  // All dispatch paths reach this through a verified instruction pack.
  // The outer verifier checks the body as a separate execution region:
  // branches and result references stay within it, and the incoming
  // argument pack is checked at its entry. Slicing preserves relative
  // offsets, with the function-end marker becoming the one-past target.
  // Full checks repeat verification of the extracted body for diagnostics.
  if (full_checks()) {
    validate(instrs, true);
  }
  functions_.at(static_cast<unsigned>(func)) =
    std::make_optional(std::make_tuple(FunctionCode(std::move(instrs)), mask, loc));
}

#if defined(GD_BC_PREPARSED_LIBRARY)
void GD::Bc::Details::VMState::defer_library_function(Letter name, std::string_view image,
                                                     Instructions (*decode)(std::string_view))
{
  auto const index = static_cast<unsigned>(name);
  functions_.at(index).reset();
  deferred_functions_.at(index) = {image, decode};
}
#endif

auto GD::Bc::Details::VMState::call(Letter func, Location const& loc) -> GD::Bc::Number
{
  GD_BC_PROFILE_EVENT(Profile::Event::function_calls, 1);
  auto const index = static_cast<unsigned>(func);
  auto& func_ptr = functions_.at(index);
#if defined(GD_BC_PREPARSED_LIBRARY)
  if (!func_ptr.has_value()) {
    auto& deferred = deferred_functions_.at(index);
    if (deferred.decode != nullptr) {
      auto const code = deferred.decode(deferred.image);
      validate(code);
      // The generator records only isolated definition items here. Outer
      // validation proves the extracted function's offsets and operands.
      assert(code.front().opcode() == Instruction::Opcode::function_begin);
      assert(code.back().opcode() == Instruction::Opcode::function_end);
      assert(std::get<Letter>(code.back().op1()) == func);
      function(func, code.cbegin() + 1, code.cend() - 1,
               std::get<VariableMask>(code.front().op1()),
               std::get<Location>(code.front().op2()));
      deferred = {};
    }
  }
#endif
  if (!func_ptr.has_value()) {
    Details::error(Msg::function_not_defined, func, loc.file_name(), loc.line(), loc.column());
  }

  FunctionDefinition& def = func_ptr.value();
  auto locals = std::get<1>(def);
  auto func_instructions = std::get<0>(def).view(full_checks(), locals);

  // Retain offsets rather than references across execute(): recursive calls
  // may grow the same buffer. Capacity remains available after restoration.
  auto const saved_begin = saved_locals_.size();
  auto const saved_scale = scale_;
  auto const saved_ibase = ibase_;
  auto const saved_obase = obase_;
  auto restore = [&](bool partial) noexcept {
    auto saved = saved_begin;
    auto const end = saved_locals_.size();
    // Allocation can fail during frame setup. In that case restore only the
    // prefix that was saved, in the same scalar-then-array order.
    locals.for_each_variable([&](Letter l) {
      if (!partial || saved < end)
        variables_.at(static_cast<unsigned>(l)) =
          std::move(std::get<Number>(saved_locals_[saved++]));
    });
    locals.for_each_array([&](Letter l) {
      if (!partial || saved < end)
        arrays_.at(static_cast<unsigned>(l)) =
          std::move(std::get<ArrayValues>(saved_locals_[saved++]));
    });
    saved_locals_.resize(saved_begin);
    if (save_specials_) {
      scale_ = saved_scale;
      ibase_ = saved_ibase;
      obase_ = saved_obase;
    }
  };

  try {
    locals.for_each_variable([this](Letter l) {
      saved_locals_.emplace_back(std::move(variables_.at(static_cast<unsigned>(l))));
      variables_.at(static_cast<unsigned>(l)) = Number(0);
    });
    locals.for_each_array([this](Letter l) {
      saved_locals_.emplace_back(std::move(arrays_.at(static_cast<unsigned>(l))));
      arrays_.at(static_cast<unsigned>(l)).reset();
    });

    Instruction::Index start = 0;
#if !GD_BC_PROFILE
    auto const& code = std::get<0>(def);
    auto const parameter = code.prepared_ ? code.prepared_->scalar_parameter : std::nullopt;
    if (parameter) {
      start = FunctionCode::scalar_prologue_size_;
    }
#endif
    InstructionPack fn(this, func_instructions);
#if !GD_BC_PROFILE
    if (start != 0 && !have_been_interrupted.test(std::memory_order_relaxed)) {
      variables_.at(static_cast<unsigned>(parameter->get())) = pop_param();
      if (!have_been_interrupted.test(std::memory_order_relaxed)) {
        pop_param_pack();
      }
    }
#endif
    auto [result, cont] = fn.execute_from(start);
    restore(false);
    return result;
  }
  catch (...) {
    restore(true);
    throw;
  }
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

auto GD::Bc::Details::VMState::acquire_results(std::size_t count, ExecutionLayout const& layout)
  -> ResultBuffer*
{
  if (result_depth_ == result_buffers_.size()) {
    result_buffers_.emplace_back();
  }
  auto& result = result_buffers_[result_depth_];
#if GD_BC_TYPED_RESULTS && GD_BC_COMPACT_EXECUTION
  assert(std::visit(
    [](auto const& buffers) {  // NOLINT
      if constexpr (std::is_same_v<std::decay_t<decltype(buffers)>, ResultBuffer>)
        return buffers.empty();
      else
        return buffers.numbers.empty() && buffers.others.empty();
    },
    result));
  try {
    if (auto const* typed = std::get_if<TypedExecutionLayout>(&layout)) {
      if (!std::holds_alternative<TypedResultBuffers>(result))
        result.emplace<TypedResultBuffers>();
      auto& buffers = std::get<TypedResultBuffers>(result);
      buffers.numbers.resize(typed->slots.number_count);
      buffers.others.resize(typed->slots.other_count);
    }
    else {
      if (!std::holds_alternative<ResultBuffer>(result))
        result.emplace<ResultBuffer>();
      std::get<ResultBuffer>(result).resize(
        std::get<GenericExecutionLayout>(layout).slots.storage_size(count));
    }
  }
  catch (...) {
    // No depth lease has been acquired. Leave an empty usable format even if
    // format construction or the second lane's allocation failed.
    if (result.valueless_by_exception())
      result.emplace<ResultBuffer>();
    else
      std::visit([](auto& buffers) { buffers.clear(); }, result);
    throw;
  }
  ++result_depth_;
  return std::get_if<ResultBuffer>(&result);
#else
  assert(result.empty());  // NOLINT
  result.resize(layout.slots.storage_size(count));
  ++result_depth_;
  return &result;
#endif
}

void GD::Bc::Details::VMState::release_results()
{
  assert(result_depth_ != 0);  // NOLINT
#if GD_BC_TYPED_RESULTS && GD_BC_COMPACT_EXECUTION
  std::visit([](auto& buffers) { buffers.clear(); }, result_buffers_[--result_depth_]);
#else
  result_buffers_[--result_depth_].clear();
#endif
}

GD::Bc::Details::InstructionPack::InstructionPack(VMState* vm, VerifiedInstructions instrs)
    : vm_(vm), instrs_(instrs.instructions()),
      executable_(instrs.generic_layout() ? &instrs.generic_layout()->code : nullptr),
      results_(vm->acquire_results(instrs_.size(), instrs.layout())),
      consumption_(instrs.consumption()),
      result_indices_(instrs.generic_layout() && !instrs.generic_layout()->slots.indices.empty()
                        ? instrs.generic_layout()->slots.indices.data()
                        : nullptr)
#if GD_BC_TYPED_RESULTS && GD_BC_COMPACT_EXECUTION
      ,
      typed_{instrs.typed_layout(), std::get_if<TypedResultBuffers>(&vm->active_results())}
#endif
{
  assert(vm_ != nullptr);  // NOLINT
}

auto GD::Bc::Details::InstructionPack::execute() -> std::pair<GD::Bc::Number, bool>
{
#if GD_BC_TYPED_RESULTS && GD_BC_COMPACT_EXECUTION
  if (typed_.layout)
    return execute_typed();
#endif
  if (!executable_->empty())
    return execute_compact();
  return vm_->full_checks() ? execute_impl<true>() : execute_impl<false>();
}

auto GD::Bc::Details::InstructionPack::execute_compact() -> std::pair<GD::Bc::Number, bool>
{
  using O = Instruction::Opcode;
  auto const& executable = *executable_;
  while (pc_ < executable.size()) {
    if (Details::have_been_interrupted.test(std::memory_order_relaxed))
      return std::make_pair(Number(0), true);
    GD_BC_PROFILE_EVENT(Profile::Event::instructions_executed, 1);
    auto const& instruction = executable[pc_];
    auto& destination = (*results_)[instruction.result];
    auto take = [&]() -> Number {
      auto& number = std::get<Number>(*(*results_)[instruction.first.index]);
      if (number.has_heap_storage() && !consumption_.empty() && consumption_[pc_] != 0) {
        GD_BC_PROFILE_EVENT(Profile::Event::temporary_consumptions, 1);
        return std::move(number);
      }
      return number;
    };
    auto unary = [&](auto operation) {
      Number value = take();
      operation(value);
      destination = std::move(value);
    };
    auto binary = [&](auto operation) {
      Number lhs = take();
      Number const& rhs = std::get<Number>(*(*results_)[instruction.second.index]);
      operation(lhs, rhs);
      destination = std::move(lhs);
    };
    switch (instruction.opcode) {
    case O::string:
      destination = std::string_view(std::get<std::string>(instrs_[pc_].op1()));
      break;
    case O::number:
      destination = instrs_[pc_].decoded_number(vm_->ibase());
      break;
    case O::variable:
      destination = Variable(instruction.first.letter);
      break;
    case O::array:
      destination = Array(instruction.first.letter);
      break;
    case O::array_element:
      destination =
        ArrayElement(Array(instruction.first.letter),
                     std::get<Number>(*(*results_)[instruction.second.index]).to_unsigned());
      break;
    case O::ibase:
      destination = Ibase();
      break;
    case O::obase:
      destination = Obase();
      break;
    case O::scale:
      destination = Scale();
      break;
    case O::print:
      print_value((*results_)[instruction.first.index],
                  static_cast<Instruction::Stream>(instruction.second.index),
                  instruction.first.index);
      break;
    case O::quit:
      std::exit(static_cast<int>(instruction.first.index));  // NOLINT(concurrency-mt-unsafe)
    case O::load:
      load_value(destination, (*results_)[instruction.first.index], instruction.first.index);
      break;
    case O::store:
      store_value((*results_)[instruction.first.index], (*results_)[instruction.second.index],
                  instruction.first.index, instruction.second.index);
      break;
    case O::length:
      length_value(destination, (*results_)[instruction.first.index], instruction.first.index);
      break;
    case O::negate:
      unary([](Number& value) { value.negate(); });
      break;
    case O::abs:
      unary([](Number& value) { value.abs(); });
      break;
    case O::sqrt:
      unary([this](Number& value) { value.sqrt(vm_->scale()); });
      break;
    case O::scale_expr:
      unary([](Number& value) { value = Number(value.scale()); });
      break;
    case O::add:
      binary([](Number& lhs, Number const& rhs) { lhs.add(rhs); });
      break;
    case O::subtract:
      binary([](Number& lhs, Number const& rhs) { lhs.sub(rhs); });
      break;
    case O::multiply:
      binary([this](Number& lhs, Number const& rhs) { lhs.multiply(rhs, vm_->scale()); });
      break;
    case O::divide:
      binary([this](Number& lhs, Number const& rhs) {
#if GD_BC_PREPARED_DIVISORS
        if (rhs.has_heap_storage() && vm_->try_divide_with_reuse(lhs, rhs, false))
          return;
#endif
        lhs.divide(rhs, vm_->scale());
      });
      break;
    case O::modulo:
      binary([this](Number& lhs, Number const& rhs) {
#if GD_BC_PREPARED_DIVISORS
        if (rhs.has_heap_storage() && vm_->try_divide_with_reuse(lhs, rhs, true))
          return;
#endif
        lhs.modulo(rhs, vm_->scale());
      });
      break;
    case O::power:
      binary([this](Number& lhs, Number const& rhs) { lhs.power(rhs, vm_->scale()); });
      break;
    case O::less_than:
      binary([](Number& lhs, Number const& rhs) { lhs = Number(lhs < rhs ? 1 : 0); });
      break;
    case O::less_than_equals:
      binary([](Number& lhs, Number const& rhs) { lhs = Number(lhs <= rhs ? 1 : 0); });
      break;
    case O::equals:
      binary([](Number& lhs, Number const& rhs) { lhs = Number(lhs == rhs ? 1 : 0); });
      break;
    case O::not_equals:
      binary([](Number& lhs, Number const& rhs) { lhs = Number(lhs != rhs ? 1 : 0); });
      break;
    case O::branch:
      pc_ = instruction.first.index;
      continue;
    case O::branch_zero:
      if (std::get<Number>(*(*results_)[instruction.first.index]).is_zero()) {
        pc_ = instruction.second.index;
        continue;
      }
      break;
    case O::function_begin: {
      auto const end = instruction.first.index - 1;
      vm_->function(std::get<Letter>(instrs_[end].op1()),
                    instrs_.begin() + static_cast<Instruction::Offset>(pc_) + 1,
                    instrs_.begin() + static_cast<Instruction::Offset>(end),
                    std::get<VariableMask>(instrs_[pc_].op1()),
                    std::get<Location>(instrs_[pc_].op2()));
      pc_ = instruction.first.index;
      continue;
    }
    case O::push_param_mark:
      vm_->push_param_pack();
      break;
    case O::push_param:
      push_value((*results_)[instruction.first.index], instruction.first.index);
      break;
    case O::pop_param_mark:
      vm_->pop_param_pack();
      break;
    case O::pop_param:
      destination = vm_->pop_param();
      break;
    case O::pop_param_array:
      destination = vm_->pop_param_array();
      break;
    case O::call:
      destination = vm_->call(instruction.first.letter, std::get<Location>(instrs_[pc_].op2()));
      break;
    case O::return_:
      return std::make_pair(std::move(std::get<Number>(*(*results_)[instruction.first.index])),
                            true);
    case O::eof:
      return std::make_pair(Number(0), false);
    case O::function_end:
      std::abort();  // Definition metadata cannot be reached in a verified activation.
    }
    ++pc_;
  }
  return std::make_pair(Number(0), true);
}

#if GD_BC_TYPED_RESULTS && GD_BC_COMPACT_EXECUTION
#if defined(GD_BC_THREADED_DISPATCH)
#  if defined(__clang__)
#    pragma clang diagnostic push
#    pragma clang diagnostic ignored "-Wgnu-label-as-value"
#  elif defined(__GNUC__)
#    pragma GCC diagnostic push
#    pragma GCC diagnostic ignored "-Wpedantic"
#  endif
#endif
auto GD::Bc::Details::InstructionPack::execute_typed() -> std::pair<Number, bool>
{
  using O = TypedOpcode;
  auto const& executable = typed_.layout->code;
  // These buffers belong to this depth until pack destruction. Nested calls
  // grow other deque entries and cannot resize either active lane.
  auto* const numbers = typed_.buffers->numbers.data();
  auto* const others = typed_.buffers->others.data();
#if defined(GD_BC_THREADED_DISPATCH)
  // Match the contiguous TypedOpcode order. Only verified typed code
  // reaches this executor, as with the switch fallback.
  static void* const targets[] = {
    &&op_eof, &&op_print_number, &&op_print_other, &&op_quit,
    &&op_string, &&op_number, &&op_variable, &&op_array_element,
    &&op_array, &&op_scale, &&op_ibase, &&op_obase,
    &&op_add, &&op_subtract, &&op_negate, &&op_multiply,
    &&op_divide, &&op_modulo, &&op_power, &&op_load_number,
    &&op_load_array, &&op_store_number, &&op_store_array, &&op_scale_expr,
    &&op_sqrt, &&op_abs, &&op_length_number, &&op_length_array,
    &&op_equals, &&op_less_than_equals, &&op_not_equals, &&op_less_than,
    &&op_branch, &&op_branch_zero, &&op_return_, &&op_call,
    &&op_push_param_mark, &&op_pop_param_mark, &&op_push_number, &&op_push_array,
    &&op_pop_param, &&op_pop_param_array, &&op_function_begin, &&op_function_end,
    &&op_load_scalar, &&op_store_scalar,
  };
  static_assert(std::size(targets) == static_cast<std::size_t>(O::store_scalar) + 1);
  TypedExecutableInstruction const* instruction = nullptr;
#  define GD_BC_BRANCH()                                                           \
    do {                                                                         \
      if (pc_ >= executable.size() ||                                            \
          Details::have_been_interrupted.test(std::memory_order_relaxed))          \
        return std::make_pair(Number(0), true);                                   \
      GD_BC_PROFILE_EVENT(Profile::Event::instructions_executed, 1);               \
      GD_BC_PROFILE_EVENT(Profile::Event::typed_instructions_executed, 1);         \
      instruction = &executable[pc_];                                             \
      goto *targets[static_cast<std::size_t>(instruction->opcode)];                \
    } while (false)
#  define GD_BC_NEXT() \
    ++pc_;             \
    GD_BC_BRANCH()
#  define GD_BC_OPCODE(name) op_##name
#else
#  define GD_BC_OPCODE(name) case O::name
#  define GD_BC_NEXT() break
#  define GD_BC_BRANCH() continue
  while (pc_ < executable.size()) {
    if (Details::have_been_interrupted.test(std::memory_order_relaxed))
      return std::make_pair(Number(0), true);
    GD_BC_PROFILE_EVENT(Profile::Event::instructions_executed, 1);
    GD_BC_PROFILE_EVENT(Profile::Event::typed_instructions_executed, 1);
    auto const* instruction = &executable[pc_];
#endif
    auto take = [&]() -> Number {
      auto& number = numbers[instruction->first.index];
      if (number.has_heap_storage() && !consumption_.empty() && consumption_[pc_] != 0) {
        GD_BC_PROFILE_EVENT(Profile::Event::temporary_consumptions, 1);
        return std::move(number);
      }
      return number;
    };
    auto unary = [&](auto operation) {
      Number value = take();
      operation(value);
      numbers[instruction->result] = std::move(value);
    };
    auto binary = [&](auto operation) {
      Number lhs = take();
      Number const& rhs = numbers[instruction->second.index];
      operation(lhs, rhs);
      numbers[instruction->result] = std::move(lhs);
    };
#if defined(GD_BC_THREADED_DISPATCH)
    GD_BC_BRANCH();
#else
    switch (instruction->opcode) {
#endif
    GD_BC_OPCODE(string):
      others[instruction->result] = std::string_view(std::get<std::string>(instrs_[pc_].op1()));
      GD_BC_NEXT();
    GD_BC_OPCODE(number):
      numbers[instruction->result] = instrs_[pc_].decoded_number(vm_->ibase());
      GD_BC_NEXT();
    GD_BC_OPCODE(variable):
      others[instruction->result] = Variable(instruction->first.letter);
      GD_BC_NEXT();
    GD_BC_OPCODE(array):
      others[instruction->result] = Array(instruction->first.letter);
      GD_BC_NEXT();
    GD_BC_OPCODE(array_element):
      others[instruction->result] = ArrayElement(Array(instruction->first.letter),
                                                numbers[instruction->second.index].to_unsigned());
      GD_BC_NEXT();
    GD_BC_OPCODE(ibase):
      others[instruction->result] = Ibase();
      GD_BC_NEXT();
    GD_BC_OPCODE(obase):
      others[instruction->result] = Obase();
      GD_BC_NEXT();
    GD_BC_OPCODE(scale):
      others[instruction->result] = Scale();
      GD_BC_NEXT();
    GD_BC_OPCODE(print_number): {
      auto& output = vm_->stream(static_cast<Instruction::Stream>(instruction->second.index));
      numbers[instruction->first.index].output(output, vm_->obase(), 0);
      output << '\n';
      GD_BC_NEXT();
    }
    GD_BC_OPCODE(print_other): {
      auto& output = vm_->stream(static_cast<Instruction::Stream>(instruction->second.index));
      std::visit(Overloaded{[](std::monostate) { std::abort(); },
                            [&output](std::string_view value) { output << value; },
                            [&output](Variable value) { output << value << '\n'; },
                            [&output](Array value) { output << value << '\n'; },
                            [&output](ArrayElement const& value) {
                              output << value.first << '[' << value.second << "]\n";
                            },
                            [&output](ArrayValues const&) { output << "<ARRAY VALUES>\n"; },
                            [&output](Ibase) { output << "ibase\n"; },
                            [&output](Obase) { output << "obase\n"; },
                            [&output](Scale) { output << "scale\n"; }},
                 others[instruction->first.index]);
      GD_BC_NEXT();
    }
    GD_BC_OPCODE(quit):
      std::exit(static_cast<int>(instruction->first.index));  // NOLINT(concurrency-mt-unsafe)
    GD_BC_OPCODE(load_scalar):
      numbers[instruction->result] = vm_->variable(Variable(instruction->first.letter));
      GD_BC_NEXT();
    GD_BC_OPCODE(store_scalar):
      vm_->variable(Variable(instruction->first.letter), numbers[instruction->second.index]);
      GD_BC_NEXT();
    GD_BC_OPCODE(load_number): {
      auto& destination = numbers[instruction->result];
      std::visit(
        Overloaded{[&destination, this](Variable value) { destination = vm_->variable(value); },
                   [&destination, this](ArrayElement const& value) {
                     destination = vm_->array_element(value);
                   },
                   [&destination, this](Ibase) { destination = Number(vm_->ibase()); },
                   [&destination, this](Obase) { destination = Number(vm_->obase()); },
                   [&destination, this](Scale) { destination = Number(vm_->scale()); },
                   [](auto const&) { std::abort(); }},
        others[instruction->first.index]);
      GD_BC_NEXT();
    }
    GD_BC_OPCODE(load_array):
      others[instruction->result] = vm_->array(std::get<Array>(others[instruction->first.index]));
      GD_BC_NEXT();
    GD_BC_OPCODE(store_number): {
      auto const& value = numbers[instruction->second.index];
      std::visit(Overloaded{[&value, this](Variable location) { vm_->variable(location, value); },
                            [&value, this](ArrayElement const& location) {
                              vm_->array_element(location, value);
                            },
                            [&value, this](Ibase) { vm_->ibase(value); },
                            [&value, this](Obase) { vm_->obase(value); },
                            [&value, this](Scale) { vm_->scale(value); },
                            [](auto const&) { std::abort(); }},
                 others[instruction->first.index]);
      GD_BC_NEXT();
    }
    GD_BC_OPCODE(store_array):
      vm_->array(std::get<Array>(others[instruction->first.index]),
                 std::get<ArrayValues>(others[instruction->second.index]));
      GD_BC_NEXT();
    GD_BC_OPCODE(length_number):
      numbers[instruction->result] = Number(numbers[instruction->first.index].length());
      GD_BC_NEXT();
    GD_BC_OPCODE(length_array):
      numbers[instruction->result] =
        Number(vm_->array_length(std::get<Array>(others[instruction->first.index])));
      GD_BC_NEXT();
    GD_BC_OPCODE(negate):
      unary([](Number& value) { value.negate(); });
      GD_BC_NEXT();
    GD_BC_OPCODE(abs):
      unary([](Number& value) { value.abs(); });
      GD_BC_NEXT();
    GD_BC_OPCODE(sqrt):
      unary([this](Number& value) { value.sqrt(vm_->scale()); });
      GD_BC_NEXT();
    GD_BC_OPCODE(scale_expr):
      unary([](Number& value) { value = Number(value.scale()); });
      GD_BC_NEXT();
    GD_BC_OPCODE(add):
      binary([](Number& lhs, Number const& rhs) { lhs.add(rhs); });
      GD_BC_NEXT();
    GD_BC_OPCODE(subtract):
      binary([](Number& lhs, Number const& rhs) { lhs.sub(rhs); });
      GD_BC_NEXT();
    GD_BC_OPCODE(multiply):
      binary([this](Number& lhs, Number const& rhs) { lhs.multiply(rhs, vm_->scale()); });
      GD_BC_NEXT();
    GD_BC_OPCODE(divide):
      binary([this](Number& lhs, Number const& rhs) {
#if GD_BC_PREPARED_DIVISORS
        if (rhs.has_heap_storage() && vm_->try_divide_with_reuse(lhs, rhs, false))
          return;
#endif
        lhs.divide(rhs, vm_->scale());
      });
      GD_BC_NEXT();
    GD_BC_OPCODE(modulo):
      binary([this](Number& lhs, Number const& rhs) {
#if GD_BC_PREPARED_DIVISORS
        if (rhs.has_heap_storage() && vm_->try_divide_with_reuse(lhs, rhs, true))
          return;
#endif
        lhs.modulo(rhs, vm_->scale());
      });
      GD_BC_NEXT();
    GD_BC_OPCODE(power):
      binary([this](Number& lhs, Number const& rhs) { lhs.power(rhs, vm_->scale()); });
      GD_BC_NEXT();
    GD_BC_OPCODE(less_than):
      binary([](Number& lhs, Number const& rhs) { lhs = Number(lhs < rhs ? 1 : 0); });
      GD_BC_NEXT();
    GD_BC_OPCODE(less_than_equals):
      binary([](Number& lhs, Number const& rhs) { lhs = Number(lhs <= rhs ? 1 : 0); });
      GD_BC_NEXT();
    GD_BC_OPCODE(equals):
      binary([](Number& lhs, Number const& rhs) { lhs = Number(lhs == rhs ? 1 : 0); });
      GD_BC_NEXT();
    GD_BC_OPCODE(not_equals):
      binary([](Number& lhs, Number const& rhs) { lhs = Number(lhs != rhs ? 1 : 0); });
      GD_BC_NEXT();
    GD_BC_OPCODE(branch):
      pc_ = instruction->first.index;
      GD_BC_BRANCH();
    GD_BC_OPCODE(branch_zero):
      if (numbers[instruction->first.index].is_zero()) {
        pc_ = instruction->second.index;
        GD_BC_BRANCH();
      }
      GD_BC_NEXT();
    GD_BC_OPCODE(function_begin): {
      auto const end = instruction->first.index - 1;
      vm_->function(std::get<Letter>(instrs_[end].op1()),
                    instrs_.begin() + static_cast<Instruction::Offset>(pc_) + 1,
                    instrs_.begin() + static_cast<Instruction::Offset>(end),
                    std::get<VariableMask>(instrs_[pc_].op1()),
                    std::get<Location>(instrs_[pc_].op2()));
      pc_ = instruction->first.index;
      GD_BC_BRANCH();
    }
    GD_BC_OPCODE(push_param_mark):
      vm_->push_param_pack();
      GD_BC_NEXT();
    GD_BC_OPCODE(push_number):
      vm_->push_param(numbers[instruction->first.index]);
      GD_BC_NEXT();
    GD_BC_OPCODE(push_array):
      vm_->push_param(std::get<ArrayValues>(others[instruction->first.index]));
      GD_BC_NEXT();
    GD_BC_OPCODE(pop_param_mark):
      vm_->pop_param_pack();
      GD_BC_NEXT();
    GD_BC_OPCODE(pop_param):
      numbers[instruction->result] = vm_->pop_param();
      GD_BC_NEXT();
    GD_BC_OPCODE(pop_param_array):
      others[instruction->result] = vm_->pop_param_array();
      GD_BC_NEXT();
    GD_BC_OPCODE(call):
      numbers[instruction->result] =
        vm_->call(instruction->first.letter, std::get<Location>(instrs_[pc_].op2()));
      GD_BC_NEXT();
    GD_BC_OPCODE(return_):
      return std::make_pair(std::move(numbers[instruction->first.index]), true);
    GD_BC_OPCODE(eof):
      return std::make_pair(Number(0), false);
    GD_BC_OPCODE(function_end):
      std::abort();
#if !defined(GD_BC_THREADED_DISPATCH)
    }
    ++pc_;
  }
  return std::make_pair(Number(0), true);
#endif
#undef GD_BC_OPCODE
#undef GD_BC_NEXT
#undef GD_BC_BRANCH
}
#if defined(GD_BC_THREADED_DISPATCH)
#  if defined(__clang__)
#    pragma clang diagnostic pop
#  elif defined(__GNUC__)
#    pragma GCC diagnostic pop
#  endif
#endif
#endif

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
  print_value(result_at(result_idx), std::get<Instruction::Stream>(instrs_[pc_].op2()), result_idx);
}

void GD::Bc::Details::InstructionPack::print_value(ResultSlot const& result,
                                                   Instruction::Stream stream, Index result_idx)
{
  std::ostream& os = vm_->stream(stream);
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
  length_value(result_at(pc_), result_at(loc_idx), loc_idx);
}

void GD::Bc::Details::InstructionPack::length_value(ResultSlot& result, ResultSlot const& loc,
                                                    Index loc_idx)
{
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
  load_value(result_at(pc_), result_at(loc_idx), loc_idx);
}

void GD::Bc::Details::InstructionPack::load_value(ResultSlot& result, ResultSlot const& loc,
                                                  Index loc_idx)
{
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
  Index const expr_idx = get_offset_index(instrs_[pc_].op2());
  store_value(result_at(loc_idx), result_at(expr_idx), loc_idx, expr_idx);
}

void GD::Bc::Details::InstructionPack::store_value(ResultSlot const& loc, ResultSlot const& result,
                                                   Index loc_idx, Index expr_idx)
{
  assert_error(loc.has_value(), Msg::empty_result, loc_idx);      // NOLINT
  assert_error(result.has_value(), Msg::empty_result, expr_idx);  // NOLINT
  auto const& expr = *result;

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
  push_value(result_at(expr_idx), expr_idx);
}

void GD::Bc::Details::InstructionPack::push_value(ResultSlot const& expr, Index expr_idx)
{
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
#if GD_BC_TYPED_RESULTS && GD_BC_COMPACT_EXECUTION
    if (instrs.typed_.layout) {
      // Entry verification proves consumer availability, not a live-value dump
      // for every future producer. Do not inspect unwritten typed registers.
      os << '\n';
      continue;
    }
#endif
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

#if defined(GD_BC_PREPARSED_LIBRARY)
void GD::Bc::VM::defer_library_function(Letter name, std::string_view image,
                                       Instructions (*decode)(std::string_view))
{
  state_->defer_library_function(name, image, decode);
}
#endif

auto GD::Bc::VM::execute(Instructions& instructions) -> bool
{
  return execute_impl(instructions, false);
}

auto GD::Bc::VM::execute_cli(Instructions& instructions, Details::CliInterruptScope const& cli)
  -> bool
{
  return execute_impl(instructions, cli.installed());
}

auto GD::Bc::VM::execute_impl(Instructions& instructions, bool cli_interrupts) -> bool
{
  auto const verified = state_->verify(instructions);
  Details::InstructionPack instrs(state_.get(), verified.view());
  // Restore the caller's handler before reporting interruption, including when
  // execution throws (for example, through an output stream's exception mask).
  auto const result = [&, this, cli_interrupts]() {
    try {
      Details::InterruptHandler handler(cli_interrupts);
#ifdef GD_BC_NATIVE_SCALAR
      if (!state_->full_checks() && state_->ibase() == 10 &&
          !Details::have_been_interrupted.test(std::memory_order_relaxed)) {
        auto const variables = Details::native_scalar_variables(instructions);
        auto const variable = [](unsigned index) {
          return Variable(static_cast<char>('a' + index));
        };
        bool eligible = variables.any();
        for (unsigned index = 0; eligible && index < Letter::count_; ++index) {
          if (variables.test(index)) {
            auto const& value = state_->variable(variable(index));
            eligible = value.is_zero() && value.scale() == 0;
          }
        }
        if (eligible) {
          auto const interrupted = []() noexcept {
            return Details::have_been_interrupted.test(std::memory_order_relaxed);
          };
          auto const result = Details::run_native_scalar(instructions, variables, interrupted);
          if (result && !Details::have_been_interrupted.test(std::memory_order_relaxed)) {
            std::array<Number, Letter::count_> pending;
            for (unsigned index = 0; index < Letter::count_; ++index) {
              if (variables.test(index))
                pending[index] = Number(std::to_string((*result)[index]), 10);
            }
            if (!Details::have_been_interrupted.test(std::memory_order_relaxed)) {
              for (unsigned index = 0; index < Letter::count_; ++index) {
                if (variables.test(index))
                  state_->variable(variable(index), pending[index]);
              }
              return std::make_pair(Number(0), true);
            }
          }
        }
      }
#endif
      return instrs.execute();
    }
    catch (...) {
      state_->discard_parameters();
      Details::have_been_interrupted.clear(std::memory_order_relaxed);
      throw;
    }
  }();
  if (Details::have_been_interrupted.test(std::memory_order_relaxed)) {
    state_->discard_parameters();
    Details::have_been_interrupted.clear(std::memory_order_relaxed);
    state_->stream(Instruction::Stream::error) << Messages::get().format(Msg::interrupted) << '\n';
  }
  return result.second;
}
