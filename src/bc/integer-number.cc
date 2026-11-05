/** Ordinary Number arithmetic boundary for the optional integer engine.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <utility>

#include "integer-function.hh"

namespace GD::Bc::Details {
struct IntegerSpillBank::Storage
{
  struct Spill
  {
    Number value;
    std::size_t charge;
  };
  std::vector<Spill, Profile::Allocator<Spill, Profile::AllocationKind::frames>> spills;
};

IntegerSpillBank::IntegerSpillBank() = default;
IntegerSpillBank::~IntegerSpillBank() = default;
IntegerSpillBank::IntegerSpillBank(IntegerSpillBank&& other) noexcept
    : storage_(std::move(other.storage_)), bytes_(std::exchange(other.bytes_, 0))
{
}

auto IntegerSpillBank::operator=(IntegerSpillBank&& other) noexcept -> IntegerSpillBank&
{
  if (this != &other) {
    storage_ = std::move(other.storage_);
    bytes_ = std::exchange(other.bytes_, 0);
  }
  return *this;
}

void IntegerSpillBank::Delete::operator()(Storage* storage) const noexcept
{
  delete storage;
#if GD_BC_PROFILE
  Profile::deallocated(Profile::AllocationKind::frames, sizeof(Storage));
#endif
}

void IntegerSpillBank::clear() noexcept
{
  if (storage_)
    storage_->spills.clear();
  // Retained header and vector capacities are still owned. Charge the header
  // to the logical bound; vector capacity remains a separate measurement.
  bytes_ = storage_ ? sizeof(Storage) : 0;
}

auto IntegerSpillBank::empty() const noexcept -> bool
{
  return !storage_ || storage_->spills.empty();
}

auto IntegerSpillBank::number(IntegerFunctionValue value) const -> Number
{
  if (!value.big)
    return Number::from_machine_integer(value.payload);
  return storage_->spills[static_cast<std::size_t>(value.payload)].value;
}

auto IntegerSpillBank::normalize(IntegerFunctionValue& target, Number value, std::size_t available)
  -> bool
{
  if (auto const small = value.to_machine_integer()) {
    GD_BC_PROFILE_EVENT(Profile::Event::integer_function_demotions, 1);
    target = {*small, false};
    return true;
  }
  auto const digits = value.length();
  if (digits > 4096)
    return false;
  auto const charge = std::size_t(digits) * sizeof(Number::NumType);
  auto const extra = sizeof(Storage::Spill) + charge + (storage_ ? 0 : sizeof(Storage));
  if (extra > available)
    return false;
  if (!storage_) {
    auto storage = std::make_unique<Storage>();
#if GD_BC_PROFILE
    Profile::allocated(Profile::AllocationKind::frames, sizeof(Storage));
#endif
    storage_.reset(storage.release());
    bytes_ = sizeof(Storage);
  }
  // Append immutable records only after both input snapshots are owned. Bank
  // growth cannot invalidate cells or arguments, which retain indices.
  auto const index = storage_->spills.size();
  storage_->spills.push_back({std::move(value), charge});
  bytes_ += sizeof(Storage::Spill) + charge;
  GD_BC_PROFILE_EVENT(Profile::Event::integer_function_promotions, 1);
  target = {static_cast<std::int64_t>(index), true};
  return true;
}

auto IntegerSpillBank::binary(Instruction::Opcode opcode, IntegerFunctionValue left,
                              IntegerFunctionValue right, IntegerFunctionValue& target,
                              std::size_t available) -> bool
{
  auto value = number(left);
  auto const rhs = number(right);
  auto result = integer_number_binary(opcode, std::move(value), rhs);
  return result && normalize(target, std::move(*result), available);
}

auto IntegerSpillBank::negate(IntegerFunctionValue input, IntegerFunctionValue& target,
                              std::size_t available) -> bool
{
  auto value = number(input);
  value.negate();
  return normalize(target, std::move(value), available);
}

auto run_integer_function(IntegerFunction const& function, std::int64_t argument,
                          IntegerFunctionScratch& scratch, bool (*interrupted)() noexcept)
  -> std::optional<Number>
{
  GD_BC_PROFILE_EVENT(Profile::Event::integer_function_attempts, 1);
  auto reset = [&]() noexcept {
    scratch.values.clear();
    scratch.spills.clear();
    scratch.frames.clear();
    scratch.arguments.clear();
  };
  reset();
  using Reset = decltype(reset);
  struct Clear
  {
    Reset& cleanup;
#if GD_BC_PROFILE
    bool complete = false;
#endif
    ~Clear()
    {
#if GD_BC_PROFILE
      if (!complete)
        GD_BC_PROFILE_EVENT(Profile::Event::integer_function_abandonments, 1);
#endif
      cleanup();
    }
  } clear{reset};
  auto const cell = run_integer_core(function, argument, scratch, interrupted);
  if (!cell)
    return {};
  auto result = scratch.spills.number(*cell);
#if GD_BC_PROFILE
  clear.complete = true;
  GD_BC_PROFILE_EVENT(Profile::Event::integer_function_successes, 1);
#endif
  return result;
}

auto integer_number_binary(Instruction::Opcode opcode, Number left, Number const& right)
  -> std::optional<Number>
{
  using O = Instruction::Opcode;
  switch (opcode) {
  case O::add:
    left.add(right);
    break;
  case O::subtract:
    left.sub(right);
    break;
  case O::multiply:
    left.multiply(right, 0);
    break;
  case O::less_than:
    left = Number(left < right ? 1 : 0);
    break;
  case O::less_than_equals:
    left = Number(left <= right ? 1 : 0);
    break;
  case O::equals:
    left = Number(left == right ? 1 : 0);
    break;
  case O::not_equals:
    left = Number(left != right ? 1 : 0);
    break;
  default:
    return {};
  }
  return left;
}
}  // namespace GD::Bc::Details
