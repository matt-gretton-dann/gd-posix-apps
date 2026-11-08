/** Exact promotion and result ownership remain in the ordinary Number context.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "word-runtime.hh"

#include <cassert>
#include <charconv>
#include <sstream>
#include <string>
#include <type_traits>
#include <utility>

#include "word-lowering.hh"

namespace GD::Bc::Details {
namespace {
auto box_word(std::int64_t value) -> Number
{
  // Unsigned magnitude also handles the most negative signed value exactly.
  auto magnitude = value < 0 ? std::uint64_t(0) - static_cast<std::uint64_t>(value)
                             : static_cast<std::uint64_t>(value);
  Number result = magnitude < Number::base_ ? Number(static_cast<Number::NumType>(magnitude))
                                            : Number(std::to_string(magnitude), 10);
  if (value < 0)
    result.negate();
  return result;
}
auto word(Number const& value) -> std::optional<std::int64_t>
{
  // Only near-range promoted results can demote. Avoid serializing arbitrary
  // large coefficients merely to discover that they do not fit in a word.
  if (value.scale() != 0)
    return {};
  if (value == Number(0))
    return 0;
  if (value.length() > 19)
    return {};
  std::ostringstream out;
  // A partial formatted prefix must never be accepted as the original value.
  // Propagate buffer failures into the optional tier's resource fallback.
  out.exceptions(std::ios::badbit | std::ios::failbit);
  value.output(out, 10, 0);
  auto text = out.str();
  std::int64_t result{};
  auto parsed = std::from_chars(text.data(), text.data() + text.size(), result);
  if (parsed.ec != std::errc{} || parsed.ptr != text.data() + text.size())
    return {};
  return result;
}
}  // namespace

auto WordFunctions::create() -> std::unique_ptr<WordFunctions>
{
  auto result = std::unique_ptr<WordFunctions>(new WordFunctions());
#if GD_BC_PROFILE
  Profile::allocated(Profile::AllocationKind::execution_plans, sizeof(WordFunctions));
#endif
  return result;
}
WordFunctions::~WordFunctions()
{
#if GD_BC_PROFILE
  Profile::deallocated(Profile::AllocationKind::execution_plans, sizeof(WordFunctions));
#endif
}

auto WordFunctions::argument(Number const& value) -> std::optional<std::int64_t>
{
  if (value.scale() != 0)
    return {};
  if (value.has_heap_storage() || value.length() > Number::base_log10_)
    return word(value);
  auto magnitude = value;
  auto negative = magnitude < Number(0);
  magnitude.abs();
  auto result = static_cast<std::int64_t>(magnitude.to_unsigned());
  return negative ? -result : result;
}

void WordFunctions::reset(unsigned index) { entries_.at(index) = Entry{}; }
void WordFunctions::clear_attempt() noexcept
{
  scratch_.clear();
  spills_.clear();
  spill_bytes_ = 0;
  interrupted_ = nullptr;
}
auto WordFunctions::take_result() noexcept -> Number
{
  static_assert(std::is_nothrow_move_constructible_v<Number>);
  assert(result_);
  Number result = std::move(*result_);
  result_.reset();
  return result;
}
auto WordFunctions::number(Word::Value value) const -> Number
{
  return value.big ? spills_.at(static_cast<std::size_t>(value.payload)) : box_word(value.payload);
}
auto WordFunctions::publish(Number value, Word::Value& target, std::size_t available) -> bool
{
  if (value.scale() != 0)
    return false;
  if (auto small = word(value)) {
    target = {*small, false};
    GD_BC_PROFILE_EVENT(Profile::Event::word_function_demotions, 1);
    return true;
  }
  auto digits = value.length();
  if (digits > 4096)
    return false;
  auto charge = sizeof(Number) + std::size_t(digits) * sizeof(Number::NumType);
  if (charge > available)
    return false;
  auto index = spills_.size();
  spills_.push_back(std::move(value));
  spill_bytes_ += charge;
  target = {static_cast<std::int64_t>(index), true};
  GD_BC_PROFILE_EVENT(Profile::Event::word_function_promotions, 1);
  return true;
}
auto WordFunctions::binary(Word::Opcode op, Word::Value left, Word::Value right,
                           Word::Value& target, std::size_t available) -> bool
{
  auto result = number(left);
  auto rhs = number(right);
  using O = Word::Opcode;
  switch (op) {
  case O::add:
    result.add(rhs);
    break;
  case O::subtract:
    result.sub(rhs);
    break;
  case O::multiply:
    result.multiply(rhs, 0);
    break;
  case O::less:
    result = Number(result < rhs ? 1 : 0);
    break;
  case O::less_equal:
    result = Number(result <= rhs ? 1 : 0);
    break;
  case O::equal:
    result = Number(result == rhs ? 1 : 0);
    break;
  case O::not_equal:
    result = Number(result != rhs ? 1 : 0);
    break;
  default:
    return false;
  }
  return publish(std::move(result), target, available);
}
auto WordFunctions::negate(Word::Value input, Word::Value& target, std::size_t available) -> bool
{
  auto result = number(input);
  result.negate();
  return publish(std::move(result), target, available);
}

auto WordFunctions::try_call(unsigned index, Instructions const& source, VariableMask locals,
                             Letter self, std::int64_t argument, bool (*interrupted)() noexcept)
  -> Outcome
{
  discard_result();
  clear_attempt();
  auto& entry = entries_.at(index);
  if (entry.failed_argument == argument)
    return Outcome::failed;
  if (entry.rejected)
    return Outcome::declined;
  struct Clear
  {
    WordFunctions& owner;
    ~Clear() { owner.clear_attempt(); }
  } clear{*this};
  Word::Counters counts;
  struct Count
  {
    Word::Counters& counts;
    ~Count()
    {
      GD_BC_PROFILE_EVENT(Profile::Event::word_function_instructions, counts.instructions);
      GD_BC_PROFILE_EVENT(Profile::Event::word_function_self_calls, counts.self_calls);
    }
  } count{counts};
  try {
    if (!entry.program) {
      GD_BC_PROFILE_EVENT(Profile::Event::word_function_plan_attempts, 1);
      auto program = lower_word_function(source, locals, self);
      if (!program) {
        entry.rejected = true;
        GD_BC_PROFILE_EVENT(Profile::Event::word_function_rejections, 1);
        return Outcome::declined;
      }
      entry.program = std::move(program);
    }
    interrupted_ = interrupted;
    Word::Callbacks callbacks{
      this,
      [](void* p) noexcept {
        auto& owner = *static_cast<WordFunctions*>(p);
        return owner.interrupted_ && owner.interrupted_();
      },
      [](void* p) noexcept { return static_cast<WordFunctions*>(p)->spill_bytes_; },
      [](void* p, Word::Opcode op, Word::Value a, Word::Value b, Word::Value& out,
         std::size_t available) {
        return static_cast<WordFunctions*>(p)->binary(op, a, b, out, available);
      },
      [](void* p, Word::Value a, Word::Value& out, std::size_t available) {
        return static_cast<WordFunctions*>(p)->negate(a, out, available);
      }};
    GD_BC_PROFILE_EVENT(Profile::Event::word_function_attempts, 1);
    auto cell = Word::execute(*entry.program, argument, scratch_, callbacks, counts);
    if (!cell) {
      if (!interrupted || !interrupted())
        entry.failed_argument = argument;
      GD_BC_PROFILE_EVENT(Profile::Event::word_function_abandonments, 1);
      return Outcome::failed;
    }
    result_.emplace(number(*cell));
    if (interrupted && interrupted()) {
      discard_result();
      GD_BC_PROFILE_EVENT(Profile::Event::word_function_abandonments, 1);
      return Outcome::failed;
    }
    // Readiness means boxing completed. Only the VM may commit the pack, then
    // count completion and move this owner without further allocation.
    return Outcome::ready;
  }
  catch (std::bad_alloc const&) {
    // Resource failure is retryable, unlike a deterministic guard decline.
    discard_result();
    GD_BC_PROFILE_EVENT(Profile::Event::word_function_abandonments, 1);
    return Outcome::failed;
  }
}
}  // namespace GD::Bc::Details
