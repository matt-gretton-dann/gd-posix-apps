/** Lazy private scalar storage for the optional closed-function experiment.
 * SPDX-License-Identifier: Apache-2.0
 */
#ifndef GD_BC_SCALAR_ACTIVATION_HH
#define GD_BC_SCALAR_ACTIVATION_HH

#include <array>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <vector>

#include "bc.hh"
#include "number.hh"

namespace GD::Bc::Details {
struct PrivateScalarPlan
{
  std::array<std::uint8_t, Letter::count_> offsets{};
  std::size_t count = 0;
  bool eligible = false;
};

/** A private activation cannot expose unpublished locals to another body.
 * Reject free scalar bindings, arrays, output and non-self calls. This proof
 * concerns verified bodies only; normal execution remains the fallback.
 */
inline auto plan_private_scalars(Instructions const& code, VariableMask locals, Letter self)
  -> PrivateScalarPlan
{
  PrivateScalarPlan plan;
  bool arrays = false;
  locals.for_each_array([&](Letter) { arrays = true; });
  if (arrays)
    return plan;
  locals.for_each_variable([&](Letter l) {
    plan.offsets[static_cast<unsigned>(l)] = static_cast<std::uint8_t>(plan.count++);
  });
  if (plan.count == 0)
    return plan;
  for (auto const& instruction : code) {
    using O = Instruction::Opcode;
    switch (instruction.opcode()) {
    case O::call:
      if (std::get<Letter>(instruction.op1()) != self)
        return plan;
      break;
    case O::number:
    case O::string:
    case O::variable:
    case O::scale:
    case O::ibase:
    case O::obase:
    case O::add:
    case O::subtract:
    case O::negate:
    case O::multiply:
    case O::divide:
    case O::modulo:
    case O::power:
    case O::load:
    case O::store:
    case O::scale_expr:
    case O::sqrt:
    case O::abs:
    case O::length:
    case O::equals:
    case O::less_than_equals:
    case O::not_equals:
    case O::less_than:
    case O::branch:
    case O::branch_zero:
    case O::return_:
    case O::push_param_mark:
    case O::pop_param_mark:
    case O::push_param:
    case O::pop_param:
      break;
    default:
      // New instructions need an explicit observability review.
      return plan;
    }
    auto safe = [&](Instruction::Operand const& operand) {
      if (auto const* v = std::get_if<Variable>(&operand))
        return locals.contains(*v);
      return !std::holds_alternative<Array>(operand);
    };
    if ((instruction.has_op1() && !safe(instruction.op1())) ||
        (instruction.has_op2() && !safe(instruction.op2())))
      return plan;
  }
  plan.eligible = true;
  return plan;
}

/** Stable chunks retain the simultaneous high-water demand, not a maximum
 * frame for each call depth. A lease is released in strict LIFO order.
 */
class ScalarActivationArena
{
public:
  struct Cell
  {
    // An empty cell does not construct a Number.
    Cell() : value(std::nullopt) {}
    std::optional<Number> value;
  };
  class Lease
  {
  public:
    Lease(ScalarActivationArena& arena, std::size_t count)
        : arena_(arena), old_chunk_(arena.chunk_), old_used_(arena.used_), count_(count)
    {
      assert(count > 0 && count <= Letter::count_);
      auto chunk = arena.chunk_;
      auto used = arena.used_;
      if (used + count > chunk_size_) {
        ++chunk;
        used = 0;
      }
      // Complete allocation before publishing the cursor or any active cells.
      if (chunk == arena.chunks_.size())
        arena.chunks_.emplace_back(chunk_size_);
      cells_ = arena.chunks_[chunk].data() + used;
      arena.chunk_ = chunk;
      arena.used_ = used + count;
    }
    ~Lease()
    {
      assert(arena_.used_ >= count_);
      assert(arena_.chunks_[arena_.chunk_].data() + arena_.used_ - count_ == cells_);
      for (std::size_t i = 0; i < count_; ++i)
        cells_[i].value.reset();
      arena_.chunk_ = old_chunk_;
      arena_.used_ = old_used_;
    }
    Lease(Lease const&) = delete;
    auto operator=(Lease const&) -> Lease& = delete;
    auto cells() const -> Cell* { return cells_; }

  private:
    ScalarActivationArena& arena_;
    std::size_t old_chunk_;
    std::size_t old_used_;
    std::size_t count_;
    Cell* cells_ = nullptr;
  };

private:
  static constexpr std::size_t chunk_size_ = 32;
  using Chunk = std::vector<Cell, Profile::Allocator<Cell, Profile::AllocationKind::frames>>;
  std::vector<Chunk, Profile::Allocator<Chunk, Profile::AllocationKind::frames>> chunks_;
  std::size_t chunk_ = 0;
  std::size_t used_ = 0;
};

struct PrivateScalarFrame
{
  PrivateScalarPlan const* plan;
  ScalarActivationArena::Cell* cells;

  auto cell(Variable v) const -> std::optional<Number>&
  {
    return cells[plan->offsets[static_cast<unsigned>(v.get())]].value;
  }
  auto read(Variable v) const -> Number const&
  {
    auto const& value = cell(v);
    static Number const zero(0);
    return value ? *value : zero;
  }
};
}  // namespace GD::Bc::Details
#endif
