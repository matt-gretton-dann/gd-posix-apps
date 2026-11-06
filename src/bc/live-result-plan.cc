/** Bounded liveness and conservative interference for verified result values.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "live-result-plan.hh"

#include <new>

#include <algorithm>
#include <array>
#include <bit>
#include <cstdint>
#include <initializer_list>
#include <limits>
#include <optional>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

namespace GD::Bc::Details {
namespace {
using Index = Instruction::Index;
using Opcode = Instruction::Opcode;
using Word = std::uint64_t;
using Color = ResultSlotPlan::CompactIndex;
enum class Domain { all, number, other };
constexpr std::size_t word_bits = 64;

auto bit(Index index) -> Word { return Word(1) << (index % word_bits); }

struct Node
{
  std::array<Index, 2> next{};
  std::array<Index, 2> reads{};
  std::size_t next_count = 0;
  std::size_t read_count = 0;
  ResultKind kind = ResultKind::none;
  bool in_region = true;
};

class Analysis
{
public:
  Analysis(std::size_t size, std::size_t words, std::size_t work)
      : size_(size), words_(words), remaining_(work), nodes_(size), reachable_(size)
  {
  }

  auto build(Instructions const& code) -> bool
  {
    std::vector<Index> definition_end(size_, size_);
    Index definition = size_;
    for (Index i = 0; i < size_; ++i) {
      if (code[i].opcode() == Opcode::function_begin) {
        if (definition != size_)
          return false;
        definition = i;
      }
      else if (code[i].opcode() == Opcode::function_end) {
        if (definition == size_)
          return false;
        definition_end[definition] = i;
        nodes_[i].in_region = false;
        definition = size_;
      }
      else if (definition != size_) {
        nodes_[i].in_region = false;
      }
    }
    if (definition != size_)
      return false;

    for (Index i = 0; i < size_; ++i) {
      auto& node = nodes_[i];
      node.kind = classify_result(code, i);
      auto target = [i](Instruction::Operand const& operand) {
        return i + static_cast<Index>(std::get<Instruction::Offset>(operand));
      };
      auto edge = [&](Index next) {
        if (next < size_)
          node.next[node.next_count++] = next;
        return next <= size_;
      };
      auto const opcode = code[i].opcode();
      switch (opcode) {
      case Opcode::branch:
        if (!edge(target(code[i].op1())))
          return false;
        break;
      case Opcode::branch_zero:
        if (!edge(target(code[i].op2())) || !edge(i + 1))
          return false;
        break;
      case Opcode::function_begin:
        if (definition_end[i] == size_ || !edge(definition_end[i] + 1))
          return false;
        break;
      case Opcode::function_end:
      case Opcode::return_:
      case Opcode::quit:
      case Opcode::eof:
        break;
      case Opcode::print:
      case Opcode::string:
      case Opcode::number:
      case Opcode::variable:
      case Opcode::array_element:
      case Opcode::array:
      case Opcode::scale:
      case Opcode::ibase:
      case Opcode::obase:
      case Opcode::add:
      case Opcode::subtract:
      case Opcode::negate:
      case Opcode::multiply:
      case Opcode::divide:
      case Opcode::modulo:
      case Opcode::power:
      case Opcode::load:
      case Opcode::store:
      case Opcode::scale_expr:
      case Opcode::sqrt:
      case Opcode::abs:
      case Opcode::length:
      case Opcode::equals:
      case Opcode::less_than_equals:
      case Opcode::not_equals:
      case Opcode::less_than:
      case Opcode::call:
      case Opcode::push_param_mark:
      case Opcode::pop_param_mark:
      case Opcode::push_param:
      case Opcode::pop_param:
      case Opcode::pop_param_array:
        if (!edge(i + 1))
          return false;
        break;
      default:
        return false;  // New control opcodes require an explicit schema.
      }
      auto read = [&](Instruction::Operand const& operand) {
        if (!std::holds_alternative<Instruction::Offset>(operand))
          return true;
        auto const producer = target(operand);
        if (producer >= size_ || producer == i)
          return false;
        node.reads[node.read_count++] = producer;
        return true;
      };
      // As in consumption planning, code offsets are not result references.
      if (opcode != Opcode::branch && opcode != Opcode::function_end) {
        if (code[i].has_op1() && !read(code[i].op1()))
          return false;
        if (code[i].has_op2() && opcode != Opcode::branch_zero && !read(code[i].op2()))
          return false;
      }
    }
    std::vector<Index> pending{0};
    reachable_[0] = true;
    while (!pending.empty()) {
      auto const i = pending.back();
      pending.pop_back();
      for (std::size_t n = 0; n < nodes_[i].next_count; ++n) {
        auto const next = nodes_[i].next[n];
        if (!reachable_[next]) {
          reachable_[next] = true;
          pending.push_back(next);
        }
      }
    }
    for (Index i = 0; i < size_; ++i) {
      if (!reachable_[i])
        continue;
      for (std::size_t n = 0; n < nodes_[i].read_count; ++n) {
        auto const producer = nodes_[i].reads[n];
        if (!reachable_[producer] || nodes_[producer].kind == ResultKind::none)
          return false;
      }
    }
    return true;
  }

  auto compute() -> bool
  {
    std::vector<Word> live(size_ * words_);
    bool changed = true;
    while (changed) {
      changed = false;
      for (Index i = size_; i-- != 0;) {
        if (!reachable_[i])
          continue;
        if (!spend(words_))
          return false;
        for (std::size_t w = 0; w < words_; ++w) {
          // IN = USE union (OUT minus DEF), OUT = union of successor INs.
          auto value = out(live, i, w);
          if (nodes_[i].kind != ResultKind::none && i / word_bits == w)
            value &= ~bit(i);
          value = with_reads(value, i, w);
          auto& previous = live[i * words_ + w];
          if (previous != value) {
            previous = value;
            changed = true;
          }
        }
      }
    }

    interference_.resize(size_ * words_);
    for (Index i = 0; i < size_; ++i) {
      if (!reachable_[i] || nodes_[i].kind == ResultKind::none)
        continue;
      if (!spend(words_))
        return false;
      for (std::size_t w = 0; w < words_; ++w) {
        // Even dead inputs remain borrowed until this instruction finishes.
        // Do not coalesce a new destination with either of its operands.
        auto neighbors = with_reads(out(live, i, w), i, w);
        if (i / word_bits == w)
          neighbors &= ~bit(i);
        while (neighbors != 0) {
          if (!spend(1))
            return false;
          auto const other = w * word_bits + static_cast<std::size_t>(std::countr_zero(neighbors));
          interference_[i * words_ + other / word_bits] |= bit(other);
          interference_[other * words_ + i / word_bits] |= bit(i);
          neighbors &= neighbors - 1;
        }
      }
    }
    return true;
  }

  auto color(std::vector<Color>& colors, std::size_t& count, Domain domain = Domain::all) -> bool
  {
    std::vector<Index> seen(size_ + 1);
    Color maximum = 0;
    for (Index i = 0; i < size_; ++i) {
      if (!reachable_[i] || !matches(i, domain))
        continue;
      auto const generation = i + 1;
      if (!spend(words_))
        return false;
      for (std::size_t w = 0; w < words_; ++w) {
        auto neighbors = interference_[i * words_ + w];
        while (neighbors != 0) {
          if (!spend(1))
            return false;
          auto const other = w * word_bits + static_cast<std::size_t>(std::countr_zero(neighbors));
          if (matches(other, domain) && colors[other] != 0)
            seen[colors[other]] = generation;
          neighbors &= neighbors - 1;
        }
      }
      Color selected = 1;
      while (selected <= maximum && seen[selected] == generation) {
        if (!spend(1))
          return false;
        ++selected;
      }
      if (selected > maximum)
        maximum = selected;
      colors[i] = selected;
    }
    count = static_cast<std::size_t>(maximum) + 1;  // Zero stays the empty sentinel.
    return true;
  }

  auto typed_plan(std::vector<Color> const& colors, std::size_t numbers, std::size_t others) const
    -> TypedResultPlan
  {
    // The lowerer validates all region producers, even after an unconditional
    // return. Unreachable producers need valid placeholders; skipped bodies
    // remain outside the region and retain no_register.
    for (auto const& node : nodes_) {
      if (!node.in_region)
        continue;
      if (node.kind == ResultKind::number && numbers == 0)
        numbers = 1;
      else if (node.kind != ResultKind::none && node.kind != ResultKind::number && others == 0)
        others = 1;
    }
    if (numbers == 0)
      return {};
    TypedResultPlan result;
    result.number_count = numbers;
    result.other_count = others;
    result.indices.resize(size_, TypedResultPlan::no_register);
    for (Index i = 0; i < size_; ++i) {
      if (nodes_[i].in_region && nodes_[i].kind != ResultKind::none)
        result.indices[i] = reachable_[i] ? colors[i] - 1 : 0;
    }
    return result;
  }

private:
  auto matches(Index i, Domain domain) const -> bool
  {
    auto const kind = nodes_[i].kind;
    if (kind == ResultKind::none)
      return false;
    if (domain == Domain::number)
      return kind == ResultKind::number;
    return domain == Domain::all || kind != ResultKind::number;
  }
  auto spend(std::size_t amount) -> bool
  {
    if (amount > remaining_)
      return false;
    remaining_ -= amount;
    return true;
  }

  auto out(std::vector<Word> const& live, Index i, std::size_t word) const -> Word
  {
    Word result = 0;
    for (std::size_t n = 0; n < nodes_[i].next_count; ++n)
      result |= live[nodes_[i].next[n] * words_ + word];
    return result;
  }

  auto with_reads(Word value, Index i, std::size_t word) const -> Word
  {
    for (std::size_t n = 0; n < nodes_[i].read_count; ++n) {
      auto const producer = nodes_[i].reads[n];
      if (producer / word_bits == word)
        value |= bit(producer);
    }
    return value;
  }

  std::size_t size_;
  std::size_t words_;
  std::size_t remaining_;
  std::vector<Node> nodes_;
  std::vector<bool> reachable_;
  std::vector<Word> interference_;
};

// Match the existing typed gate's retained headers, slots, maps and executable
// records. Check every product and sum; overflow cannot justify a candidate.
auto payload_bytes(std::size_t fixed,
                   std::initializer_list<std::pair<std::size_t, std::size_t>> parts)
  -> std::optional<std::size_t>
{
  auto total = fixed;
  auto const limit = std::numeric_limits<std::size_t>::max();
  for (auto const& [count, bytes] : parts) {
    if (bytes != 0 && count > limit / bytes)
      return std::nullopt;
    auto const payload = count * bytes;
    if (payload > limit - total)
      return std::nullopt;
    total += payload;
  }
  return total;
}

auto generic_bytes(std::size_t size, ResultSlotPlan const& plan, TypedResultCosts costs)
  -> std::optional<std::size_t>
{
  return payload_bytes(costs.generic_fixed_bytes,
                       {{plan.storage_size(size), costs.generic_slot_bytes},
                        {plan.indices.size(), sizeof(Color)},
                        {size, costs.generic_instruction_bytes}});
}

auto typed_bytes(std::size_t size, TypedResultPlan const& plan, TypedResultCosts costs)
  -> std::optional<std::size_t>
{
  if (plan.empty())
    return std::nullopt;
  return payload_bytes(costs.typed_fixed_bytes, {{plan.number_count, costs.number_slot_bytes},
                                                 {plan.other_count, costs.other_slot_bytes},
                                                 {size, sizeof(Color)},
                                                 {size, costs.typed_instruction_bytes}});
}
}  // namespace

auto plan_live_result_slots(Instructions const& code, ResultSlotPlan fallback,
                            std::size_t slot_bytes, bool persistent, LivenessBudget budget)
  -> ResultSlotPlan
{
  auto const size = code.size();
  auto const limit = std::numeric_limits<std::size_t>::max();
  if (size == 0 || (!persistent && size < 16) || size > LivenessBudget::instruction_limit ||
      size > budget.max_instructions || budget.max_word_operations == 0 ||
      slot_bytes <= sizeof(Color) || size > limit / slot_bytes)
    return fallback;
  auto const words = (size + word_bits - 1) / word_bits;
  if (size > limit / words || size > limit / sizeof(Color))
    return fallback;
  auto const old_count = fallback.storage_size(size);
  auto const old_map = fallback.indices.size();
  if (old_count > limit / slot_bytes || old_map > limit / sizeof(Color) ||
      old_count * slot_bytes > limit - old_map * sizeof(Color))
    return fallback;
  try {
    Analysis analysis(size, words,
                      std::min(budget.max_word_operations, LivenessBudget::word_operation_limit));
    if (!analysis.build(code) || !analysis.compute())
      return fallback;
    std::vector<Color> colors(size);
    std::size_t count = 0;
    if (!analysis.color(colors, count) || count >= old_count || count > limit / slot_bytes ||
        count * slot_bytes > limit - size * sizeof(Color) ||
        count * slot_bytes + size * sizeof(Color) >=
          old_count * slot_bytes + old_map * sizeof(Color))
      return fallback;
    ResultSlotPlan result;
    result.indices.assign(colors.begin(), colors.end());
    result.count = count;
    return result;
  }
  catch (std::bad_alloc const&) {
    // The caller's existing plan is still complete and untouched. Analysis is
    // optional and runs before execution; failure need not disable that path.
    return fallback;
  }
}

auto plan_live_result_layouts(Instructions const& code, LiveResultPlans fallback,
                              TypedResultCosts costs, bool persistent, LivenessBudget budget)
  -> LiveResultPlans
{
  static_assert(std::is_nothrow_move_constructible_v<LiveResultPlans>);
  static_assert(std::is_nothrow_default_constructible_v<TypedResultPlan>);
  static_assert(std::is_nothrow_move_assignable_v<TypedResultPlan>);
  auto const size = code.size();
  auto const limit = std::numeric_limits<std::size_t>::max();
  if (size == 0 || (!persistent && size < 16) || size > LivenessBudget::instruction_limit ||
      size > budget.max_instructions || budget.max_word_operations == 0 ||
      costs.generic_slot_bytes <= sizeof(Color) || costs.number_slot_bytes == 0 ||
      costs.other_slot_bytes == 0 || costs.generic_instruction_bytes == 0 ||
      costs.typed_instruction_bytes == 0 ||
      (!fallback.generic.indices.empty() && fallback.generic.indices.size() != size) ||
      (!fallback.typed.empty() && fallback.typed.indices.size() != size))
    return fallback;
  auto const words = (size + word_bits - 1) / word_bits;
  if (size > limit / words || !generic_bytes(size, fallback.generic, costs) ||
      (!fallback.typed.empty() && !typed_bytes(size, fallback.typed, costs)))
    return fallback;
  try {
    Analysis analysis(size, words,
                      std::min(budget.max_word_operations, LivenessBudget::word_operation_limit));
    if (!analysis.build(code) || !analysis.compute())
      return fallback;
    std::vector<Color> generic_colors(size);
    std::vector<Color> typed_colors(size);
    std::size_t generic_count = 0;
    std::size_t number_palette = 0;
    std::size_t other_palette = 0;
    if (!analysis.color(generic_colors, generic_count) ||
        !analysis.color(typed_colors, number_palette, Domain::number) ||
        !analysis.color(typed_colors, other_palette, Domain::other))
      return fallback;

    auto const proposed_generic_bytes =
      payload_bytes(costs.generic_fixed_bytes, {{generic_count, costs.generic_slot_bytes},
                                                {size, sizeof(Color)},
                                                {size, costs.generic_instruction_bytes}});
    if (!proposed_generic_bytes)
      return fallback;
    bool const improve_generic =
      *proposed_generic_bytes < *generic_bytes(size, fallback.generic, costs);
    ResultSlotPlan proposed_generic;
    if (improve_generic) {
      proposed_generic.indices.assign(generic_colors.begin(), generic_colors.end());
      proposed_generic.count = generic_count;
    }
    auto const& best_generic = improve_generic ? proposed_generic : fallback.generic;
    auto const best_generic_bytes = *generic_bytes(size, best_generic, costs);
    auto proposed_typed = analysis.typed_plan(typed_colors, number_palette - 1, other_palette - 1);
    auto const proposed_typed_bytes = typed_bytes(size, proposed_typed, costs);
    if (!proposed_typed.empty() && !proposed_typed_bytes)
      return fallback;
    auto const original_typed_bytes = typed_bytes(size, fallback.typed, costs);
    bool const keep_original_typed =
      original_typed_bytes && *original_typed_bytes < best_generic_bytes;
    bool const use_proposed_typed =
      proposed_typed_bytes && *proposed_typed_bytes < best_generic_bytes &&
      (!keep_original_typed || *proposed_typed_bytes < *original_typed_bytes);

    // Finish every analysis/allocation/cost check before moving an original
    // plan. These publication moves cannot throw or expose half a pair.
    LiveResultPlans result{
      improve_generic ? std::move(proposed_generic) : std::move(fallback.generic), {}};
    if (use_proposed_typed)
      result.typed = std::move(proposed_typed);
    else if (keep_original_typed)
      result.typed = std::move(fallback.typed);
    return result;
  }
  catch (std::bad_alloc const&) {
    return fallback;
  }
}
}  // namespace GD::Bc::Details
