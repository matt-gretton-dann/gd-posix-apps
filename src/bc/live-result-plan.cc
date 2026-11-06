/** Bounded liveness and conservative interference for verified result values.
 * SPDX-License-Identifier: Apache-2.0
 */
#include "live-result-plan.hh"

#include <new>

#include <algorithm>
#include <array>
#include <bit>
#include <cstdint>
#include <limits>
#include <variant>
#include <vector>

namespace GD::Bc::Details {
namespace {
using Index = Instruction::Index;
using Opcode = Instruction::Opcode;
using Word = std::uint64_t;
using Color = ResultSlotPlan::CompactIndex;
constexpr std::size_t word_bits = 64;

auto bit(Index index) -> Word { return Word(1) << (index % word_bits); }

struct Node
{
  std::array<Index, 2> next{};
  std::array<Index, 2> reads{};
  std::size_t next_count = 0;
  std::size_t read_count = 0;
  ResultKind kind = ResultKind::none;
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
        definition = size_;
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

  auto color(std::vector<Color>& colors, std::size_t& count) -> bool
  {
    std::vector<Index> seen(size_ + 1);
    Color maximum = 0;
    for (Index i = 0; i < size_; ++i) {
      if (!reachable_[i] || nodes_[i].kind == ResultKind::none)
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
          if (colors[other] != 0)
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

private:
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
}  // namespace GD::Bc::Details
