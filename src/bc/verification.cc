/** \file verification.cc
 * Validate function regions and dominance with linear auxiliary storage.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "verification.hh"

#if !defined(_WIN32)
#  include "verification-linear.hh"
#endif

#include <algorithm>
#include <array>
#include <numeric>
#include <span>
#include <utility>
#include <vector>

namespace GD::Bc::Details {
namespace {
using Index = Instruction::Index;
using Edge = std::pair<Index, Index>;

// An isolated definition has no outer executable instructions. With no
// branches and only backward body-local operands, every reachable producer
// dominates its consumer. Reject unsupported shapes to the graph verifier.
auto verify_linear_definition(Instructions const& code) -> bool
{
  using O = Instruction::Opcode;
  auto const end = code.size() - 1;
  if (end < 2 || code[end].opcode() != O::function_end ||
      end + static_cast<Index>(std::get<Instruction::Offset>(code[end].op2())) != 0) {
    return false;
  }

  bool incoming_consumed = false;
  for (Index i = 1; i < end; ++i) {
    auto const& instruction = code[i];
    switch (instruction.opcode()) {
    case O::eof:
    case O::branch:
    case O::branch_zero:
    case O::function_begin:
    case O::function_end:
    case O::quit:
    case O::push_param_mark:
    case O::push_param:
    case O::call:
      return false;
    case O::return_:
      if (!incoming_consumed) {
        return false;
      }
      break;
    case O::pop_param:
    case O::pop_param_array:
      if (incoming_consumed) {
        return false;
      }
      break;
    case O::pop_param_mark:
      if (incoming_consumed) {
        return false;
      }
      incoming_consumed = true;
      break;
    default:
      break;
    }
    auto previous = [i](Instruction::Operand const& operand) {
      auto const* offset = std::get_if<Instruction::Offset>(&operand);
      return offset == nullptr || (*offset < 0 && i + static_cast<Index>(*offset) > 0);
    };
    if ((instruction.has_op1() && !previous(instruction.op1())) ||
        (instruction.has_op2() && !previous(instruction.op2()))) {
      return false;
    }
  }
  return incoming_consumed;
}

// Preserve the insertion order within each node's neighbors. Flat storage
// avoids allocating separate vectors for every edge in a small input item.
class Adjacency
{
public:
  Adjacency(Index nodes, std::span<Edge const> edges, bool reverse = false)
      : bounds_(nodes + 1), neighbors_(edges.size())
  {
    for (auto const& [from, to] : edges) {
      ++bounds_[(reverse ? to : from) + 1];
    }
    std::partial_sum(bounds_.begin(), bounds_.end(), bounds_.begin());
    auto next = bounds_;
    for (auto const& [from, to] : edges) {
      neighbors_[next[reverse ? to : from]++] = reverse ? from : to;
    }
  }

  [[nodiscard]] auto operator[](Index node) const -> std::span<Index const>
  {
    return std::span<Index const>(neighbors_).subspan(bounds_[node],
                                                      bounds_[node + 1] - bounds_[node]);
  }

private:
  std::vector<Index> bounds_;
  std::vector<Index> neighbors_;
};
}  // namespace

// Prove the common short scalar case in one pass. Every referenced
// instruction has already executed in this branch-free stream. A rejected
// shape goes through the general verifier for its full checks and diagnostics.
auto verify_short_scalar_stream(Instructions const& code) -> bool
{
  using O = Instruction::Opcode;
  enum class Kind { none, number, location, string };
  std::array<Kind, 16> results;
  if (code.size() > results.size())
    return false;

  for (Index i = 0; i < code.size(); ++i) {
    auto const& instruction = code[i];
    auto input = [&](Instruction::Operand const& operand) {
      auto const* offset = std::get_if<Instruction::Offset>(&operand);
      if (offset == nullptr || *offset >= 0 || static_cast<Index>(-(*offset + 1)) >= i)
        return Kind::none;
      return results[i + static_cast<Index>(*offset)];
    };
    auto const opcode = instruction.opcode();
    switch (opcode) {
    case O::eof:
      return i + 1 == code.size();
    case O::number:
    case O::string:
      if (!std::holds_alternative<std::string>(instruction.op1()))
        return false;
      results[i] = opcode == O::number ? Kind::number : Kind::string;
      break;
    case O::variable:
      if (!std::holds_alternative<Variable>(instruction.op1()))
        return false;
      [[fallthrough]];
    case O::scale:
    case O::ibase:
    case O::obase:
      results[i] = Kind::location;
      break;
    case O::load:
      if (input(instruction.op1()) != Kind::location)
        return false;
      results[i] = Kind::number;
      break;
    case O::store:
      if (input(instruction.op1()) != Kind::location || input(instruction.op2()) != Kind::number)
        return false;
      results[i] = Kind::none;
      break;
    case O::print:
      if (input(instruction.op1()) == Kind::none ||
          !std::holds_alternative<Instruction::Stream>(instruction.op2()))
        return false;
      results[i] = Kind::none;
      break;
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
      if (input(instruction.op2()) != Kind::number)
        return false;
      [[fallthrough]];
    case O::negate:
    case O::sqrt:
    case O::abs:
    case O::scale_expr:
    case O::length:
      if (input(instruction.op1()) != Kind::number)
        return false;
      results[i] = Kind::number;
      break;
    default:
      return false;
    }
  }
  return true;
}

auto verify_control_flow(Instructions const& code, bool function_body)
  -> std::optional<VerificationError>
{
  using I = Instruction::Index;
  using O = Instruction::Opcode;
#if defined(_WIN32)
  if (!function_body && is_verified_linear_stream(code)) {
#else
  if (!function_body && verify_linear_stream(code)) {
#endif
    return std::nullopt;
  }
  if (!function_body && !code.empty() && code.front().opcode() == O::function_begin &&
      verify_linear_definition(code)) {
    return std::nullopt;
  }
  I const size = code.size();
  I const root = size + 1;
  I const absent = size + 2;
  auto target = [](I index, Instruction::Operand const& operand) {
    return index + static_cast<I>(std::get<Instruction::Offset>(operand));
  };
  std::vector<I> owner(size, 0);
  std::vector<I> function_end(size, absent);
  std::vector<I> entries{0};
  I active = absent;
  for (I i = 0; i < size; ++i) {
    if (code[i].opcode() == O::function_begin) {
      if (active != absent || function_body) {
        return VerificationError{i, "nested function definition"};
      }
      active = i;
      entries.push_back(i + 1);
    }
    else if (code[i].opcode() == O::function_end) {
      if (active == absent) {
        return VerificationError{i, "orphan function end"};
      }
      if (target(i, code[i].op2()) != active) {
        return VerificationError{i, "function end does not match its beginning"};
      }
      owner[i] = active + 1;
      function_end[active] = i;
      active = absent;
    }
    else if (active != absent) {
      owner[i] = active + 1;
    }
    if (code[i].opcode() == O::eof && (i + 1 != size || owner[i] != 0 || function_body)) {
      return VerificationError{i, "EOF is only valid at the end of the outer stream"};
    }
  }
  if (active != absent) {
    return VerificationError{active, "missing function end"};
  }

  // The outer stream and each function body have separate roots. Function-end
  // nodes represent body fall-through, never execution of that metadata opcode.
  // A synthetic root lets a single dominance calculation serve all regions.
  std::vector<Edge> edges;
  edges.reserve(size + entries.size());
  auto edge = [&](I from, I to) { edges.emplace_back(from, to); };
  for (auto entry : entries) {
    edge(root, entry);
  }
  for (I i = 0; i < size; ++i) {
    auto branch = [&](Instruction::Operand const& operand) -> bool {
      I const destination = target(i, operand);
      if (destination == size ? owner[i] != 0 : owner[destination] != owner[i]) {
        return false;
      }
      edge(i, destination);
      return true;
    };
    switch (code[i].opcode()) {
    case O::branch:
      if (!branch(code[i].op1())) {
        return VerificationError{i, "branch crosses a function boundary"};
      }
      break;
    case O::branch_zero:
      if (!branch(code[i].op2())) {
        return VerificationError{i, "conditional branch crosses a function boundary"};
      }
      edge(i, i + 1);
      break;
    case O::function_begin:
      edge(i, function_end[i] + 1);
      break;
    case O::function_end:
    case O::return_:
    case O::quit:
    case O::eof:
      break;
    default:
      edge(i, i + 1);
      break;
    }
  }

  Adjacency const successors(size + 2, edges);

  // Each execution region starts with no owned argument packs. A function
  // additionally receives one caller-owned pack, consumed by its prologue.
  // Exact states at joins prevent underflow or accumulation around loops.
  struct MarkerState
  {
    I owned;
    bool incoming_consumed;
  };
  std::vector<std::optional<MarkerState>> states(size + 2);
  std::vector<I> pending;
  for (I entry : entries) {
    bool const incoming = function_body || (entry < size && owner[entry] != 0);
    states[entry] = MarkerState{0, !incoming};
    pending.push_back(entry);
  }
  for (I cursor = 0; cursor < pending.size(); ++cursor) {
    I const node = pending[cursor];
    auto state = *states[node];
    if (node < size) {
      switch (code[node].opcode()) {
      case O::push_param_mark:
        ++state.owned;
        break;
      case O::push_param:
        if (state.owned == 0) {
          return VerificationError{node, "parameter push has no argument-pack marker"};
        }
        break;
      case O::call:
        if (state.owned == 0) {
          return VerificationError{node, "call has no argument-pack marker"};
        }
        --state.owned;
        break;
      case O::pop_param:
      case O::pop_param_array:
        if (state.incoming_consumed || state.owned != 0) {
          return VerificationError{node, "parameter extraction is outside the incoming prologue"};
        }
        break;
      case O::pop_param_mark:
        if (state.incoming_consumed || state.owned != 0) {
          return VerificationError{node, "incoming parameter marker is absent or already consumed"};
        }
        state.incoming_consumed = true;
        break;
      default:
        break;
      }
    }
    if (successors[node].empty() && (state.owned != 0 || !state.incoming_consumed)) {
      return VerificationError{node, "execution exits with an unconsumed parameter marker"};
    }
    for (I destination : successors[node]) {
      if (!states[destination]) {
        states[destination] = state;
        pending.push_back(destination);
      }
      else if (states[destination]->owned != state.owned ||
               states[destination]->incoming_consumed != state.incoming_consumed) {
        return VerificationError{destination,
                                 "parameter-marker states disagree at a control-flow join"};
      }
    }
  }

  // A cycle before the incoming pack is consumed can bypass the prologue
  // forever. Remove acyclic nodes from that subgraph; any remainder is a cycle.
  // Ordinary function loops after prologue consumption remain valid.
  std::vector<I> indegree(size + 2, 0);
  auto unconsumed = [&](I node) { return states[node] && !states[node]->incoming_consumed; };
  for (I node : pending) {
    if (unconsumed(node)) {
      for (I destination : successors[node]) {
        if (unconsumed(destination)) {
          ++indegree[destination];
        }
      }
    }
  }
  std::vector<I> acyclic;
  for (I node : pending) {
    if (unconsumed(node) && indegree[node] == 0) {
      acyclic.push_back(node);
    }
  }
  for (I cursor = 0; cursor < acyclic.size(); ++cursor) {
    for (I destination : successors[acyclic[cursor]]) {
      if (unconsumed(destination) && --indegree[destination] == 0) {
        acyclic.push_back(destination);
      }
    }
  }
  for (I node : pending) {
    if (indegree[node] != 0) {
      return VerificationError{node, "function can loop before consuming its incoming parameters"};
    }
  }

  // Dominance needs only the entries and exits of straight-line runs.
  // Keep marker-state propagation above on instruction nodes so errors
  // retain their existing locations and order.
  std::vector<I> block_of(size + 2, 0);
  for (auto const& [from, to] : edges) {
    if (to != from + 1)
      block_of[to] = absent;
  }
  I blocks = 0;
  for (I node = 0; node < size + 2; ++node) {
    bool join_previous = false;
    if (node != 0 && node < size && block_of[node] != absent) {
      auto const preceding = successors[node - 1];
      join_previous = preceding.size() == 1 && preceding.front() == node;
    }
    if (!join_previous)
      ++blocks;
    block_of[node] = blocks - 1;
  }

  edges.clear();
  for (I node = 0; node < size + 2; ++node) {
    // Only a coalesced immediate edge is implicit. Preserve backedges
    // within a block, including an explicit branch to its own entry.
    if (node < root && block_of[node] == block_of[node + 1])
      continue;
    for (I next : successors[node])
      edge(block_of[node], block_of[next]);
  }
  I const block_root = block_of[root];
  Adjacency const block_successors(blocks, edges);
  Adjacency const block_predecessors(blocks, edges, true);

  // Iterative DFS avoids making host stack depth depend on source length.
  std::vector<bool> visited(blocks, false);
  std::vector<std::pair<I, I>> stack{{block_root, 0}};
  std::vector<I> postorder;
  visited[block_root] = true;
  while (!stack.empty()) {
    auto& frame = stack.back();
    if (frame.second == block_successors[frame.first].size()) {
      postorder.push_back(frame.first);
      stack.pop_back();
    }
    else {
      I const next = block_successors[frame.first][frame.second++];
      if (!visited[next]) {
        visited[next] = true;
        stack.emplace_back(next, 0);
      }
    }
  }
  std::reverse(postorder.begin(), postorder.end());
  std::vector<I> rank(blocks, absent);
  for (I i = 0; i < postorder.size(); ++i) {
    rank[postorder[i]] = i;
  }
  std::vector<I> dominator(blocks, absent);
  dominator[block_root] = block_root;
  auto intersect = [&](I left, I right) {
    while (left != right) {
      if (rank[left] > rank[right]) {
        left = dominator[left];
      }
      else {
        right = dominator[right];
      }
    }
    return left;
  };
  bool changed = false;
  do {
    changed = false;
    for (I node : postorder) {
      if (node == block_root) {
        continue;
      }
      I parent = absent;
      for (I predecessor : block_predecessors[node]) {
        if (dominator[predecessor] != absent) {
          parent = parent == absent ? predecessor : intersect(parent, predecessor);
        }
      }
      if (dominator[node] != parent) {
        dominator[node] = parent;
        changed = true;
      }
    }
  } while (changed);

  // Domination queries use tree intervals, avoiding a repeated walk through a
  // long expression's ancestors for every operand. No quadratic bitset matrix
  // or recursive traversal is needed.
  edges.clear();
  for (I node : postorder) {
    if (node != block_root) {
      edge(dominator[node], node);
    }
  }
  Adjacency const children(blocks, edges);
  std::vector<I> enter(blocks, 0);
  std::vector<I> leave(blocks, 0);
  I clock = 0;
  stack.emplace_back(block_root, 0);
  enter[block_root] = clock++;
  while (!stack.empty()) {
    auto& frame = stack.back();
    if (frame.second == children[frame.first].size()) {
      leave[frame.first] = clock;
      stack.pop_back();
    }
    else {
      I const child = children[frame.first][frame.second++];
      enter[child] = clock++;
      stack.emplace_back(child, 0);
    }
  }

  for (I i = 0; i < size; ++i) {
    auto check = [&](Instruction::Operand const& operand) -> bool {
      if (!std::holds_alternative<Instruction::Offset>(operand)) {
        return true;
      }
      I const producer = target(i, operand);
      if (owner[producer] != owner[i]) {
        return false;
      }
      // Unreachable consumers cannot execute a read. Reachable ones require a
      // strictly dominating producer; previous loop iterations are not assumed.
      auto const consumer_block = block_of[i];
      auto const producer_block = block_of[producer];
      if (!visited[consumer_block])
        return true;
      if (producer_block == consumer_block)
        return producer < i;
      return visited[producer_block] && enter[producer_block] <= enter[consumer_block] &&
             enter[consumer_block] < leave[producer_block];
    };
    if (code[i].opcode() == O::branch || code[i].opcode() == O::function_end) {
      continue;
    }
    if (code[i].has_op1() && !check(code[i].op1())) {
      return VerificationError{i, "operand 1 producer is unavailable on an incoming path"};
    }
    if (code[i].has_op2() && code[i].opcode() != O::branch_zero && !check(code[i].op2())) {
      return VerificationError{i, "operand 2 producer is unavailable on an incoming path"};
    }
  }
  return std::nullopt;
}
}  // namespace GD::Bc::Details
