/** \file verification.cc
 * Validate function regions and dominance with linear auxiliary storage.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "verification.hh"

#if !defined(_WIN32)
#  include "verification-linear.hh"
#endif

#include <algorithm>
#include <cstddef>
#if defined(GD_BC_ARENA_CONTROL)
#  include <cstdlib>
#endif
#include <memory_resource>
#include <numeric>
#include <span>
#include <utility>
#include <vector>

namespace GD::Bc::Details {
namespace {
using Index = Instruction::Index;
using Edge = std::pair<Index, Index>;

// Preserve the insertion order within each node's neighbors. Flat storage
// avoids allocating separate vectors for every edge in a small input item.
class Adjacency
{
public:
  Adjacency(Index nodes, std::span<Edge const> edges, std::pmr::memory_resource* storage,
            bool reverse = false)
      : bounds_(nodes + 1, storage), neighbors_(edges.size(), storage)
  {
    for (auto const& [from, to] : edges) {
      ++bounds_[(reverse ? to : from) + 1];
    }
    std::partial_sum(bounds_.begin(), bounds_.end(), bounds_.begin());
    std::pmr::vector<Index> next(bounds_, storage);
    for (auto const& [from, to] : edges) {
      neighbors_[next[reverse ? to : from]++] = reverse ? from : to;
    }
  }

  [[nodiscard]] auto operator[](Index node) const -> std::span<Index const>
  {
    return std::span<Index const>(neighbors_)
      .subspan(bounds_[node], bounds_[node + 1] - bounds_[node]);
  }

private:
  std::pmr::vector<Index> bounds_;
  std::pmr::vector<Index> neighbors_;
};
}  // namespace

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
  I const size = code.size();
  I const root = size + 1;
  I const absent = size + 2;
  auto target = [](I index, Instruction::Operand const& operand) {
    return index + static_cast<I>(std::get<Instruction::Offset>(operand));
  };
  // Graph scratch dies together at the end of verification. Keep small
  // graphs off the heap and let larger graphs use the upstream resource.
  alignas(std::max_align_t) std::byte scratch[4096];
  std::pmr::monotonic_buffer_resource arena(scratch, sizeof scratch);
  // Use ordinary allocations for larger graphs so temporary adjacency
  // cursors and old vector capacity can be released as soon as they die.
  bool const small = size <= 24;
#if defined(GD_BC_ARENA_CONTROL)
  static bool const use_arena = std::getenv("GD_BC_GRAPH_HEAP_CONTROL") == nullptr;
  auto* storage = small && use_arena ? &arena : std::pmr::new_delete_resource();
#else
  auto* storage = small ? &arena : std::pmr::new_delete_resource();
#endif
  std::pmr::vector<I> owner(size, 0, storage);
  std::pmr::vector<I> function_end(size, absent, storage);
  std::pmr::vector<I> entries(storage);
  entries.push_back(0);
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
  std::pmr::vector<Edge> edges(storage);
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

  Adjacency const successors(size + 2, edges, storage);
  Adjacency const predecessors(size + 2, edges, storage, true);

  // Each execution region starts with no owned argument packs. A function
  // additionally receives one caller-owned pack, consumed by its prologue.
  // Exact states at joins prevent underflow or accumulation around loops.
  struct MarkerState
  {
    I owned;
    bool incoming_consumed;
  };
  std::pmr::vector<std::optional<MarkerState>> states(size + 2, storage);
  std::pmr::vector<I> pending(storage);
  if (small) {
    pending.reserve(size + 2);
  }
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
  std::pmr::vector<I> indegree(size + 2, 0, storage);
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
  std::pmr::vector<I> acyclic(storage);
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

  // Iterative DFS avoids making host stack depth depend on source length.
  std::pmr::vector<bool> visited(size + 2, false, storage);
  std::pmr::vector<std::pair<I, I>> stack(storage);
  std::pmr::vector<I> postorder(storage);
  if (small) {
    stack.reserve(size + 2);
    postorder.reserve(size + 2);
  }
  stack.emplace_back(root, 0);
  visited[root] = true;
  while (!stack.empty()) {
    auto& frame = stack.back();
    if (frame.second == successors[frame.first].size()) {
      postorder.push_back(frame.first);
      stack.pop_back();
    }
    else {
      I const next = successors[frame.first][frame.second++];
      if (!visited[next]) {
        visited[next] = true;
        stack.emplace_back(next, 0);
      }
    }
  }
  std::reverse(postorder.begin(), postorder.end());
  std::pmr::vector<I> rank(size + 2, absent, storage);
  for (I i = 0; i < postorder.size(); ++i) {
    rank[postorder[i]] = i;
  }
  std::pmr::vector<I> dominator(size + 2, absent, storage);
  dominator[root] = root;
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
      if (node == root) {
        continue;
      }
      I parent = absent;
      for (I predecessor : predecessors[node]) {
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
    if (node != root) {
      edge(dominator[node], node);
    }
  }
  Adjacency const children(size + 2, edges, storage);
  std::pmr::vector<I> enter(size + 2, 0, storage);
  std::pmr::vector<I> leave(size + 2, 0, storage);
  I clock = 0;
  stack.emplace_back(root, 0);
  enter[root] = clock++;
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
      return !visited[i] || (producer != i && visited[producer] && enter[producer] <= enter[i] &&
                             enter[i] < leave[producer]);
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
