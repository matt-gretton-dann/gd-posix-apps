/** \file verification.cc
 * Validate function regions and dominance with linear auxiliary storage.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "verification.hh"

#include <algorithm>
#include <utility>
#include <vector>

namespace GD::Bc::Details {
auto verify_control_flow(Instructions const& code, bool function_body)
  -> std::optional<VerificationError>
{
  using I = Instruction::Index;
  using O = Instruction::Opcode;
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
  std::vector<std::vector<I>> successors(size + 2);
  std::vector<std::vector<I>> predecessors(size + 2);
  auto edge = [&](I from, I to) {
    successors[from].push_back(to);
    predecessors[to].push_back(from);
  };
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

  // Iterative DFS avoids making host stack depth depend on source length.
  std::vector<bool> visited(size + 2, false);
  std::vector<std::pair<I, I>> stack{{root, 0}};
  std::vector<I> postorder;
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
  std::vector<I> rank(size + 2, absent);
  for (I i = 0; i < postorder.size(); ++i) {
    rank[postorder[i]] = i;
  }
  std::vector<I> dominator(size + 2, absent);
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
  std::vector<std::vector<I>> children(size + 2);
  for (I node : postorder) {
    if (node != root) {
      children[dominator[node]].push_back(node);
    }
  }
  std::vector<I> enter(size + 2, 0);
  std::vector<I> leave(size + 2, 0);
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
