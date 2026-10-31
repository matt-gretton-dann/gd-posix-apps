/** \file test-verification.cc
 * Replayable control-flow checks against an independent path-removal oracle.
 * SPDX-License-Identifier: Apache-2.0
 */

#include <catch2/catch.hpp>

#include <array>
#include <random>
#include <vector>

#include "verification.hh"

TEST_CASE("Verifier dominance agrees with path removal", "[bc][vm][verifier][property]")
{
  using I = GD::Bc::Instruction;
  using O = I::Opcode;
  using D = I::Offset;
  constexpr unsigned count = 8;
  constexpr unsigned absent = count + 1;
  std::mt19937 rng(197187);
  for (unsigned sample = 0; sample < 20; ++sample) {
    std::array<std::array<unsigned, 2>, count> edges{};
    for (auto& destinations : edges) {
      destinations = {
        {static_cast<unsigned>(rng() % (count + 1)), static_cast<unsigned>(rng() % (count + 1))}};
    }
    auto reachable = [&](unsigned destination, unsigned removed) {
      std::array<bool, count + 1> seen{};
      std::vector<unsigned> pending{0};
      while (!pending.empty()) {
        auto const node = pending.back();
        pending.pop_back();
        if (node == removed || seen[node]) {
          continue;
        }
        seen[node] = true;
        if (node < count) {
          for (auto next : edges[node]) {
            pending.push_back(next);
          }
        }
      }
      return seen[destination];
    };
    for (unsigned producer = 0; producer < count; ++producer) {
      for (unsigned consumer = 0; consumer < count; ++consumer) {
        INFO("seed = 197187, sample = " << sample << ", producer = " << producer
                                        << ", consumer = " << consumer);
        GD::Bc::Instructions code;
        for (unsigned block = 0; block < count; ++block) {
          code.emplace_back(O::number, "0");
          if (block == consumer) {
            code.emplace_back(O::print, D(4 * producer) - D(4 * block + 1), I::Stream::output);
          }
          else {
            code.emplace_back(O::number, "0");
          }
          code.emplace_back(O::branch_zero, D(-2), D(4 * edges[block][0]) - D(4 * block + 2));
          code.emplace_back(O::branch, D(4 * edges[block][1]) - D(4 * block + 3));
        }
        code.emplace_back(O::eof);
        // Removing a producer disconnects every path to a dominated consumer.
        // This oracle uses graph reachability, not the verifier's dominator tree.
        bool const valid = !reachable(consumer, absent) || !reachable(consumer, producer);
        auto const failure = GD::Bc::Details::verify_control_flow(code, false);
        CHECK(failure.has_value() == !valid);
      }
    }
  }
}
