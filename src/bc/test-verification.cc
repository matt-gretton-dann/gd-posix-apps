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

TEST_CASE("Verifier checks isolated function-body boundaries", "[bc][vm][verifier][pairs]")
{
  using I = GD::Bc::Instruction;
  using O = I::Opcode;
  using D = I::Offset;
  auto check = [](GD::Bc::Instructions const& code, std::string_view reason) {
    auto const failure = GD::Bc::Details::verify_control_flow(code, true);
    REQUIRE(failure.has_value());
    CHECK(failure->reason == reason);
  };
  check({I(O::function_begin, GD::Bc::VariableMask(), GD::Bc::Location("fixture", 1, 1))},
        "nested function definition");
  check({I(O::eof)}, "EOF is only valid at the end of the outer stream");
  check({I(O::push_param_mark), I(O::pop_param)},
        "parameter extraction is outside the incoming prologue");
  check({I(O::push_param_mark), I(O::pop_param_mark)},
        "incoming parameter marker is absent or already consumed");

  // An acyclic diamond before consuming the incoming pack gives the join
  // two predecessors. Its indegree decreases once without reaching zero.
  GD::Bc::Instructions const diamond{
    I(O::number, "0"), I(O::branch_zero, D(-1), D(3)), I(O::number, "1"), I(O::branch, D(2)),
    I(O::number, "2"), I(O::pop_param_mark),           I(O::number, "0"), I(O::return_, D(-1))};
  CHECK_FALSE(GD::Bc::Details::verify_control_flow(diamond, true).has_value());
}

TEST_CASE("Verifier independently rejects an unavailable second producer",
          "[bc][vm][verifier][pairs]")
{
  using I = GD::Bc::Instruction;
  using O = I::Opcode;
  using D = I::Offset;
  // The first operand dominates the addition; the second is bypassed by a
  // branch. Schemas, offset bounds and producer kinds satisfy prerequisites.
  GD::Bc::Instructions code{I(O::number, "1"), I(O::branch, D(2)), I(O::number, "2"),
                            I(O::add, D(-3), D(-1)), I(O::eof)};
  auto const failure = GD::Bc::Details::verify_control_flow(code, false);
  REQUIRE(failure.has_value());
  CHECK(failure->instruction == 3);
  CHECK(failure->reason == "operand 2 producer is unavailable on an incoming path");
  code[1] = I(O::branch, D(1));
  CHECK_FALSE(GD::Bc::Details::verify_control_flow(code, false).has_value());
}

TEST_CASE("Verifier rejects missing ends and conditional region escapes",
          "[bc][vm][verifier][pairs]")
{
  using I = GD::Bc::Instruction;
  using O = I::Opcode;
  using D = I::Offset;
  GD::Bc::Instructions const missing{
    I(O::function_begin, GD::Bc::VariableMask(), GD::Bc::Location("fixture", 1, 1))};
  auto failure = GD::Bc::Details::verify_control_flow(missing, false);
  REQUIRE(failure.has_value());
  CHECK(failure->instruction == 0);
  CHECK(failure->reason == "missing function end");

  GD::Bc::Instructions escape{
    I(O::function_begin, GD::Bc::VariableMask(), GD::Bc::Location("fixture", 1, 1)),
    I(O::pop_param_mark),
    I(O::number, "0"),
    I(O::branch_zero, D(-1), D(4)),
    I(O::number, "0"),
    I(O::return_, D(-1)),
    I(O::function_end, GD::Bc::Letter('f'), D(-6)),
    I(O::eof)};
  failure = GD::Bc::Details::verify_control_flow(escape, false);
  REQUIRE(failure.has_value());
  CHECK(failure->instruction == 3);
  CHECK(failure->reason == "conditional branch crosses a function boundary");
  escape[3] = I(O::branch_zero, D(-1), D(1));
  CHECK_FALSE(GD::Bc::Details::verify_control_flow(escape, false).has_value());
}
