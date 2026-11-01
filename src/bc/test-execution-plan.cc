/** \file test-execution-plan.cc
 * Basic-block and repeated-use obligations for arithmetic ownership transfer.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <catch2/catch.hpp>

#include "execution-plan.hh"
#include "verification.hh"

namespace {
using I = GD::Bc::Instruction;
using O = I::Opcode;
using D = I::Offset;
auto plan(GD::Bc::Instructions const& code, bool persistent = true)
  -> GD::Bc::Details::ConsumptionPlan
{
  REQUIRE_FALSE(GD::Bc::Details::verify_control_flow(code, false).has_value());
  return GD::Bc::Details::plan_consumption(code, persistent);
}
}  // namespace

TEST_CASE("Consumption plans skip empty and tiny once-only streams", "[bc][vm][execution-plan]")
{
  GD::Bc::Instructions const code{I(O::number, "7"), I(O::number, "1"), I(O::add, D(-2), D(-1))};
  CHECK(GD::Bc::Details::plan_consumption({}, false).empty());
  CHECK(plan(code, false).empty());
  auto const persistent = plan(code);
  REQUIRE(persistent.size() == code.size());
  CHECK(persistent[2] == 1);
}

TEST_CASE("Repeated operands keep their producer available", "[bc][vm][execution-plan]")
{
  CHECK(plan({I(O::number, "7"), I(O::add, D(-1), D(-1))}).empty());
}

TEST_CASE("A backedge to the consumer prevents consuming an outer producer",
          "[bc][vm][execution-plan]")
{
  // 2 is reached again without re-executing 0 or 1. The sole static read
  // therefore cannot transfer 0, despite entry dominance.
  CHECK(plan({I(O::number, "7"), I(O::number, "1"), I(O::add, D(-2), D(-1)), I(O::branch, D(-1))})
          .empty());
}

TEST_CASE("A backedge that re-executes the producer permits consumption",
          "[bc][vm][execution-plan]")
{
  GD::Bc::Instructions const code{I(O::number, "7"), I(O::number, "1"), I(O::add, D(-2), D(-1)),
                                  I(O::branch, D(-3))};
  auto const flags = plan(code);
  REQUIRE(flags.size() == code.size());
  CHECK(flags[2] == 1);
  // The backward branch names a control target, not a second data use of 0.
}

TEST_CASE("Conditional entries divide consumption blocks", "[bc][vm][execution-plan]")
{
  CHECK(plan({I(O::number, "7"), I(O::number, "0"), I(O::branch_zero, D(-1), D(2)),
              I(O::number, "1"), I(O::number, "1"), I(O::add, D(-5), D(-1))})
          .empty());
}

TEST_CASE("A second later use prevents an arithmetic transfer", "[bc][vm][execution-plan]")
{
  GD::Bc::Instructions const code{I(O::number, "7"), I(O::number, "1"), I(O::add, D(-2), D(-1)),
                                  I(O::print, D(-3), I::Stream::output)};
  CHECK(plan(code).empty());
}
