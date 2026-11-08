/** Exact storage and eligibility checks for private scalar activations.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <catch2/catch.hpp>

#include <memory>
#include <vector>

#include "scalar-activation.hh"

namespace {
using namespace GD::Bc;
using namespace GD::Bc::Details;
using I = Instruction;
using O = I::Opcode;
}  // namespace

TEST_CASE("Private scalar proof rejects observable dynamic bindings", "[bc][private-scalars]")
{
  VariableMask locals("xt", "");
  Instructions body{I(O::variable, Variable('x')), I(O::load, I::Offset(-1)),
                    I(O::return_, I::Offset(-1))};
  CHECK(plan_private_scalars(body, locals, Letter('f')).eligible);
  auto rejects = [&](I extra) {
    auto candidate = body;
    candidate.push_back(std::move(extra));
    CHECK_FALSE(plan_private_scalars(candidate, locals, Letter('f')).eligible);
  };
  rejects(I(O::variable, Variable('g')));
  rejects(I(O::array, Array('x')));
  rejects(I(O::pop_param_array));
  rejects(I(O::print, I::Offset(-1), I::Stream::output));
  rejects(I(O::quit, 0U));
  rejects(I(O::call, Letter('g'), Location("test", 1, 1)));
  CHECK_FALSE(plan_private_scalars(body, VariableMask("xt", "a"), Letter('f')).eligible);
  CHECK_FALSE(plan_private_scalars({}, VariableMask{}, Letter('f')).eligible);
  body.emplace_back(O::call, Letter('f'), Location("test", 1, 1));
  CHECK(plan_private_scalars(body, locals, Letter('f')).eligible);
}

TEST_CASE("Private scalar leases preserve parents and clear reused cells", "[bc][private-scalars]")
{
  ScalarActivationArena arena;
  auto plan = plan_private_scalars({}, VariableMask("xz", ""), Letter('f'));
  REQUIRE(plan.eligible);
  ScalarActivationArena::Lease parent(arena, plan.count);
  PrivateScalarFrame frame{&plan, parent.cells()};
  CHECK(frame.read(Variable('x')).to_unsigned() == 0);
  CHECK_FALSE(frame.cell(Variable('x')).has_value());
  frame.cell(Variable('x')).emplace(123);
  auto const* original = &frame.read(Variable('x'));
  std::vector<std::unique_ptr<ScalarActivationArena::Lease>> children;
  for (unsigned depth = 0; depth < 256; ++depth) {
    children.push_back(std::make_unique<ScalarActivationArena::Lease>(arena, 26));
    children.back()->cells()[0].value.emplace(depth + 1);
    CHECK(&frame.read(Variable('x')) == original);
    CHECK(frame.read(Variable('x')).to_unsigned() == 123);
  }
  while (!children.empty())
    children.pop_back();
  for (unsigned repeat = 0; repeat < 20; ++repeat) {
    ScalarActivationArena::Lease child(arena, 26);
    for (unsigned i = 0; i < 26; ++i)
      CHECK_FALSE(child.cells()[i].value.has_value());
    child.cells()[0].value.emplace(repeat + 1);
  }
  CHECK(frame.read(Variable('x')).to_unsigned() == 123);
}

TEST_CASE("Private scalar leases unwind without exposing stale values", "[bc][private-scalars]")
{
  ScalarActivationArena arena;
  ScalarActivationArena::Cell* previous = nullptr;
  try {
    ScalarActivationArena::Lease lease(arena, 3);
    previous = lease.cells();
    previous[2].value.emplace(99);
    throw 1;
  }
  catch (int) {
  }
  ScalarActivationArena::Lease next(arena, 3);
  CHECK(next.cells() == previous);
  CHECK_FALSE(next.cells()[2].value.has_value());
}
