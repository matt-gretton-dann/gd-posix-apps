/** \file test-coefficient-owner.cc
 * Strong/weak coefficient control-block lifetime and thread handoff checks.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <catch2/catch.hpp>

#include <array>
#include <atomic>
#include <memory>
#include <stdexcept>
#include <thread>
#include <utility>

#include "coefficient-owner.hh"

namespace {
struct Value
{
  unsigned* destroyed;
  int value = 7;
  explicit Value(unsigned& count, bool fail = false) : destroyed(&count)
  {
    if (fail) {
      throw std::runtime_error("coefficient construction failed");
    }
  }
  ~Value() { ++*destroyed; }
};
using Owner = GD::Bc::Details::CoefficientOwner<Value>;
}  // namespace

TEST_CASE("Compact coefficient handles preserve strong and weak lifetimes",
          "[bc][storage][coefficient-owner]")
{
  REQUIRE(sizeof(Owner) == sizeof(void*));
  REQUIRE(sizeof(Owner::Weak) == sizeof(void*));
  unsigned destroyed = 0;
  auto owner = Owner::make(destroyed);
  Owner::Weak weak(owner);
  REQUIRE(owner.use_count() == 1);
  REQUIRE(owner.shares_owner(weak));
  auto cleared = weak;
  cleared.reset();
  REQUIRE(cleared.expired());
  REQUIRE(owner.use_count() == 1);
  auto copy = owner;
  REQUIRE(owner.use_count() == 2);
  auto moved = std::move(copy);
  REQUIRE_FALSE(copy);
  REQUIRE(copy.use_count() == 0);
  REQUIRE(moved->value == 7);
  auto const& self = moved;
  moved = self;
  auto* move_self = std::addressof(moved);
  moved = std::move(*move_self);
  REQUIRE(moved.use_count() == 2);
  owner.reset();
  REQUIRE_FALSE(weak.expired());
  REQUIRE(destroyed == 0);
  moved.reset();
  REQUIRE(weak.expired());
  REQUIRE(destroyed == 1);
  for (unsigned i = 0; i != 100; ++i) {
    auto replacement = Owner::make(destroyed);
    REQUIRE_FALSE(replacement.shares_owner(weak));
  }
  REQUIRE(destroyed == 101);
  weak = {};
}

TEST_CASE("Compact coefficient construction failure releases its control block",
          "[bc][storage][coefficient-owner]")
{
  unsigned destroyed = 0;
#if GD_BC_PROFILE
  auto const before = GD::Bc::Profile::snapshot().total.live_bytes;
#endif
  REQUIRE_THROWS_AS(Owner::make(destroyed, true), std::runtime_error);
  REQUIRE(destroyed == 0);
#if GD_BC_PROFILE
  REQUIRE(GD::Bc::Profile::snapshot().total.live_bytes == before);
#endif
  auto next = Owner::make(destroyed);
  next.reset();
  REQUIRE(destroyed == 1);
}

#if !GD_BC_PROFILE
// Allocation profiling has deliberately thread-confined counters. Exercise
// independent atomic owners in an ordinary build, without changing that scope.
TEST_CASE("Independent coefficient owners survive concurrent copy and release",
          "[bc][storage][coefficient-owner][thread]")
{
  unsigned destroyed = 0;
  auto owner = Owner::make(destroyed);
  Owner::Weak observation(owner);
  std::atomic<bool> correct{true};
  std::array<std::thread, 4> threads;
  try {
    for (auto& thread : threads) {
      thread = std::thread([owner, &correct] {
        Owner::Weak weak(owner);
        for (unsigned i = 0; i != 10000; ++i) {
          auto copy = owner;
          auto moved = std::move(copy);
          auto weak_copy = weak;
          if (copy || moved->value != 7 || weak_copy.expired() || !moved.shares_owner(weak_copy)) {
            correct.store(false, std::memory_order_relaxed);
          }
        }
      });
    }
  }
  catch (...) {
    for (auto& thread : threads) {
      if (thread.joinable()) {
        thread.join();
      }
    }
    throw;
  }
  owner.reset();
  for (auto& thread : threads) {
    thread.join();
  }
  REQUIRE(correct.load(std::memory_order_relaxed));
  REQUIRE(observation.expired());
  REQUIRE(destroyed == 1);
}
#endif
