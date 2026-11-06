/** Opaque return storage owns only completely constructed values.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <catch2/catch.hpp>

#include <type_traits>

#include "integer-call-result.hh"

namespace {
using GD::Bc::Number;
using GD::Bc::Details::IntegerCallResult;
constexpr auto large = "123456789012345678901234567890123456789012345678901234567890";

#if GD_BC_PROFILE
void check_live(GD::Bc::Profile::Snapshot const& entry)
{
  auto const exit = GD::Bc::Profile::snapshot();
  CHECK(exit.total.live_bytes == entry.total.live_bytes);
  for (std::size_t i = 0; i != exit.allocations.size(); ++i)
    CHECK(exit.allocations[i].live_bytes == entry.allocations[i].live_bytes);
}
#endif
}  // namespace

TEST_CASE("Pure integer return storage survives throwing factories", "[bc][integer-function]")
{
  STATIC_REQUIRE_FALSE(std::is_copy_constructible_v<IntegerCallResult>);
  STATIC_REQUIRE_FALSE(std::is_move_constructible_v<IntegerCallResult>);
#if GD_BC_PROFILE
  auto const entry = GD::Bc::Profile::snapshot();
#endif
  {
    IntegerCallResult result;
    // Keep both outcomes possible for link-time reachability diagnostics.
    volatile bool throw_after_allocating = true;
    CHECK_FALSE(result.has_value());
    CHECK_THROWS_AS(result.construct([&]() -> Number {
      Number allocated(large, 10);
      if (throw_after_allocating)
        throw std::bad_alloc();
      return allocated;
    }),
                    std::bad_alloc);
    CHECK_FALSE(result.has_value());
    result.construct([] { return Number(9); });
    CHECK(result.has_value());
    CHECK(result.take() == Number(9));
  }
#if GD_BC_PROFILE
  check_live(entry);
#endif
}

TEST_CASE("Pure integer return storage retains escaped aliases", "[bc][integer-function]")
{
#if GD_BC_PROFILE
  auto const entry = GD::Bc::Profile::snapshot();
#endif
  {
    Number escaped;
    {
      Number source(large, 10);
      IntegerCallResult result;
      result.construct([&] { return source; });
      source.add(Number(1));
      escaped = result.take();
      CHECK(escaped == Number(large, 10));
      CHECK(source != escaped);
    }
    CHECK(escaped == Number(large, 10));
  }
#if GD_BC_PROFILE
  check_live(entry);
#endif
}

#if GD_BC_PROFILE
TEST_CASE("Pure integer return transport accounts for its extra move", "[bc][integer-function]")
{
  IntegerCallResult result;
  result.construct([] { return Number(7); });
  auto const before = GD::Bc::Profile::snapshot();
  auto value = result.take();
  auto const after = GD::Bc::Profile::snapshot();
  auto const move = static_cast<std::size_t>(GD::Bc::Profile::Event::digit_moves);
  CHECK(after.events[move] == before.events[move] + 1);
  CHECK(value == Number(7));
}
#endif
