/** Differential VM state and activation checks for portable integer regions.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <new>

#include <catch2/catch.hpp>

#include <array>
#include <memory>
#include <sstream>
#include <string>
#include <string_view>
#include <tuple>

#include "bc.hh"

namespace {
using I = GD::Bc::Instruction;
using O = I::Opcode;
using D = I::Offset;
using Checks = GD::Bc::VM::ExecutionChecks;
using GD::Bc::Variable;

struct Observation
{
  std::string output;
  std::array<std::uint64_t, 3> events{};
};

auto parse(std::string const& program) -> GD::Bc::Instructions
{
  GD::Bc::Parser parser(
    std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(program)), false);
  auto code = parser.parse();
  REQUIRE(code);
  if (!code->empty() && code->back().opcode() == O::eof)
    code->pop_back();
  return *code;
}

auto observe(std::string const& program, Checks checks, std::string const& initial = {},
             bool all_locations = false) -> Observation
{
  std::ostringstream output;
  std::ostringstream errors;
  GD::Bc::VM vm(output, errors, false, checks);
  if (!initial.empty()) {
    auto setup = parse(initial);
    REQUIRE(vm.execute(setup));
  }
  auto code = parse(program);
  if (all_locations) {
    GD::Bc::Instructions locations;
    for (char letter : std::string_view("abcdefghijklmnopqrstuvwxyz"))
      locations.emplace_back(O::variable, Variable(letter));
    code.insert(code.begin(), locations.begin(), locations.end());
  }
#if GD_BC_PROFILE
  auto const before = GD::Bc::Profile::snapshot();
#endif
  CHECK(vm.execute(code));
  Observation observed;
#if GD_BC_PROFILE
  auto const after = GD::Bc::Profile::snapshot();
  using E = GD::Bc::Profile::Event;
  constexpr std::array events{E::portable_scalar_attempts, E::portable_scalar_successes,
                              E::portable_scalar_fallbacks};
  for (unsigned index = 0; index < events.size(); ++index) {
    auto const event = static_cast<std::size_t>(events[index]);
    observed.events[index] = after.events[event] - before.events[event];
  }
#endif
  auto inspect = parse("{i;s;scale(s);a;a[0];}\n");
  CHECK(vm.execute(inspect));
  CHECK(errors.str().empty());
  observed.output = output.str();
  return observed;
}

void activation(Observation const& observed, Checks checks, bool eligible, bool completes = true)
{
#if GD_BC_PROFILE
  if (GD_BC_EXPECT_NATIVE_SCALAR && GD_BC_EXPECT_PORTABLE_SCALAR)
    return;  // Existing Apple emission has priority; mapping failure may fall through.
  bool const attempted = GD_BC_EXPECT_PORTABLE_SCALAR && checks == Checks::verified && eligible;
  CHECK(observed.events[0] == (attempted ? 1 : 0));
  CHECK(observed.events[1] == (attempted && completes ? 1 : 0));
  CHECK(observed.events[2] == (attempted && !completes ? 1 : 0));
#else
  (void)observed;
  (void)checks;
  (void)eligible;
  (void)completes;
#endif
}
}  // namespace

TEST_CASE("Portable scalar VM execution preserves both checked and verified observable state",
          "[bc][vm][portable-scalar-vm]")
{
  for (Checks checks : {Checks::full, Checks::verified}) {
    for (unsigned bound : {0U, 1U, 2U, 17U, 101U}) {
      auto const observed = observe("for(i=0;i<" + std::to_string(bound) + ";i++)s+=i\n", checks);
      auto const sum = bound == 0 ? 0 : bound * (bound - 1) / 2;
      CHECK(observed.output == std::to_string(bound) + '\n' + std::to_string(sum) + "\n0\n0\n0\n");
      activation(observed, checks, true);
    }
    auto const unrelated = observe("for(i=0;i<10;i++)s+=i\n", checks, "{a=7;a[0]=11;}\n");
    CHECK(unrelated.output == "10\n45\n0\n7\n11\n");
    activation(unrelated, checks, true);
    auto const scaled = observe("for(i=0;i<10;i++)s+=i\n", checks, "scale=3\n");
    CHECK(scaled.output == "10\n45\n0\n0\n0\n");
    activation(scaled, checks, true);
    auto const radix = observe("for(i=0;i<10;i++)s+=i\n", checks, "obase=2\n");
    CHECK(radix.output == "1010\n101101\n0\n0\n0\n");
    activation(radix, checks, true);
  }
}

TEST_CASE("Portable scalar VM guards retain nonzero fractional and nondecimal semantics",
          "[bc][vm][portable-scalar-vm]")
{
  for (Checks checks : {Checks::full, Checks::verified}) {
    for (auto const& [initial, expected] :
         {std::pair{"s=7\n", "10\n52\n0\n0\n0\n"}, std::pair{"s= -7\n", "10\n38\n0\n0\n0\n"},
          std::pair{"s=0.0\n", "10\n45.0\n1\n0\n0\n"}, std::pair{"ibase=2\n", "2\n1\n0\n0\n0\n"}}) {
      auto const observed = observe("for(i=0;i<10;i++)s+=i\n", checks, initial);
      CHECK(observed.output == expected);
      activation(observed, checks, false);
    }
  }
}

TEST_CASE("Portable scalar VM overflow replays once without publishing private stores",
          "[bc][vm][portable-scalar-vm]")
{
  for (Checks checks : {Checks::full, Checks::verified}) {
    auto const observed = observe("for(i=0;i<2;i++)s+=18446744073709551615\n", checks);
    CHECK(observed.output == "2\n36893488147419103230\n0\n0\n0\n");
    activation(observed, checks, true, false);
    auto const exact = observe("for(i=0;i<1;i++)s+=18446744073709551615\n", checks);
    CHECK(exact.output == "1\n18446744073709551615\n0\n0\n0\n");
    activation(exact, checks, true);
  }
}

TEST_CASE("Portable scalar VM declines output array and call streams without changing behavior",
          "[bc][vm][portable-scalar-vm]")
{
  for (Checks checks : {Checks::full, Checks::verified}) {
    auto const output = observe("for(i=0;i<3;i++){s+=i;i;}\n", checks);
    CHECK(output.output == "0\n1\n2\n3\n3\n0\n0\n0\n");
    activation(output, checks, false);
    auto const arrays = observe("for(i=0;i<3;i++)a[i]=i+7\n", checks);
    CHECK(arrays.output == "3\n0\n0\n0\n7\n");
    activation(arrays, checks, false);
    auto const calls =
      observe("for(i=0;i<3;i++)s=s+f(i)\n", checks, "define f(x){\nreturn(x+1)\n}\n");
    CHECK(calls.output == "3\n6\n0\n0\n0\n");
    activation(calls, checks, false);
  }
}

TEST_CASE("Portable scalar VM guards every encoded scalar location", "[bc][vm][portable-scalar-vm]")
{
  // Prefix unused location producers so the plan's entry guard includes all
  // letters without exceeding its instruction bound. Spell the alphabet:
  // character codes need not be contiguous on a supported target.
  for (Checks checks : {Checks::full, Checks::verified}) {
    auto const zero = observe("for(i=0;i<5;i++)s+=i\n", checks, {}, true);
    CHECK(zero.output == "5\n10\n0\n0\n0\n");
    activation(zero, checks, true);
    for (char letter : std::string_view("abcdefghijklmnopqrstuvwxyz")) {
      auto const initial = std::string(1, letter) + "=7\n";
      auto const observed = observe("for(i=0;i<1;i++)s+=i\n", checks, initial, true);
      CHECK(observed.output == std::string("1\n") + (letter == 's' ? "7" : "0") + "\n0\n" +
                                 (letter == 'a' ? "7" : "0") + "\n0\n");
      activation(observed, checks, false);
    }
  }
}
