/** Public-loader replacement after hot integer call generations.
 * SPDX-License-Identifier: Apache-2.0
 */
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <sstream>
#include <string>
#include <string_view>

#include "bc.hh"

namespace GD::Bc::Library {
auto text() -> std::string_view;
}

namespace {
auto run(GD::Bc::VM& vm, std::string_view source) -> bool
{
  // StringReader borrows this view; its caller keeps the input alive until
  // this complete parse and execution have finished.
  GD::Bc::Parser parser(
    std::make_unique<GD::Bc::Lexer>(std::make_unique<GD::Bc::StringReader>(source)), false);
  auto code = parser.parse();
  return code && !vm.execute(*code);
}

auto reload(GD::Bc::VM& vm) -> bool
{
#if defined(GD_BC_PREPARSED_LIBRARY)
  GD::Bc::Library::load(vm);
  return true;
#else
  return run(vm, GD::Bc::Library::text());
#endif
}

#if GD_BC_PROFILE
auto successes() -> std::uint64_t
{
  return GD::Bc::Profile::snapshot()
    .events[static_cast<std::size_t>(GD::Bc::Profile::Event::integer_function_successes)];
}
#endif

auto check(GD::Bc::VM::ExecutionChecks checks, bool save_specials) -> bool
{
#if GD_BC_PROFILE
  auto const entry = GD::Bc::Profile::snapshot();
#endif
  {
    std::ostringstream output, errors;
    GD::Bc::VM vm(output, errors, save_specials, checks);
#if GD_BC_PROFILE
    auto const before_warmup = successes();
#endif
    // The cubic result escapes as an exact promoted value. Reloading must neither
    // retain the old pure plan nor invalidate the independently owned result.
    if (!run(vm, "define a(x) {\nreturn(x*x*x+1)\n}\n"
                 "for(i=0;i<18;i++) q=a(2)\n"))
      return false;
#if GD_BC_PROFILE
    auto const before_escape = GD::Bc::Profile::snapshot();
#endif
    if (!run(vm, "v=a(9223372036854775807)\n"))
      return false;
#if GD_BC_PROFILE
    auto const after_warmup = successes();
    bool const enabled =
      GD_BC_EXPECT_INTEGER_FUNCTIONS && checks == GD::Bc::VM::ExecutionChecks::verified;
    if ((after_warmup > before_warmup) != enabled)
      return false;
    auto const after_escape = GD::Bc::Profile::snapshot();
    auto const success =
      static_cast<std::size_t>(GD::Bc::Profile::Event::integer_function_successes);
    auto const promotion =
      static_cast<std::size_t>(GD::Bc::Profile::Event::integer_function_promotions);
    if (after_escape.events[success] - before_escape.events[success] != (enabled ? 1U : 0U) ||
        (after_escape.events[promotion] > before_escape.events[promotion]) != enabled)
      return false;
#endif
    if (!reload(vm) || !run(vm, "scale\nscale=0\n"
                                "for(i=0;i<18;i++) q=a(0)\na(0)\nv\n"))
      return false;
#if GD_BC_PROFILE
    // The real library touches special registers and cannot use the pure tier.
    if (successes() != after_warmup)
      return false;
#endif
    if (output.str() != "20\n0\n"
                        "784637716923335095224261902710254454442933591094742482944\n")
      return false;

    // Warm a statically rejected generation, then replace its deferred image
    // before the first library call materializes it. User binding must win.
    if (!run(vm, "n=0\ndefine a(x) {\nn+=1\nreturn(x)\n}\n"
                 "for(i=0;i<18;i++) q=a(2)\n") ||
        !reload(vm) ||
        !run(vm, "scale=0\ndefine a(x) {\nreturn(x+1)\n}\n"
                 "for(i=0;i<18;i++) q=a(2)\nq\nn\n"))
      return false;
#if GD_BC_PROFILE
    auto const after_replacement = successes();
    if ((after_replacement > after_warmup) != enabled)
      return false;
#endif
    if (!reload(vm) || !run(vm, "scale=0\na(0)\n"))
      return false;
#if GD_BC_PROFILE
    if (successes() != after_replacement)
      return false;
#endif
    if (output.str() != "20\n0\n"
                        "784637716923335095224261902710254454442933591094742482944\n"
                        "3\n18\n0\n" ||
        !errors.str().empty())
      return false;
  }
#if GD_BC_PROFILE
  auto const exit = GD::Bc::Profile::snapshot();
  if (exit.total.live_bytes != entry.total.live_bytes)
    return false;
  for (std::size_t i = 0; i != exit.allocations.size(); ++i) {
    if (exit.allocations[i].live_bytes != entry.allocations[i].live_bytes)
      return false;
  }
#endif
  return true;
}
}  // namespace

auto main() -> int
{
  for (auto checks : {GD::Bc::VM::ExecutionChecks::full, GD::Bc::VM::ExecutionChecks::verified}) {
    for (bool save_specials : {false, true}) {
      if (!check(checks, save_specials)) {
        std::fprintf(stderr, "Library reload lost state: checks=%s, save_specials=%d\n",
                     checks == GD::Bc::VM::ExecutionChecks::full ? "full" : "verified",
                     static_cast<int>(save_specials));
        return 1;
      }
    }
  }
  return 0;
}
