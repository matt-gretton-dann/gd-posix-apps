/** \file test-vm-signal.cc
 * Check process signal state in an isolated executable, without delivering signals.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "gd/signal.h"

#include <cerrno>
#include <cstring>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string_view>

#include "bc.hh"

namespace {
class RestoreAction
{
public:
  explicit RestoreAction(struct sigaction const& action) : action_(action) {}
  ~RestoreAction() { (void)sigaction(SIGINT, &action_, nullptr); }
  RestoreAction(RestoreAction const&) = delete;
  auto operator=(RestoreAction const&) -> RestoreAction& = delete;

private:
  struct sigaction action_;
};

class ThrowingBuffer : public std::streambuf
{
  auto overflow(int_type /*unused*/) -> int_type override
  {
    throw std::runtime_error("deliberate output failure");
  }
};

auto masks_match(sigset_t const& lhs, sigset_t const& rhs) -> bool
{
#if _WIN32
  // The supplemental mask is one initialized unsigned word, without padding.
  static_assert(sizeof(sigset_t) == sizeof(unsigned));
  return std::memcmp(&lhs, &rhs, sizeof(sigset_t)) == 0;
#else
  return sigismember(&lhs, SIGINT) == sigismember(&rhs, SIGINT) &&
         sigismember(&lhs, SIGTERM) == sigismember(&rhs, SIGTERM);
#endif
}

auto fail(std::string_view reason) -> int
{
  std::cerr << reason << '\n';
  return 1;
}
}  // namespace

auto main(int argc, char** argv) -> int
{
  if (argc != 2) {
    return fail("Expected normal or throw mode");
  }
  bool const throwing = std::string_view(argv[1]) == "throw";
  if (!throwing && std::string_view(argv[1]) != "normal") {
    return fail("Unknown mode");
  }
  struct sigaction original{};
  if (sigaction(SIGINT, nullptr, &original) != 0) {
    if (errno == ENOTSUP) {
      std::cerr << "SIGINT disposition unsupported\n";
      return 77;
    }
    return fail("Cannot query initial SIGINT action");
  }
  RestoreAction restore(original);
  struct sigaction requested{};
  requested.sa_handler = SIG_IGN;
  requested.sa_flags = SA_RESTART;
  if (sigemptyset(&requested.sa_mask) != 0 || sigaddset(&requested.sa_mask, SIGINT) != 0 ||
      sigaddset(&requested.sa_mask, SIGTERM) != 0) {
    return fail("Cannot initialize signal mask");
  }
  if (sigaction(SIGINT, &requested, nullptr) != 0) {
    if (errno == ENOTSUP) {
      std::cerr << "SIGINT installation unsupported\n";
      return 77;
    }
    return fail("Cannot install prior SIGINT action");
  }
  struct sigaction expected{};
  if (sigaction(SIGINT, nullptr, &expected) != 0 || expected.sa_handler != SIG_IGN) {
    return fail("Signal API lost the first requested action before VM execution");
  }
  std::ostringstream output;
  std::ostringstream errors;
  ThrowingBuffer buffer;
  std::ostream failing_output(&buffer);
  failing_output.exceptions(std::ios::badbit);
  GD::Bc::VM vm(throwing ? failing_output : output, errors, false);
  using I = GD::Bc::Instruction;
  GD::Bc::Instructions code{I(I::Opcode::eof)};
  if (throwing) {
    code = {I(I::Opcode::string, "hello"), I(I::Opcode::print, I::Offset(-1), I::Stream::output),
            I(I::Opcode::eof)};
  }
  bool threw = false;
  try {
    (void)vm.execute(code);
  }
  catch (std::exception const&) {
    threw = true;
  }
  if (threw != throwing) {
    return fail("Execution exception propagation changed");
  }
  struct sigaction actual{};
  if (sigaction(SIGINT, nullptr, &actual) != 0) {
    return fail("Cannot query final action");
  }
  auto const flags = SA_RESTART | SA_RESETHAND | SA_SIGINFO | SA_NODEFER;
  if ((actual.sa_flags & flags) != (expected.sa_flags & flags)) {
    return fail("VM did not restore the prior SIGINT flags");
  }
  if (actual.sa_handler != expected.sa_handler) {
    return fail("VM did not restore the prior SIGINT handler");
  }
  if (!masks_match(actual.sa_mask, expected.sa_mask)) {
    return fail("VM did not restore the prior signal mask");
  }
  if (!output.str().empty() || !errors.str().empty()) {
    return fail("Unexpected VM output");
  }
  return 0;
}
