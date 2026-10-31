/** \file test-first-action.cc
 * Check the first sigaction request in an isolated process.
 * SPDX-License-Identifier: Apache-2.0
 */

#include "gd/signal.h"

#include <cerrno>
#include <iostream>

namespace {
class RestoreAction
{
public:
  explicit RestoreAction(struct sigaction const& saved) : saved_(saved) {}
  ~RestoreAction() { (void)sigaction(SIGINT, &saved_, nullptr); }
  RestoreAction(RestoreAction const&) = delete;
  auto operator=(RestoreAction const&) -> RestoreAction& = delete;

private:
  struct sigaction saved_;
};
}  // namespace

auto main() -> int
{
  struct sigaction requested{};
  requested.sa_handler = SIG_IGN;
  requested.sa_flags = SA_RESTART;
  if (sigemptyset(&requested.sa_mask) != 0) {
    return 1;
  }
  struct sigaction original{};
  // This must be the process's first call, not a fixture-preinitialized API.
  if (sigaction(SIGINT, &requested, &original) != 0) {
    if (errno == ENOTSUP) {
      std::cerr << "SIGINT action facility unsupported\n";
      return 77;
    }
    std::cerr << "Initial sigaction installation failed\n";
    return 1;
  }
  RestoreAction restore(original);
  struct sigaction actual{};
  if (sigaction(SIGINT, nullptr, &actual) != 0 || actual.sa_handler != SIG_IGN ||
      (actual.sa_flags & SA_RESTART) == 0) {
    std::cerr << "The first sigaction request was lost during initialization\n";
    return 1;
  }
  requested.sa_handler = SIG_DFL;
  requested.sa_flags = 0;
  struct sigaction previous{};
  if (sigaction(SIGINT, &requested, &previous) != 0 || previous.sa_handler != SIG_IGN ||
      (previous.sa_flags & SA_RESTART) == 0) {
    std::cerr << "Replacing an action did not return the previous request\n";
    return 1;
  }
  return 0;
}
