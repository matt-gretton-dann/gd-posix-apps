/** \file   cli-interrupt.cc
 *  \brief  POSIX CLI interrupt handling
 *  \author Copyright 2026, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/signal.h"
#include "gd/unistd.h"

#include <atomic>

#include "bc.hh"

namespace GD::Bc::Details {
__EXTERN_C void handle_sigint(int signal);

struct CliInterruptScope::State
{
  struct sigaction previous{};
};

#if !defined(_WIN32)
enum class CliInterruptPhase : unsigned { idle, executing, interrupted };
// The CLI has one executing thread. The handler and that thread exchange
// state through a lock-free atomic only.
static std::atomic<CliInterruptPhase> cli_interrupt_phase{CliInterruptPhase::idle};

__EXTERN_C void handle_cli_sigint(int /*signal*/)
{
  auto expected = CliInterruptPhase::executing;
  if (cli_interrupt_phase.compare_exchange_strong(expected, CliInterruptPhase::interrupted,
                                                  std::memory_order_relaxed)) {
    handle_sigint(SIGINT);
    return;
  }
  // Outside an execution, or on a second signal in one execution, preserve
  // default termination. SIGINT is blocked until this handler returns.
  struct sigaction action{};
  action.sa_handler = SIG_DFL;
  (void)sigemptyset(&action.sa_mask);
  if (sigaction(SIGINT, &action, nullptr) == 0 && ::kill(::getpid(), SIGINT) == 0) {
    return;
  }
  ::_exit(128 + SIGINT);
}
#endif

CliInterruptScope::CliInterruptScope()
{
#if !defined(_WIN32)
  if constexpr (std::atomic<CliInterruptPhase>::is_always_lock_free) {
    auto state = std::make_unique<State>();
    if (sigaction(SIGINT, nullptr, &state->previous) == 0 &&
        state->previous.sa_handler == SIG_DFL) {
      struct sigaction action{};
      action.sa_handler = handle_cli_sigint;
      (void)sigemptyset(&action.sa_mask);
      action.sa_flags = SA_RESTART;
      if (sigaction(SIGINT, &action, nullptr) == 0) {
        state_ = std::move(state);
      }
    }
  }
#endif
}

CliInterruptScope::~CliInterruptScope()
{
  if (state_) {
    (void)sigaction(SIGINT, &state_->previous, nullptr);
  }
}

void set_cli_interrupt_active(bool active) noexcept
{
#if !defined(_WIN32)
  cli_interrupt_phase.store(active ? CliInterruptPhase::executing : CliInterruptPhase::idle,
                            std::memory_order_relaxed);
#else
  (void)active;
#endif
}

}  // namespace GD::Bc::Details
