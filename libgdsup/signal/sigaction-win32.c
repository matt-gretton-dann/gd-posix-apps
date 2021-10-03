/** \file libgdsup/signal/sigaction-win32.c
 *  \brief Implemenation of sigaction() for Windows
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/signal.h"

#include <errno.h>
#include <stdbool.h>
#include <windows.h>

#include "support/support.h"

/* !!!! THREAD SAFETY !!!! */
/* Current action for SIGINT. */
static struct sigaction intaction;

static BOOL WINAPI ctrl_c_handler(DWORD dwCtrlType)
{
  if (dwCtrlType != CTRL_C_EVENT) {
    return FALSE;
  }

  bool use_sighandler = true;
  void (*sighandler)(int) = NULL;
  void (*sigactionhandler)(int, siginfo_t*, void*) = NULL;

  if (intaction.sa_flags & SA_SIGINFO) {
    use_sighandler = false;
    sigactionhandler = intaction.sa_sigaction;
  }
  else {
    sighandler = intaction.sa_handler;
  }

  if ((intaction.sa_flags & SA_RESETHAND) != 0) {
    intaction.sa_flags &= ~SA_SIGINFO;
    intaction.sa_handler = SIG_DFL;
  }

  if (use_sighandler) {
    if (sighandler == SIG_DFL) {
      return FALSE;
    }
    if (sighandler != SIG_IGN) {
      sighandler(SIGINT);
    }
    return TRUE;
  }

  /* Siginfo handler.  */
  siginfo_t si;
  si.si_signo = SIGINT;
  si.si_code = SI_USER;
  si.si_pid = (pid_t)GetCurrentProcessId();
  si.si_uid = 0;
  si.si_addr = NULL;
  sigactionhandler(SIGINT, &si, NULL);
  return TRUE;
}

static bool install_handlers(void)
{
  static sig_atomic_t installed = 0;
  if (!installed) {
    /* Set up default handlers.  */
    sigemptyset(&intaction.sa_mask);
    intaction.sa_flags = 0;
    intaction.sa_handler = SIG_DFL;

    /* Install Ctrl-C handler.  */
    BOOL success = SetConsoleCtrlHandler(ctrl_c_handler, TRUE);
    if (!success) {
      __support_log("install_handlers: Unable to install Console Control-C Handler.\n");
      errno = ENOTSUP;
      return false;
    }

    installed = 1;
  }

  return true;
}

int sigaction(int sig, struct sigaction const* __RESTRICT act, struct sigaction* __RESTRICT oact)
{
  if (sig != SIGINT) {
    __support_log("sigaction: Unsupported signal: %d\n", sig);
    errno = ENOTSUP;
    return -1;
  }

  if (oact != NULL) {
    *oact = intaction;
  }

  if (act != NULL) {
    struct sigaction new_action = *act;
    sigdelset(&new_action.sa_mask, SIGKILL);
    sigdelset(&new_action.sa_mask, SIGSTOP);
    intaction = new_action;

    if (!install_handlers()) {
      return -1;
    }
  }

  return 0;
}
