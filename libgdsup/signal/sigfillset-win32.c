/** \file libgdsup/signal/sigfillset-win32.c
 *  \brief Implemenation of sigemptyset() for Windows
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/signal.h"

#include <errno.h>

#include "support/support.h"

int sigfillset(sigset_t* set)
{
  if (set == NULL) {
    __support_log("sigaddset: Called with NULL set.\n");
    errno = EINVAL;
    return -1;
  }

  for (size_t i = 0; i < __GD_SS_FLAGS_SIZE; ++i) {
    set->__gd_ss_flags[i] = ~((uint32_t)0);
  }
  return 0;
}
