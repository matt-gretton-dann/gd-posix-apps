/** \file libgdsup/signal/sigdelset-win32.c
 *  \brief Implemenation of sigdelset() for Windows
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/signal.h"

#include <errno.h>

#include "support/support.h"

int sigdelset(sigset_t* _set, int _sig)
{
  if (_set == NULL) {
    __support_log("sigaddset: Called with NULL set.\n");
    errno = EINVAL;
    return -1;
  }
  uint32_t entry = _sig / __GD_SS_FLAGS_BITS;
  if (entry >= __GD_SS_FLAGS_SIZE) {
    __support_log("sigaddset: Signal %d doesn't fit in sigset...\n", _sig);
    errno = EINVAL;
    return -1;
  }

  uint32_t bit = ((uint32_t)1) << (_sig % __GD_SS_FLAGS_BITS);
  _set->__gd_ss_flags[entry] &= ~bit;
  return 0;
}
