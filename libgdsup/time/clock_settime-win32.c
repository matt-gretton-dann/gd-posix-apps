/** \file libgdsup/time/clock_settime.c
 *  \brief Implemenation of clock_settime()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/time.h"

#include <errno.h>

#include "support/support.h"

int clock_settime(clockid_t clock, struct timespec const* _ts)
{
  /* Current implementation is a "NULL" implementation.  */
  if (clock == CLOCK_REALTIME) {
    if (_ts == NULL) {
      __support_log("clock_settime called with NULL timespec.\n");
      errno = EINVAL;
      return -1;
    }

    __support_log("clock_settime: Called to set realtime clock - not implemented yet.\n");
    errno = EINVAL;
    return -1;
  }

  __support_log("clock_settime: Called to set unsettable clock with ID: %d\n", clock);
  errno = EINVAL;
  return -1;
}
