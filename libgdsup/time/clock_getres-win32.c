/** \file libgdsup/time/clock_getres-win32.c
 *  \brief Win32 Implemenation of clock_getres()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/time.h"

#include <errno.h>

#include "support/support.h"
#include "time/time.h"

int clock_getres(clockid_t clock, struct timespec* ts)
{
  if (ts == NULL) {
    /* ts is allowed to be NULL by the spec in which case we do nothing. */
    return 0;
  }

  switch (clock) {
  case CLOCK_MONOTONIC: {
    int64_t query_frequency = __support_performance_frequency();
    ts->tv_sec = 0;
    ts->tv_nsec = (long)(NSEC_PER_SEC / query_frequency);
    return 0;
  }
  case CLOCK_PROCESS_CPUTIME_ID:
  case CLOCK_REALTIME:
  case CLOCK_THREAD_CPUTIME_ID:
    ts->tv_sec = 0;
    ts->tv_nsec = HNS_PER_NSEC;
    return 0;
  default:
    __support_log("clock_getres: Invalid clock ID %d\n", clock);
    errno = EINVAL;
    return -1;
  }
}
