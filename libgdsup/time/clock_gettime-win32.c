/** \file libgdsup/time/clock_gettime.c
 *  \brief Implemenation of clock_gettime()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/time.h"

#include <errno.h>
#include <windows.h>

#include "support/support.h"
#include "time/time.h"

/* Convert a value stored as a file time to a timespec
 *  Does not do the rebias for different epochs.
 */
static int convert_file_time_to_timespec(struct timespec* ts, FILETIME ft, LONGLONG bias)
{
  LARGE_INTEGER li;
  li.HighPart = ft.dwHighDateTime;
  li.LowPart = ft.dwLowDateTime;
  uint64_t file_time = li.QuadPart - bias;

  /* file_time is in units of 100 nano-seconds (10^-4).  tv_nsec is in micro-seconds (10^-9). */
  ts->tv_sec = (time_t)(file_time / HNS_PER_SEC);
  ts->tv_nsec = (long)((file_time % HNS_PER_SEC) * HNS_PER_NSEC);
  if (((int64_t)ts->tv_sec) * HNS_PER_SEC + ts->tv_nsec / HNS_PER_NSEC != file_time) {
    errno = EOVERFLOW;
    __support_log("convert_file_to_timespec: Overflow when converting file time to time_t.\n");
    return -1;
  }
  return 0;
}

int clock_gettime(clockid_t clock, struct timespec* ts)
{
  if (ts == NULL) {
    __support_log("clock_gettime: ts is NULL");
    errno = EINVAL;
    return -1;
  }

  switch (clock) {
  case CLOCK_PROCESS_CPUTIME_ID: {
    FILETIME start_time, end_time, user_time, kernel_time;
    BOOL success =
      GetProcessTimes(GetCurrentProcess(), &start_time, &end_time, &kernel_time, &user_time);
    if (!success) {
      __support_log("clock_gettime: Failed to get process time: %08x\n", GetLastError());
      errno = ENOTSUP;
      return -1;
    }

    return convert_file_time_to_timespec(ts, user_time, 0);
  }
  case CLOCK_THREAD_CPUTIME_ID: {
    FILETIME start_time, end_time, user_time, kernel_time;
    BOOL success =
      GetProcessTimes(GetCurrentProcess(), &start_time, &end_time, &kernel_time, &user_time);
    if (!success) {
      __support_log("clock_gettime: Failed to get thread time: %08x\n", GetLastError());
      errno = ENOTSUP;
      return -1;
    }

    return convert_file_time_to_timespec(ts, user_time, 0);
  }
  case CLOCK_REALTIME: {
    FILETIME system_time;
    GetSystemTimeAsFileTime(&system_time);
    return convert_file_time_to_timespec(ts, system_time, WINDOWS_TO_POSIX_BIAS);
  }
  case CLOCK_MONOTONIC: {
    int64_t query_frequency = __support_performance_frequency();
    LARGE_INTEGER li;

    /* Do not need to check error code as Win32 guarantees this will succeed on WinXP and later.
     */
    QueryPerformanceCounter(&li);
    ts->tv_sec = (time_t)(li.QuadPart / query_frequency);
    ts->tv_nsec = (long)(((li.QuadPart % query_frequency) * NSEC_PER_SEC) / query_frequency);
    return 0;
  }
  default:
    __support_log("clock_gettime: Unrecognised clock id: %d\n", clock);
    errno = EINVAL;
    return -1;
  }
}
