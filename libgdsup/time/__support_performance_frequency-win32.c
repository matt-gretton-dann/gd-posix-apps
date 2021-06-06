/** \file libgdsup/time/__support_performance_frequency-win32.c
 *  \brief Implemenation of __support_performance_frequency() for Win32
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/time.h"

#include <errno.h>
#include <windows.h>

#include "support/support.h"
#include "time/time.h"

int64_t __support_performance_frequency()
{
  static int64_t query_frequency = 0;
  LARGE_INTEGER li;

  if (query_frequency == 0) {
    /* Do not need to check error code as Win32 guarantees this will succeed on WinXP and later.
     */
    QueryPerformanceFrequency(&li);
    /* We don't care what the old value was as it will either be zero or the right value.  Just
     * write atomically.  */
    InterlockedExchange64(&query_frequency, li.QuadPart);
  }

  return query_frequency;
}
