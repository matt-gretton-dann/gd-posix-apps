/** \file libgdsup/time/time.h
 *  \brief Internal header
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef __LIBGDSUP_TIME_TIME_H_INCLUDED
#define __LIBGDSUP_TIME_TIME_H_INCLUDED
#include "gd/time.h"

#include <errno.h>
#include <stdint.h>

#include "support/support.h"

/* Number of Hundred nanoseconds per second. */
#define HNS_PER_SEC 10000000ULL

/* Number of 100 nanoseconds per second */
#define HNS_PER_NSEC 100

/* Number of nanoseconds per seconds. */
#define NSEC_PER_SEC 1000000000

/** \brief Value to subtract from a Windows time to get to the equivalent POSIX time
 *
 * Calculated as 369 years from 1 Jan 1601 to 1 Jan 1970, of which 89 were leap years, multiplied
 * up to be in 100-nsecs units.
 *
 * Note that as this is UTC we ignore the 11 day gap in 1752.
 */
#define WINDOWS_TO_POSIX_BIAS (((int64_t)(369 * 365 + 89)) * 24 * 60 * 60 * HNS_PER_SEC)

/** \brief  Get the performance frequency counter.
 *  \return Performance frequency.
 */
int64_t __support_performance_frequency();

#endif  //__LIBGDSUP_TIME_TIME_H_INCLUDED
