/** \file   libgdsup/include/gd/time.h
 *  \brief  Expose time.h
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_GD_STRING_H_INCLUDED
#define _LIBGDSUP_INCLUDE_GD_STRING_H_INCLUDED

#include "gd/bits/defines.h"

#include <time.h>

#if _WIN32

/** Type used to specify a clock ID. */
typedef int clockid_t;

/** Identifier of clock giving the time post epoch. */
#  define CLOCK_REALTIME ((clockid_t)1)

/** Identifier of a montonic clock - it does not have a known epoch. */
#  define CLOCK_MONOTONIC ((clockid_t)2)

/** Identifier for the clock that measures the process CPU time. */
#  define CLOCK_PROCESS_CPUTIME_ID ((clockid_t)3)

/** Identifier for the clock that measures the thread CPU time. */
#  define CLOCK_THREAD_CPUTIME_ID ((clockid_t)4)

/** \brief         Get the resolution of a clock.
 *  \param  _clock ID of clock to get resolution of.
 *  \param  _ts    Time spec to populate with result (is allowed to be NULL by spec).
 *  \return        0 on success, -1 on error with code in code in \c error.
 *
 * If \a _ts is NULL this is a no-op.
 *
 * \subsection Errors
 *
 * The possible error return codes are:
 *
 *  * \c EINVAL - an invalid clock ID is used.
 */
__EXTERN_C int clock_getres(clockid_t _clock, struct timespec* _ts);

/** \brief         Get the current value of a given clock
 *  \param  _clock ID of clock to get current value of
 *  \param  _ts    Time spec to populate with result
 *  \return        0 on success, -1 on error with code in code in \c error.
 *
 * \subsection Errors
 *
 * The possible error return codes are:
 *
 *  * \c EINVAL - an invalid clock ID is used, or _ts is NULL.
 */
__EXTERN_C int clock_gettime(clockid_t _clock, struct timespec* _ts);

/** \brief         Set the current time of a clock
 *  \param  _clock ID of clock to set the time of
 *  \param  _ts    Time spec to set the time to.
 *  \return        0 on success, -1 on error with code in code in \c error.
 *
 * \subsection Errors
 *
 * The possible error return codes are:
 *
 *  * \c EINVAL - an invalid clock ID is used, or _ts is NULL.
 *  * \c EPERM  - lack of permission.
 */
__EXTERN_C int clock_settime(clockid_t _clock, struct timespec const* _ts);

#endif  // _WIN32

#endif  // _LIBGDSUP_INCLUDE_GD_STDLIB_H_INCLUDED
