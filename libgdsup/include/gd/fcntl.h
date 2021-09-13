/** \file   libgdsup/include/gd/fcntl.h
 *  \brief  Expose fcntl.h
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef LIBGDSUP_INCLUDE_GD_FCNTL_H_INCLUDED
#define LIBGDSUP_INCLUDE_GD_FCNTL_H_INCLUDED

#if _WIN32
#  define _CRT_DECLARE_NONSTDC_NAMES 0  // NOLINT
#endif

#include <fcntl.h>

#if _WIN32
#  include "gd/bits/defines.h"

/** \brief Close file on process spawn.  */
#  define O_CLOEXEC _O_NOINHERIT
/** \brief Create new file if it doesn't exist.  */
#  define O_CREAT _O_CREAT
// O_DIRECTORY
/** \brief When used with \c O_CREAT ensure we created a new file.  */
#  define O_EXCL _O_EXCL
// O_NOCTTY
// O_NOFOLLOW
/** \brief Truncated the file. */
#  define O_TRUNC _O_TRUNC
// O_TTY_INIT
/** \brief Append to the end of the file.  */
#  define O_APPEND _O_APPEND
// O_DSYNC
// O_NONBLOCK
// O_RSYNC
// O_SYNC

/** \brief Open file for execution.
 *
 * Windows doesn't have this concept so map to open for reading.  Technially a POSIX violation.
 */
#  define O_EXEC _O_RDONLY
/** \brief Open file for reading. */
#  define O_RDONLY _O_RDONLY
/** \brief Open file for writing. */
#  define O_WRONLY _O_WRONLY
/** \brief Open file for reading & writing.  */
#  define O_RDWR _O_RDWR
// O_SEARCH

/** \brief        Open a file
 *  \param  path  File to open
 *  \param  oflag Flags to open with.
 *  \param  perm  User permissions
 *  \return       File descriptor or -1 on failure, \c errno updated.
 */
__EXTERN_C int open(const char* path, int oflag, ...);

#endif

#endif
