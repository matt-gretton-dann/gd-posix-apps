/** \file   libgdsup/include/gd/unistd.h
 *  \brief  Expose unistd.h
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_GD_UNISTD_H_INCLUDED
#define _LIBGDSUP_INCLUDE_GD_UNISTD_H_INCLUDED

#if defined(_WIN32)
#  include "gd/bits/defines.h"

#  ifdef STDIN_FILENO
#    undef STDIN_FILENO
#  endif
/** \brief File number of stdin.  */
#  define STDIN_FILENO 0

#  ifdef STDOUT_FILENO
#    undef STDOUT_FILENO
#  endif
/** \brief File number of stdout.  */
#  define STDOUT_FILENO 1

#  ifdef STDERR_FILENO
#    undef STDERR_FILENO
#  endif
/** \brief File number of stderr.  */
#  define STDERR_FILENO 2

/** \typedef ssize_t
 *  \brief   Signed integer, either count of bytes or error indication.
 */

#  if _WIN64
typedef long long ssize_t;
#  elif _WIN32
typedef long ssize_t;
#  else
#    error "Unable to specify ssize_t."
#  endif

/** \brief     close file-descriptor
 *  \param  fd File descriptor
 *  \return    -1 on error, 0 otherwise
 */
EXTERN_C int close(int fd);

/** \brief         Read from a file
 *  \param  fd     file descriptor
 *  \param  buf    Buffer to write to
 *  \param  nbytes Number of bytes to read into buffer
 *  \return        Number of bytes read, or -1 on error. \c errno will be updated.
 *
 * Windows implementation won't write more than 2^32 bytes at once.
 */
EXTERN_C ssize_t read(int fd, void* buf, size_t nbytes);

/** \brief       Remove a directory entry.
 *  \param  path Path to remove.
 *  \return      0 on success, -1 on failure - updating \c errno.
 */
EXTERN_C int unlink(char const* path);

/** \brief         Write to a file
 *  \param  int    file descriptor
 *  \param  buf    Buffer to read from
 *  \param  nbytes Number of bytes to write into file
 *  \return        Number of bytes written, or -1 on error. \c errno will be updated.
 */
EXTERN_C ssize_t write(int fd, const void* buf, size_t nbytes);

#else  // ^ Win32 v POSIX
#  include <unistd.h>
#endif  // Win32 or POSIX

#endif  // _LIBGDSUP_INCLUDE_GD_UNISTD_H_INCLUDED
