/** \file   libgdsup/include/gd/unistd.h
 *  \brief  Expose unistd.h
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef LIBGDSUP_INCLUDE_GD_UNISTD_H_INCLUDED
#define LIBGDSUP_INCLUDE_GD_UNISTD_H_INCLUDED

#include "gd/bits/defines.h"
#include "gd/bits/types/gid_t.h"
#include "gd/bits/types/pid_t.h"
#include "gd/bits/types/uid_t.h"

__EXTERN_C_BEGIN
/** \brief Pointer to argument - used by \fn getopt.  */
extern char* optarg;  // NOLINT

/** \brief Set to 0 to turn error reporting by \fn getopt off.  */
extern int opterr;  // NOLINT

/** \brief Index of current parameter for \fn getopt.  */
extern int optind;  // NOLINT

/** \brief Invalid option character reported by \fn getopt.  */
extern int optopt;  // NOLINT

__EXTERN_C_END

/** \brief           Process options
 *  \param argc      Argument cont
 *  \param argv      Argument vector
 *  \param optstring Option string
 *  \return          Matched option, or -1 for complete.
 */
// NOLINTNEXTLINE
__EXTERN_C int getopt(int argc, char* const argv[], const char* optstring) __NOEXCEPT;

/** \brief Reset the getopt state to enable multiple passes of getopt.
 *
 * Resets optarg, opterr, optind, and optopt to their default values.
 */
// NOLINTNEXTLINE
__EXTERN_C void __gd_getopt_reset(void) __NOEXCEPT;

#if defined(_WIN32)
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
__EXTERN_C int close(int fd);

#  define _PC_NAME_MAX 4

/** \brief  Get configurable pathname variables.
 *  \param  _path Path name
 *  \param  _name _PC_* value to query.
 *  \return       -1 on error, otherwise value.
 *
 * Supported _PC_* values:
 *
 * _PC_NAME_MAX: Maximum number of bytes in a filename.
 */
__EXTERN_C long pathconf(char const* _path, int _name);

/** \brief         Read from a file
 *  \param  fd     file descriptor
 *  \param  buf    Buffer to write to
 *  \param  nbytes Number of bytes to read into buffer
 *  \return        Number of bytes read, or -1 on error. \c errno will be updated.
 *
 * Windows implementation won't write more than 2^32 bytes at once.
 */
__EXTERN_C ssize_t read(int fd, void* buf, size_t nbytes);

/** \brief       Remove a directory entry.
 *  \param  path Path to remove.
 *  \return      0 on success, -1 on failure - updating \c errno.
 */
__EXTERN_C int unlink(char const* path);

/** \brief         Write to a file
 *  \param  int    file descriptor
 *  \param  buf    Buffer to read from
 *  \param  nbytes Number of bytes to write into file
 *  \return        Number of bytes written, or -1 on error. \c errno will be updated.
 */
__EXTERN_C ssize_t write(int fd, const void* buf, size_t nbytes);

#else  // ^ Win32 v POSIX
#  include <unistd.h>
#endif  // Win32 or POSIX

#endif  // LIBGDSUP_INCLUDE_GD_UNISTD_H_INCLUDED
