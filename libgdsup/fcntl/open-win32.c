/** \file   libgdsup/unistd/write-win32.h
 *  \brief  Win32 API implementation of write()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/fcntl.h"
#include "gd/sys/stat.h"

#define _CRT_DECLARE_NONSTDC_NAMES 0  // NOLINT
#include <errno.h>
#include <io.h>
#include <share.h>
#include <stdarg.h>

mode_t __umask_getter_setter(mode_t mask, mode_t set);  // NOLINT

int open(char const* path, int oflags, ...)
{
  mode_t mode = 0;
  int pmode = 0;
  va_list ap;

  va_start(ap, oflags);
  if (oflags & O_CREAT) {
    mode = va_arg(ap, int);
    mode &= ~__umask_getter_setter(~((mode_t)0), 0);
    if ((mode & (S_IRUSR | S_IRGRP | S_IROTH)) != 0) {
      pmode |= _S_IREAD;
    }
    if ((mode & (S_IWUSR | S_IWGRP | S_IWOTH)) != 0) {
      pmode |= _S_IWRITE;
    }
    if ((mode & (S_IXUSR | S_IXGRP | S_IXOTH)) != 0) {
      pmode |= _S_IEXEC;
    }
  }
  va_end(ap);

  int fd;
  errno_t err = _sopen_s(&fd, path, oflags | _O_BINARY, _SH_DENYNO, pmode);
  if (err != 0) {
    errno = err;
    return -1;
  }
  return fd;
}
