/** \file   libgdsup/unistd/write-win32.h
 *  \brief  Win32 API implementation of write()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/fcntl.h"

#define _CRT_DECLARE_NONSTDC_NAMES 0
#include <errno.h>
#include <io.h>
#include <share.h>
#include <stdarg.h>

int open(char const* path, int oflags, ...)
{
  va_list ap;
  va_start(ap, oflags);
  int mode = 0;
  if (oflags & O_CREAT) {
    mode = va_arg(ap, int);
  }

  int fd;
  errno_t err = _sopen_s(&fd, path, oflags | _O_BINARY, _SH_DENYNO, mode);
  if (err != 0) {
    errno = err;
    return -1;
  }
  return fd;
}
