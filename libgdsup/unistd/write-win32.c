/** \file   libgdsup/unistd/write-win32.h
 *  \brief  Win32 API implementation of write()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/limits.h"
#include "gd/unistd.h"

#define _CRT_DECLARE_NONSTDC_NAMES 0
#include <io.h>

ssize_t write(int fd, void const* buf, size_t nbytes)
{
  /* Win32 _write returns int not ssize_t, so we clamp to INT_MAX.  */
  if (nbytes > INT_MAX) {
    nbytes = INT_MAX;
  }
  return _write(fd, buf, nbytes);
}
