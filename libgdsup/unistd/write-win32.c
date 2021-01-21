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
  unsigned int amt = INT_MAX;
  if (nbytes < INT_MAX) {
    amt = (unsigned int)nbytes;
  }
  return _write(fd, buf, amt);
}
