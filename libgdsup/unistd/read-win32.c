/** \file   libgdsup/unistd/read-win32.h
 *  \brief  Win32 API implementation of read()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/limits.h"
#include "gd/unistd.h"

#define _CRT_DECLARE_NONSTDC_NAMES 0  // NOLINT
#include <io.h>

ssize_t read(int fd, void* buf, size_t nbytes)
{
  /* Win32 _read returns int not ssize_t, so we clamp to INT_MAX.  */
  unsigned int amt = INT_MAX;
  if (nbytes < INT_MAX) {
    amt = (unsigned int)nbytes;
  }
  return _read(fd, buf, amt);
}
