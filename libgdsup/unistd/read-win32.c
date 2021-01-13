/** \file   libgdsup/unistd/read-win32.h
 *  \brief  Win32 API implementation of read()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/unistd.h"

#include <io.h>

ssize_t read(int fd, void* buf, size_t nbytes)
{
  /* Win32 _read returns int not ssize_t, so we clamp to INT_MAX.  */
  if (nbytes > INT_MAX) {
    nbytes = INT_MAX;
  }
  return _read(fd, buf, nbytes);
}
