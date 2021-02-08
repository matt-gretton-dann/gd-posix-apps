/** \file   libgdsup/unistd/__unistd_read.c
 *  \brief  Implemenation of __unistd_read()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "gd/limits.h"
#include "gd/unistd.h"

#include <errno.h>

#include "unistd/unistd.h"

ssize_t __unistd_read(int fd, void* buf, size_t nbyte)
{
  ssize_t r = 0;
  ssize_t snbyte = SSIZE_MAX;
  if (nbyte < SSIZE_MAX) {
    snbyte = (ssize_t)nbyte;
  }

  while (r < snbyte) {
    ssize_t r2 = read(fd, ((char*)buf) + r, snbyte - r);
    if (r2 == 0) {
      return r;
    }

    if (r2 == -1) {
      if (errno != EINTR) {
        return -1;
      }
    }
    else {
      r += r2;
    }
  }

  return r;
}
