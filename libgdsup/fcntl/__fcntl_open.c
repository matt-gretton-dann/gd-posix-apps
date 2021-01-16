/** \file   libgdsup/fcntl/__fcntl_open.c
 *  \brief  Implemenation of __fcntl_open()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *          SPDX-License-Identifier: Apache-2.0
 */

#include "fcntl/fcntl.h"
#include "gd/fcntl.h"

#include <assert.h>
#include <errno.h>
#include <stdbool.h>

int __fcntl_open(char const* path, int oflags)
{
  assert((oflags & O_CREAT) == 0 && "Do not specify O_CREAT");
  int saved_errno = errno;
  while (true) {
    int fd = open(path, oflags);
    if (fd != -1) {
      errno = saved_errno;
      return fd;
    }

    if (errno != EINTR) {
      return fd;
    }
  }
}
