/** \file   libgdsup/stdlib/mkstemp-win32.c
 *  \brief  Win32 API implementation of mkstemp()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/bits/defines.h"
#include "gd/stdlib.h"

#include <errno.h>
#include <fcntl.h>
#include <io.h>
#include <share.h>
#include <stddef.h>
#include <string.h>
#include <sys/stat.h>

int mkstemp(char* temp)
{
  /* Find length of template.  */
  if (temp == NULL) {
    errno = ENOENT;
    return -1;
  }
  size_t len = strlen(temp);

  /* Repeat until we manage to open a file, or have an error that isn't "file exists". */
  while (1) {
    errno_t err = _mktemp_s(temp, len + 1);
    if (err != 0) {
      errno = err;
      return -1;
    }

    int fd;
    err = _sopen_s(&fd, temp, _O_RDWR | _O_CREAT | _O_EXCL, _SH_DENYNO, _S_IREAD | _S_IWRITE);
    if (fd != -1 || err != EEXIST) {
      if (fd == -1) {
        errno = err;
      }
      return fd;
    }
  }
}
