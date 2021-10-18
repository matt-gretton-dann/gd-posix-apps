/** \file   libgdsup/sys_stat/chmod-win32.h
 *  \brief  Win32 API implementation of chmod()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/sys/stat.h"

#include <io.h>

int chmod(char const* _path, int _mode)
{
  int pmode = 0;

  /* Translate POSIX modes to Win32 ones.  */
  if ((_mode & (S_IRUSR | S_IRGRP | S_IROTH)) != 0) {
    pmode |= _S_IREAD;
  }
  if ((_mode & (S_IWUSR | S_IWGRP | S_IWOTH)) != 0) {
    pmode |= _S_IWRITE;
  }
  if ((_mode & (S_IXUSR | S_IXGRP | S_IXOTH)) != 0) {
    pmode |= _S_IEXEC;
  }

  return _chmod(_path, pmode);
}
