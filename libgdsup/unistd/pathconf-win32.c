/** \file   libgdsup/unistd/pathconf-win32.h
 *  \brief  Win32 API implementation of path()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/limits.h"
#include "gd/stdlib.h"
#include "gd/unistd.h"

#include <Windows.h>
#include <errno.h>
#include <fileapi.h>
#include <limits.h>
#include <tchar.h>

static long pathconf_name_max(char const* path)
{
  int nlen = MultiByteToWideChar(CP_UTF8, 0, path, -1, NULL, 0);
  if (nlen == 0) {
    errno = EINVAL;
    return -1;
  }

  wchar_t* wpath = (wchar_t*)malloc(nlen * sizeof(wchar_t));
  if (wpath == NULL) {
    errno = ENOMEM;
    return -1;
  }

  nlen = MultiByteToWideChar(CP_UTF8, 0, path, -1, wpath, nlen);
  if (nlen == 0) {
    free(wpath);
    errno = EINVAL;
    return -1;
  }

  wchar_t wroot_path[MAX_PATH + 1];
  BOOL success = GetVolumePathNameW(wpath, wroot_path, MAX_PATH + 1);
  if (!success) {
    free(wpath);
    errno = EINVAL;
    return -1;
  }

  DWORD name_max = 0;
  DWORD flags = 0;
  success = GetVolumeInformationW(wroot_path, NULL, 0, NULL, &name_max, &flags, NULL, 0);
  free(wpath);
  if (!success) {
    errno = EINVAL;
    return -1;
  }

  return (long)(name_max);
}

long pathconf(char const* _path, int _name)
{
  switch (_name) {
  case _PC_NAME_MAX:
    return pathconf_name_max(_path);
  default:
    errno = EINVAL;
    return -1;
  }
}
