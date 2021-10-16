/** \file   libgdsup/sys/umask-win32.h
 *  \brief  Win32 API implementation of umask()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/sys/stat.h"

mode_t __umask_getter_setter(mode_t mask, mode_t set)  // NOLINT
{
  static mode_t umask_value = 0;
  mode_t omask = umask_value;
  umask_value = (umask_value & mask) | set;
  return omask;
}

mode_t umask(mode_t _cmask) { return __umask_getter_setter(0, _cmask); }
