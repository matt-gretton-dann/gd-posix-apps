/** \file   libgdsup/unistd/unlink-win32.h
 *  \brief  Win32 API implementation of unlink()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/unistd.h"

#define _CRT_DECLARE_NONSTDC_NAMES 0  // NOLINT
#include <io.h>

/* Just call underlying _close(). */
int unlink(char const* path) { return _unlink(path); }
