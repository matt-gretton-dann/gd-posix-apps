/** \file   libgdsup/unistd/isatty-win32.h
 *  \brief  Win32 API implementation of isatty()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/unistd.h"

#define _CRT_DECLARE_NONSTDC_NAMES 0
#include <io.h>

int isatty(int __fildes) { return _isatty(__fildes); }
