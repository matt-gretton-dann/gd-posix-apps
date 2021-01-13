/** \file   libgdsup/unistd/close-win32.h
 *  \brief  Win32 API implementation of close()
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#include "gd/unistd.h"

#include <io.h>

/* Just call underlying _close(). */
int close(int fd) { return _close(fd); }
