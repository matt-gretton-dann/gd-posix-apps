/** \file   libgdsup/include/gd/unistd.h
 *  \brief  Expose unistd.h
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_GD_UNISTD_H_INCLUDED
#define _LIBGDSUP_INCLUDE_GD_UNISTD_H_INCLUDED

#if defined(_WIN32)
// Basic start use Windows' io.h header.
#  define _CRT_NONSTDC_NO_WARNINGS
#  include <io.h>
#else  // ^ Win32 v POSIX
#  include <unistd.h>
#endif  // Win32 or POSIX

#endif  // _LIBGDSUP_INCLUDE_GD_UNISTD_H_INCLUDED
