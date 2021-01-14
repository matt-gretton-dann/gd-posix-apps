/** \file   libgdsup/include/gd/sys/stat.h
 *  \brief  Expose stat.h
 *  \author Copyright 2021, Matthew Gretton-Dann
 *  SPDX-License-Identifier: Apache-2.0
 */

#ifndef _LIBGDSUP_INCLUDE_GD_SYS_STAT_H_INCLUDED
#define _LIBGDSUP_INCLUDE_GD_SYS_STAT_H_INCLUDED

#if _WIN32
#  define _CRT_DECLARE_NONSTDC_NAMES 0
#endif

#include <sys/stat.h>

#if _WIN32

#  define S_IRUSR _S_IREAD
#  define S_IRGRP _S_IREAD
#  define S_IROTH _S_IREAD
#  define S_IWUSR _S_IWRITE
#  define S_IWGRP _S_IWRITE
#  define S_IWOTH _S_IWRITE
#  define S_IXUSR _S_IEXEC
#  define S_IXGRP _S_IEXEC
#  define S_IXOTH _S_IEXEC

#endif

#endif  // _LIBGDSUP_INCLUDE_GD_SYS_STAT_H_INCLUDED
